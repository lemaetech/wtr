(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *-------------------------------------------------------------------------*)

(* Arg id type. *)
type 'a witness = ..
type (_, _) eq = Eq : ('a, 'a) eq

module type Ty = sig
  type t

  val witness : t witness
  val eq : 'a witness -> ('a, t) eq option
end

type 'a id = (module Ty with type t = 'a)

let new_id (type a) () =
  let module Ty = struct
    type t = a
    type 'a witness += Ty : t witness

    let witness = Ty

    let eq (type b) : b witness -> (b, t) eq option = function
      | Ty -> Some Eq
      | _ -> None
  end in
  (module Ty : Ty with type t = a)

let eq : type a b. a id -> b id -> (a, b) eq option =
 fun (module TyA) (module TyB) -> TyB.eq TyA.witness

(* Types *)

(* We use array for node_types so that we get better cache locality. The hope being
   that iterating nodes via array is faster than via the list. *)
type 'a router =
  {route: 'a route option; node_types: (node_type * 'a router) array}

(* Unoptimized/un-compiled router type. *)
and 'a node = {route': 'a route option; node_types': (node_type * 'a node) list}

and ('a, 'b) uri =
  | Nil : ('b, 'b) uri
  | Splat : (string -> 'b, 'b) uri
  | Slash : ('b, 'b) uri
  | Exact : string * ('a, 'b) uri -> ('a, 'b) uri
  | Query_exact : string * string * ('a, 'b) uri -> ('a, 'b) uri
  | Arg : 'c arg * ('a, 'b) uri -> ('c -> 'a, 'b) uri
  | Query_arg : string * 'c arg * ('a, 'b) uri -> ('c -> 'a, 'b) uri

(** Existential to encode uri component/node type. *)
and node_type =
  | NSlash : node_type
  | NSplat : node_type
  | NExact : string -> node_type
  | NQuery_exact : string * string -> node_type
  | NMethod : method' -> node_type
  | NArg : 'c arg -> node_type
  | NQuery_arg : string * 'c arg -> node_type

and 'c route = Route : method' * ('a, 'c) uri * 'a -> 'c route

and method' =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]

and ('a, 'b) path = ('a, 'b) uri

and ('a, 'b) query = ('a, 'b) uri

and 'a arg =
  { name: string (* name e.g. int, float, bool, string etc *)
  ; convert: string -> 'a option
  ; id: 'a id }

and arg_value = D : 'c arg * 'c -> arg_value

(* HTTP Method *)

let method_equal (meth1 : method') (meth2 : method') =
  match (meth1, meth2) with
  | `Method m1, `Method m2 ->
      String.(equal (uppercase_ascii m1) (uppercase_ascii m2))
  | meth1, meth2 -> compare meth1 meth2 = 0

let method' meth =
  match String.uppercase_ascii meth with
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "DELETE" -> `DELETE
  | "CONNECT" -> `CONNECT
  | "OPTIONS" -> `OPTIONS
  | "TRACE" -> `TRACE
  | _ -> `Method meth

(* Arg *)

let arg name convert =
  let id = new_id () in
  {name; convert; id}

let int_d = arg "int" int_of_string_opt
let int32_d = arg "int32" Int32.of_string_opt
let int64_d = arg "int64" Int64.of_string_opt
let float_d = arg "float" float_of_string_opt
let string_d = arg "string" (fun a -> Some a)
let bool_d = arg "bool" bool_of_string_opt

(* Path *)

let int u = Arg (int_d, u)
let int32 u = Arg (int32_d, u)
let int64 u = Arg (int64_d, u)
let float u = Arg (float_d, u)
let bool u = Arg (bool_d, u)
let string u = Arg (string_d, u)
let parg d u = Arg (d, u)
let exact s uri = Exact (s, uri)
let pend = Nil
let splat = Splat
let slash = Slash
let ( / ) f1 f2 r = f1 @@ f2 r
let ( /. ) f e = f e

external to_uri : ('a, 'b) path -> ('a, 'b) uri = "%identity"

(* Query *)

let ( /& ) f1 f2 r = f1 @@ f2 r
let qint field u = Query_arg (field, int_d, u)
let qint32 field u = Query_arg (field, int32_d, u)
let qint64 field u = Query_arg (field, int64_d, u)
let qfloat field u = Query_arg (field, float_d, u)
let qbool field u = Query_arg (field, bool_d, u)
let qstring field u = Query_arg (field, string_d, u)
let qarg (field, d) u = Query_arg (field, d, u)
let qexact (field, exact) uri = Query_exact (field, exact, uri)

(* URI Combinators *)

let root = Slash
let ( /? ) f1 f2 r = f1 (f2 r)
let ( /?. ) qf () = qf Nil

(* Route and Router *)

let route : ?method':method' -> ('a, 'b) uri -> 'a -> 'b route =
 fun ?(method' = `GET) uri f -> Route (method', uri, f)

let routes methods uri f =
  List.map (fun method' -> route ~method' uri f) methods

let node_type_equal a b =
  match (a, b) with
  | NSlash, NSlash -> true
  | NSplat, NSplat -> true
  | NExact exact1, NExact exact2 -> String.equal exact2 exact1
  | NQuery_exact (name1, value1), NQuery_exact (name2, value2) ->
      String.equal name1 name2 && String.equal value1 value2
  | NMethod meth1, NMethod meth2 -> method_equal meth1 meth2
  | NArg arg, NArg arg' -> (
    match eq arg'.id arg.id with Some Eq -> true | None -> false )
  | NQuery_arg (name1, arg1), NQuery_arg (name2, arg2) -> (
      String.equal name1 name2
      && match eq arg1.id arg2.id with Some Eq -> true | None -> false )
  | _ -> false

let rec node_type_of_uri : type a b. (a, b) uri -> node_type list = function
  | Nil -> []
  | Slash -> [NSlash]
  | Splat -> [NSplat]
  | Exact (exact1, uri) -> NExact exact1 :: node_type_of_uri uri
  | Query_exact (name, value, uri) ->
      NQuery_exact (name, value) :: node_type_of_uri uri
  | Arg (arg, uri) -> NArg arg :: node_type_of_uri uri
  | Query_arg (name, arg, uri) -> NQuery_arg (name, arg) :: node_type_of_uri uri

let rec node : 'a node -> 'a route -> 'a node =
 fun node' (Route (method', uri, _) as route) ->
  let rec loop node node_types =
    match node_types with
    | [] -> {node with route'= Some route}
    | node_type :: node_types ->
        let node'' =
          List.find_opt
            (fun (node_type', _) -> node_type_equal node_type node_type')
            node.node_types'
        in
        { node with
          node_types'=
            ( match node'' with
            | Some _ ->
                List.map
                  (fun (node_type', t') ->
                    if node_type_equal node_type node_type' then
                      (node_type', loop t' node_types)
                    else (node_type', t') )
                  node.node_types'
            | None ->
                (node_type, loop empty_node node_types) :: node.node_types' ) }
  in
  let node_types = NMethod method' :: node_type_of_uri uri in
  loop node' node_types

and empty_node : 'a node = {route'= None; node_types'= []}

let rec compile : 'a node -> 'a router =
 fun t ->
  { route= t.route'
  ; node_types=
      List.rev t.node_types'
      |> List.map (fun (node_type, t) -> (node_type, compile t))
      |> Array.of_list }

let router routes = compile @@ List.fold_left node empty_node routes

let router' routes =
  compile @@ (List.concat routes |> List.fold_left node empty_node)

let rec drop : 'a list -> int -> 'a list =
 fun l n -> match l with _ :: tl when n > 0 -> drop tl (n - 1) | t -> t

let rec match' : method' -> string -> 'a router -> 'a option =
 fun method' uri t ->
  (* split uri path and query into tokens *)
  let uri' = uri |> String.trim |> Uri.of_string in
  let path_tokens =
    Uri.path uri'
    |> String.split_on_char '/'
    |> List.tl
    |> List.map (fun tok -> `Path tok)
  in
  let query_tokens =
    Uri.query uri'
    |> List.map (fun (k, values) -> List.map (fun v' -> `Query (k, v')) values)
    |> List.concat
  in
  let uri_tokens = path_tokens @ query_tokens in
  (* Matching algorithm overview:

     1. First match the HTTP method as all routes always start with a HTTP method
     2. Then follow the trie nodes as suggested by the trie algorithm.
  *)
  let rec try_match t arg_values uri_tokens matched_token_count =
    match uri_tokens with
    | [] ->
        Option.map
          (fun (Route (_, uri, f)) ->
            exec_route_handler f (uri, List.rev arg_values) )
          t.route
    | uri_token :: uri_tokens ->
        let continue = ref true in
        let index = ref 0 in
        let matched_node = ref None in
        let full_splat_matched = ref false in
        while !continue && !index < Array.length t.node_types do
          match (uri_token, t.node_types.(!index)) with
          | `Path v, (NArg arg, t') -> (
            match arg.convert v with
            | Some v ->
                matched_node := Some (t', D (arg, v) :: arg_values) ;
                continue := false
            | None -> incr index )
          | `Path v, (NExact exact, t') when String.equal exact v ->
              matched_node := Some (t', arg_values) ;
              continue := false
          | `Path v, (NSlash, t') when String.equal "" v ->
              matched_node := Some (t', arg_values) ;
              continue := false
          | `Path _, (NSplat, t') ->
              let path =
                drop path_tokens matched_token_count
                |> List.map (fun (`Path tok) -> tok)
                |> String.concat "/"
              in
              let splat_url =
                String.split_on_char '?' uri
                |> fun l ->
                if List.length l > 1 then path ^ "?" ^ List.nth l 1 else path
              in
              matched_node := Some (t', D (string_d, splat_url) :: arg_values) ;
              continue := false ;
              full_splat_matched := true
          | `Query (name, value), (NQuery_arg (name', arg), t') -> (
            match arg.convert value with
            | Some v when String.equal name name' ->
                matched_node := Some (t', D (arg, v) :: arg_values) ;
                continue := false
            | _ -> incr index )
          | `Query (name1, value1), (NQuery_exact (name2, value2), t')
            when String.equal name1 name2 && String.equal value1 value2 ->
              matched_node := Some (t', arg_values) ;
              continue := false
          | _ -> incr index
        done ;
        Option.bind !matched_node (fun (t', arg_values) ->
            let matched_tok_count = matched_token_count + 1 in
            if !full_splat_matched then
              (try_match [@tailcall]) t' arg_values [] matched_tok_count
            else
              (try_match [@tailcall]) t' arg_values uri_tokens matched_tok_count )
  in
  if List.length uri_tokens > 0 then
    let n = Array.length t.node_types in
    let rec loop i =
      if i = n then None
      else
        match t.node_types.(i) with
        | NMethod method'', t' when method_equal method' method'' ->
            try_match t' [] uri_tokens 0
        | _ -> (loop [@tailcall]) (i + 1)
    in
    loop 0
  else None

and exec_route_handler : type a b. a -> (a, b) uri * arg_value list -> b =
 fun f -> function
  | Nil, [] -> f
  | Splat, [D (d, v)] -> (
    match eq string_d.id d.id with Some Eq -> f v | None -> assert false )
  | Slash, [] -> f
  | Exact (_, uri), arg_values -> exec_route_handler f (uri, arg_values)
  | Query_exact (_, _, uri), arg_values -> exec_route_handler f (uri, arg_values)
  | Arg ({id; _}, uri), D ({id= id'; _}, v) :: arg_values -> (
    match eq id id' with
    | Some Eq -> exec_route_handler (f v) (uri, arg_values)
    | None -> assert false )
  | Query_arg (_, {id; _}, uri), D ({id= id'; _}, v) :: arg_values -> (
    match eq id id' with
    | Some Eq -> exec_route_handler (f v) (uri, arg_values)
    | None -> assert false )
  | _, _ -> assert false

(* Pretty Printers *)

let rec pp_uri : type a b. Format.formatter -> (a, b) uri -> unit =
 fun fmt uri ->
  let query_start_tok_emitted = ref false in
  let pp_query_tok fmt pp' uri =
    if not !query_start_tok_emitted then (
      query_start_tok_emitted := true ;
      Format.fprintf fmt "?%a" pp' uri )
    else Format.fprintf fmt "&%a" pp' uri
  in
  match uri with
  | Nil -> Format.fprintf fmt "%!"
  | Splat -> Format.fprintf fmt "/**%!"
  | Slash -> Format.fprintf fmt "/%!"
  | Exact (exact, uri) -> Format.fprintf fmt "/%s%a" exact pp_uri uri
  | Query_exact (name, value, uri) ->
      let pp' fmt uri = Format.fprintf fmt "%s=%s%a" name value pp_uri uri in
      pp_query_tok fmt pp' uri
  | Arg (arg, uri) -> Format.fprintf fmt "/:%s%a" arg.name pp_uri uri
  | Query_arg (name, arg, uri) ->
      let pp' fmt uri =
        Format.fprintf fmt "%s=:%s%a" name arg.name pp_uri uri
      in
      pp_query_tok fmt pp' uri

let pp_method fmt t =
  ( match t with
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `CONNECT -> "CONNECT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `Method s -> Format.sprintf "Method (%s)" s )
  |> Format.fprintf fmt "%s"

let pp_node_type fmt node_type =
  match node_type with
  | NSplat -> Format.fprintf fmt "/**"
  | NSlash -> Format.fprintf fmt "/"
  | NExact exact -> Format.fprintf fmt "/%s" exact
  | NQuery_exact (name, value) -> Format.fprintf fmt "%s=%s" name value
  | NArg arg -> Format.fprintf fmt "/:%s" arg.name
  | NQuery_arg (name, arg) -> Format.fprintf fmt "%s=:%s" name arg.name
  | NMethod method' -> Format.fprintf fmt "%a" pp_method method'

let pp_route : Format.formatter -> 'b route -> unit =
 fun fmt (Route (method', uri, _)) ->
  Format.fprintf fmt "%a%a" pp_method method' pp_uri uri

let rec pp fmt t =
  let nodes = t.node_types |> Array.to_list in
  let len = List.length nodes in
  let query_tok_printed = ref false in
  Format.pp_print_list
    ~pp_sep:(if len > 1 then Format.pp_force_newline else fun _ () -> ())
    (fun fmt (node_type, t') ->
      Format.pp_open_vbox fmt 2 ;
      ( match node_type with
      | NQuery_exact _ | NQuery_arg _ ->
          if not !query_tok_printed then
            Format.fprintf fmt "?%a" pp_node_type node_type
          else Format.fprintf fmt "&%a" pp_node_type node_type
      | node -> Format.fprintf fmt "%a" pp_node_type node ) ;
      if Array.length t'.node_types > 0 then (
        Format.pp_print_break fmt 0 0 ;
        pp fmt t' ) ;
      Format.pp_close_box fmt () )
    fmt nodes

module Private = struct
  let nil = Nil
  let splat = Splat
  let slash = Slash
  let exact s uri = Exact (s, uri)
  let query_exact name value uri = Query_exact (name, value, uri)
  let arg d uri = Arg (d, uri)
  let query_arg name d uri = Query_arg (name, d, uri)
  let int = int_d
  let int32 = int32_d
  let int64 = int64_d
  let float = float_d
  let string = string_d
  let bool = bool_d
end
