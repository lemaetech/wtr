(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

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

type 'a decoder =
  { name: string (* name e.g. int, float, bool, string etc *)
  ; decode: string -> 'a option
  ; id: 'a id }

let decoder ~name ~decode =
  let id = new_id () in
  {name; decode; id}

let int = decoder ~name:"int" ~decode:int_of_string_opt
let int32 = decoder ~name:"int32" ~decode:Int32.of_string_opt
let int64 = decoder ~name:"int64" ~decode:Int64.of_string_opt
let float = decoder ~name:"float" ~decode:float_of_string_opt
let string = decoder ~name:"string" ~decode:(fun a -> Some a)
let bool = decoder ~name:"bool" ~decode:bool_of_string_opt

type method' =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]

let method_equal (meth1 : method') (meth2 : method') = compare meth1 meth2 = 0

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
  | header -> `Method header

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

type ('a, 'b) uri =
  | Nil : ('b, 'b) uri
  | Splat : (string -> 'b, 'b) uri
  | Trailing_slash : ('b, 'b) uri
  | Literal : string * ('a, 'b) uri -> ('a, 'b) uri
  | Decode : 'c decoder * ('a, 'b) uri -> ('c -> 'a, 'b) uri

let rec pp_uri : type a b. Format.formatter -> (a, b) uri -> unit =
 fun fmt -> function
  | Nil -> Format.fprintf fmt "%!"
  | Splat -> Format.fprintf fmt "/**%!"
  | Trailing_slash -> Format.fprintf fmt "/%!"
  | Literal (lit, uri) -> Format.fprintf fmt "/%s%a" lit pp_uri uri
  | Decode (decoder, uri) -> Format.fprintf fmt "/:%s%a" decoder.name pp_uri uri

type 'c route = Route : method' * ('a, 'c) uri * 'a -> 'c route

let route : method' -> ('a, 'b) uri -> 'a -> 'b route =
 fun method' uri f -> Route (method', uri, f)

let routes methods uri f = List.map (fun method' -> route method' uri f) methods

let pp_route : Format.formatter -> 'b route -> unit =
 fun fmt (Route (method', uri, _)) ->
  Format.fprintf fmt "%a%a" pp_method method' pp_uri uri

(** Existential to encode uri component/node type. *)
type node_type =
  | NTrailing_slash : node_type
  | NSplat : node_type
  | NLiteral : string -> node_type
  | NMethod : method' -> node_type
  | NDecoder : 'c decoder -> node_type

let node_type_equal a b =
  match (a, b) with
  | NTrailing_slash, NTrailing_slash -> true
  | NSplat, NSplat -> true
  | NLiteral lit1, NLiteral lit2 -> String.equal lit2 lit1
  | NMethod meth1, NMethod meth2 -> method_equal meth1 meth2
  | NDecoder decoder, NDecoder decoder' -> (
    match eq decoder'.id decoder.id with Some Eq -> true | None -> false )
  | _ -> false

(* [uri'_of_uri uri] converts [uri] to [node_type list]. This is done to get around OCaml
   type inference issue when using [uri] type in the [node] function below. *)
let rec node_type_of_uri : type a b. (a, b) uri -> node_type list = function
  | Nil -> []
  | Trailing_slash -> [NTrailing_slash]
  | Splat -> [NSplat]
  | Literal (lit, uri) -> NLiteral lit :: node_type_of_uri uri
  | Decode (decoder, uri) -> NDecoder decoder :: node_type_of_uri uri

let node_type_to_string node_type =
  match node_type with
  | NSplat -> Format.sprintf "/**%!"
  | NTrailing_slash -> Format.sprintf "/%!"
  | NLiteral lit -> Format.sprintf "/%s" lit
  | NDecoder decoder -> Format.sprintf "/:%s" decoder.name
  | NMethod method' -> Format.asprintf "%a" pp_method method'

(** ['a t] is a node in a trie based router. *)
type 'a node = {route: 'a route option; node_types: (node_type * 'a node) list}

let rec node : 'a node -> 'a route -> 'a node =
 fun node' (Route (method', uri, _) as route) ->
  let rec loop node node_types =
    match node_types with
    | [] -> {node with route= Some route}
    | node_type :: node_types ->
        let node'' =
          List.find_opt
            (fun (node_type', _) -> node_type_equal node_type node_type')
            node.node_types
        in
        { node with
          node_types=
            ( match node'' with
            | Some _ ->
                List.map
                  (fun (node_type', t') ->
                    if node_type_equal node_type node_type' then
                      (node_type', loop t' node_types)
                    else (node_type', t') )
                  node.node_types
            | None -> (node_type, loop empty_node node_types) :: node.node_types
            ) }
  in
  let node_types = NMethod method' :: node_type_of_uri uri in
  loop node' node_types

and empty_node : 'a node = {route= None; node_types= []}

(* We use array for node_types so that we get better cache locality. The hope being
   that iterating nodes via array is faster than via the list. *)
type 'a t = {route: 'a route option; node_types: (node_type * 'a t) array}

let rec t routes =
  List.concat routes |> List.fold_left node empty_node |> compile

and compile : 'a node -> 'a t =
 fun t ->
  { route= t.route
  ; node_types=
      List.rev t.node_types
      |> List.map (fun (node_type, t) -> (node_type, compile t))
      |> Array.of_list }

let rec pp fmt t =
  let nodes = t.node_types |> Array.to_list in
  let len = List.length nodes in
  nodes
  |> Format.pp_print_list
       ~pp_sep:(if len > 1 then Format.pp_force_newline else fun _ () -> ())
       (fun fmt (nt, t') ->
         Format.pp_open_vbox fmt 2 ;
         Format.pp_print_string fmt (node_type_to_string nt) ;
         if Array.length t'.node_types > 0 then (
           Format.pp_print_break fmt 0 0 ;
           pp fmt t' ) ;
         Format.pp_close_box fmt () ;
         () )
       fmt

type decoded_value = D : 'c decoder * 'c -> decoded_value

let rec match' method' uri (t : 'a t) =
  (* split uri path and query into tokens *)
  let uri' = uri |> String.trim |> Uri.of_string in
  let path_tokens = Uri.path uri' |> String.split_on_char '/' |> List.tl in
  let query_tokens =
    Uri.query uri'
    |> List.map (fun (k, v) ->
           if List.length v > 0 then [k; List.hd v] else [k] )
    |> List.concat
  in
  let uri_toks = path_tokens @ query_tokens in
  (* Matching algorithm overview:

     1. First match the HTTP method as all routes always start with a HTTP method
     2. Then follow the trie nodes as suggested by the trie algorithm.
  *)
  let rec try_match t decoded_values uri_toks matched_token_count =
    match uri_toks with
    | [] ->
        Option.map
          (fun (Route (_, uri, f)) ->
            exec_route_handler f (uri, List.rev decoded_values) )
          t.route
    | uri_part :: uris ->
        let continue = ref true in
        let index = ref 0 in
        let matched_node = ref None in
        let full_splat_matched = ref false in
        while !continue && !index < Array.length t.node_types do
          match t.node_types.(!index) with
          | NDecoder decoder, t' -> (
            match decoder.decode uri_part with
            | Some v ->
                matched_node := Some (t', D (decoder, v) :: decoded_values) ;
                continue := false
            | None -> incr index )
          | NLiteral lit, t' when String.equal lit uri_part ->
              matched_node := Some (t', decoded_values) ;
              continue := false
          | NTrailing_slash, t' when String.equal "" uri_part ->
              matched_node := Some (t', decoded_values) ;
              continue := false
          | NSplat, t' ->
              let path =
                drop path_tokens matched_token_count |> String.concat "/"
              in
              let splat_url =
                String.split_on_char '?' uri
                |> fun l ->
                if List.length l > 1 then path ^ "?" ^ List.nth l 1 else path
              in
              matched_node := Some (t', D (string, splat_url) :: decoded_values) ;
              continue := false ;
              full_splat_matched := true
          | _ -> incr index
        done ;
        Option.bind !matched_node (fun (t', decoded_values) ->
            let matched_tok_count = matched_token_count + 1 in
            if !full_splat_matched then
              (try_match [@tailcall]) t' decoded_values [] matched_tok_count
            else
              (try_match [@tailcall]) t' decoded_values uris matched_tok_count )
  in
  if List.length uri_toks > 0 then
    let n = Array.length t.node_types in
    let rec loop i =
      if i = n then None
      else
        match t.node_types.(i) with
        | NMethod method'', t' when method_equal method' method'' ->
            try_match t' [] uri_toks 0
        | _ -> (loop [@tailcall]) (i + 1)
    in
    loop 0
  else None

and drop l n = match l with _ :: tl when n > 0 -> drop tl (n - 1) | t -> t

and exec_route_handler : type a b. a -> (a, b) uri * decoded_value list -> b =
 fun f -> function
  | Nil, [] -> f
  | Splat, [D (d, v)] -> (
    match eq string.id d.id with Some Eq -> f v | None -> assert false )
  | Trailing_slash, [] -> f
  | Literal (_, uri), decoded_values ->
      exec_route_handler f (uri, decoded_values)
  | Decode ({id; _}, uri), D ({id= id'; _}, v) :: decoded_values -> (
    match eq id id' with
    | Some Eq -> exec_route_handler (f v) (uri, decoded_values)
    | None -> assert false )
  | _, _ -> assert false

module Private = struct
  let nil = Nil
  let splat = Splat
  let t_slash = Trailing_slash
  let lit s uri = Literal (s, uri)
  let decode d uri = Decode (d, uri)
  let int = int
  let int32 = int32
  let int64 = int64
  let float = float
  let string = string
  let bool = bool
end
