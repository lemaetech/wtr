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

(* We use an array for node_types so that we get better cache locality. *)
type 'a router = {
  route : 'a route option;
  node_types : (node_type * 'a router) array;
}

(* Unoptimized/un-compiled router type. *)
and 'a node = {
  route' : 'a route option;
  node_types' : (node_type * 'a node) list;
}

and ('a, 'b) request_target =
  | Nil : ('b, 'b) request_target
  | Rest : (rest -> 'b, 'b) request_target
  | Slash : ('b, 'b) request_target
  | Exact : string * ('a, 'b) request_target -> ('a, 'b) request_target
  | Query_exact :
      string * string * ('a, 'b) request_target
      -> ('a, 'b) request_target
  | Arg : 'c arg * ('a, 'b) request_target -> ('c -> 'a, 'b) request_target
  | Query_arg :
      string * 'c arg * ('a, 'b) request_target
      -> ('c -> 'a, 'b) request_target

(** Existential to encode request_target component/node type. *)
and node_type =
  | NSlash : node_type
  | NRest : node_type
  | NExact : string -> node_type
  | NQuery_exact : string * string -> node_type
  | NMethod : method' -> node_type
  | NArg : 'c arg -> node_type
  | NQuery_arg : string * 'c arg -> node_type

and 'c route = Route : method' * ('a, 'c) request_target * 'a -> 'c route

and 'a routes = 'a route list

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

and ('a, 'b) path = ('a, 'b) request_target

and rest = string

and ('a, 'b) query = ('a, 'b) request_target

and 'a arg = {
  name : string;
  (* name e.g. int, float, bool, string etc *)
  convert : string -> 'a option;
  id : 'a id;
}

and arg_value = Arg_value : 'c arg * 'c -> arg_value

(* Arg *)

let arg name convert =
  let id = new_id () in
  { name; convert; id }

let int_d = arg "int" int_of_string_opt
let int32_d = arg "int32" Int32.of_string_opt
let int64_d = arg "int64" Int64.of_string_opt
let float_d = arg "float" float_of_string_opt
let string_d = arg "string" (fun a -> Some a)
let bool_d = arg "bool" bool_of_string_opt

(* Request Target DSL *)

(* General Components *)

let ( / ) f1 f2 r = f1 @@ f2 r
let ( /& ) f1 f2 r = f1 @@ f2 r
let ( /? ) f1 f2 r = f1 (f2 r)
let ( //. ) f e = f e
let ( /. ) f e = f e
let ( /?. ) qf () = qf Nil

external of_path : ('a, 'b) path -> ('a, 'b) request_target = "%identity"

let exact s request_target = Exact (s, request_target)

let qexact (field, exact) request_target =
  Query_exact (field, exact, request_target)

external to_request_target : ('a, 'b) path -> ('a, 'b) request_target
  = "%identity"

let root = Slash

(* Path *)

let int u = Arg (int_d, u)
let int32 u = Arg (int32_d, u)
let int64 u = Arg (int64_d, u)
let float u = Arg (float_d, u)
let bool u = Arg (bool_d, u)
let string u = Arg (string_d, u)
let parg d u = Arg (d, u)

(* Query *)

let qint field u = Query_arg (field, int_d, u)
let qint32 field u = Query_arg (field, int32_d, u)
let qint64 field u = Query_arg (field, int64_d, u)
let qfloat field u = Query_arg (field, float_d, u)
let qbool field u = Query_arg (field, bool_d, u)
let qstring field u = Query_arg (field, string_d, u)
let qarg (field, d) u = Query_arg (field, d, u)

(* Matching Last Components *)

let pend = Nil
let rest = Rest
let slash = Slash

external rest_to_string : rest -> string = "%identity"

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

(* Routes and Router *)

let route : method' -> ('a, 'b) request_target -> 'a -> 'b route =
 fun method' request_target f -> Route (method', request_target, f)

let routes methods request_target f =
  List.map (fun method' -> Route (method', request_target, f)) methods

let node_type_equal a b =
  match (a, b) with
  | NSlash, NSlash -> true
  | NRest, NRest -> true
  | NExact exact1, NExact exact2 -> String.equal exact2 exact1
  | NQuery_exact (name1, value1), NQuery_exact (name2, value2) ->
      String.equal name1 name2 && String.equal value1 value2
  | NMethod meth1, NMethod meth2 -> method_equal meth1 meth2
  | NArg arg, NArg arg' -> (
      match eq arg'.id arg.id with Some Eq -> true | None -> false)
  | NQuery_arg (name1, arg1), NQuery_arg (name2, arg2) -> (
      String.equal name1 name2
      && match eq arg1.id arg2.id with Some Eq -> true | None -> false)
  | _ -> false

let rec node_type_of_request_target :
    type a b. (a, b) request_target -> node_type list = function
  | Nil -> []
  | Slash -> [ NSlash ]
  | Rest -> [ NRest ]
  | Exact (exact1, request_target) ->
      NExact exact1 :: node_type_of_request_target request_target
  | Query_exact (name, value, request_target) ->
      NQuery_exact (name, value) :: node_type_of_request_target request_target
  | Arg (arg, request_target) ->
      NArg arg :: node_type_of_request_target request_target
  | Query_arg (name, arg, request_target) ->
      NQuery_arg (name, arg) :: node_type_of_request_target request_target

let rec node : 'a node -> 'a route -> 'a node =
 fun node' (Route (method', request_target, _) as route) ->
  let rec loop node node_types =
    match node_types with
    | [] -> { node with route' = Some route }
    | node_type :: node_types ->
        let node'' =
          List.find_opt
            (fun (node_type', _) -> node_type_equal node_type node_type')
            node.node_types'
        in
        let node_types' =
          match node'' with
          | Some _ ->
              List.map
                (fun (node_type', t') ->
                  if node_type_equal node_type node_type' then
                    (node_type', loop t' node_types)
                  else (node_type', t'))
                node.node_types'
          | None -> (node_type, loop empty_node node_types) :: node.node_types'
        in
        { node with node_types' }
  in
  let node_types =
    NMethod method' :: node_type_of_request_target request_target
  in
  loop node' node_types

and empty_node : 'a node = { route' = None; node_types' = [] }

let rec compile : 'a node -> 'a router =
 fun t ->
  {
    route = t.route';
    node_types =
      List.rev t.node_types'
      |> List.map (fun (node_type, t) -> (node_type, compile t))
      |> Array.of_list;
  }

let router routes =
  compile @@ (List.concat routes |> List.fold_left node empty_node)

let rec drop : 'a list -> int -> 'a list =
 fun l n -> match l with _ :: tl when n > 0 -> drop tl (n - 1) | t -> t

let rec match' : method' -> string -> 'a router -> 'a option =
 fun method' request_target t ->
  (* split request_target into path and query tokens *)
  let request_target' = request_target |> String.trim |> Uri.of_string in
  let path_tokens =
    Uri.path request_target'
    |> String.split_on_char '/'
    |> List.tl
    |> List.map (fun tok -> `Path tok)
  in
  let query_tokens =
    Uri.query request_target'
    |> List.map (fun (k, values) -> List.map (fun v' -> `Query (k, v')) values)
    |> List.concat
  in
  let request_target_tokens = path_tokens @ query_tokens in
  (* Matching algorithm overview:

     1. First match the HTTP method as all routes always start with a HTTP method
     2. Then follow the trie nodes as suggested by the trie algorithm.
  *)
  let rec try_match t arg_values request_target_tokens matched_token_count =
    match request_target_tokens with
    | [] ->
        Option.map
          (fun (Route (_, request_target, f)) ->
            exec_route_handler f (request_target, List.rev arg_values))
          t.route
    | request_target_token :: request_target_tokens ->
        let continue = ref true in
        let index = ref 0 in
        let matched_node = ref None in
        let rest_matched = ref false in
        while !continue && !index < Array.length t.node_types do
          match (request_target_token, t.node_types.(!index)) with
          | `Path v, (NArg arg, t') -> (
              match arg.convert v with
              | Some v ->
                  matched_node := Some (t', Arg_value (arg, v) :: arg_values);
                  continue := false
              | None -> incr index)
          | `Path v, (NExact exact, t') when String.equal exact v ->
              matched_node := Some (t', arg_values);
              continue := false
          | `Path v, (NSlash, t') when String.equal "" v ->
              matched_node := Some (t', arg_values);
              continue := false
          | `Path _, (NRest, t') ->
              let path =
                drop path_tokens matched_token_count
                |> List.map (fun (`Path tok) -> tok)
                |> String.concat "/"
              in
              let rest_url =
                String.split_on_char '?' request_target |> fun l ->
                if List.length l > 1 then path ^ "?" ^ List.nth l 1 else path
              in
              matched_node :=
                Some (t', Arg_value (string_d, rest_url) :: arg_values);
              continue := false;
              rest_matched := true
          | `Query (name, value), (NQuery_arg (name', arg), t') -> (
              match arg.convert value with
              | Some v when String.equal name name' ->
                  matched_node := Some (t', Arg_value (arg, v) :: arg_values);
                  continue := false
              | _ -> incr index)
          | `Query (name1, value1), (NQuery_exact (name2, value2), t')
            when String.equal name1 name2 && String.equal value1 value2 ->
              matched_node := Some (t', arg_values);
              continue := false
          | _ -> incr index
        done;
        Option.bind !matched_node (fun (t', arg_values) ->
            let matched_tok_count = matched_token_count + 1 in
            if !rest_matched then
              (try_match [@tailcall]) t' arg_values [] matched_tok_count
            else
              (try_match [@tailcall]) t' arg_values request_target_tokens
                matched_tok_count)
  in
  if List.length request_target_tokens > 0 then
    let n = Array.length t.node_types in
    let rec loop i =
      if i = n then None
      else
        match t.node_types.(i) with
        | NMethod method'', t' when method_equal method' method'' ->
            try_match t' [] request_target_tokens 0
        | _ -> (loop [@tailcall]) (i + 1)
    in
    loop 0
  else None

and exec_route_handler :
    type a b. a -> (a, b) request_target * arg_value list -> b =
 fun f -> function
  | Nil, [] -> f
  | Rest, [ Arg_value (d, v) ] -> (
      match eq string_d.id d.id with Some Eq -> f v | None -> assert false)
  | Slash, [] -> f
  | Exact (_, request_target), arg_values ->
      exec_route_handler f (request_target, arg_values)
  | Query_exact (_, _, request_target), arg_values ->
      exec_route_handler f (request_target, arg_values)
  | ( Arg ({ id; _ }, request_target),
      Arg_value ({ id = id'; _ }, v) :: arg_values ) -> (
      match eq id id' with
      | Some Eq -> exec_route_handler (f v) (request_target, arg_values)
      | None -> assert false)
  | ( Query_arg (_, { id; _ }, request_target),
      Arg_value ({ id = id'; _ }, v) :: arg_values ) -> (
      match eq id id' with
      | Some Eq -> exec_route_handler (f v) (request_target, arg_values)
      | None -> assert false)
  | _, _ -> assert false

(* Pretty Printers *)

let pp_request_target fmt request_target =
  let rec loop :
      type a b. bool -> Format.formatter -> (a, b) request_target -> unit =
   fun qmark_printed fmt request_target ->
    match request_target with
    | Nil -> Format.fprintf fmt "%!"
    | Rest -> Format.fprintf fmt "/**%!"
    | Slash -> Format.fprintf fmt "/%!"
    | Exact (exact, request_target) ->
        Format.fprintf fmt "/%s%a" exact (loop qmark_printed) request_target
    | Query_exact (name, value, request_target) ->
        if not qmark_printed then
          Format.fprintf fmt "?%s=%s%a" name value (loop true) request_target
        else
          Format.fprintf fmt "&%s=%s%a" name value (loop qmark_printed)
            request_target
    | Arg (arg, request_target) ->
        Format.fprintf fmt "/:%s%a" arg.name (loop qmark_printed) request_target
    | Query_arg (name, arg, request_target) ->
        if not qmark_printed then
          Format.fprintf fmt "?%s=:%s%a" name arg.name (loop true)
            request_target
        else
          Format.fprintf fmt "&%s=:%s%a" name arg.name (loop qmark_printed)
            request_target
  in
  loop false fmt request_target

let pp_method fmt t =
  (match t with
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `CONNECT -> "CONNECT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `Method s -> Format.sprintf "Method (%s)" s)
  |> Format.fprintf fmt "%s"

let pp_node_type fmt node_type =
  match node_type with
  | NRest -> Format.fprintf fmt "/**"
  | NSlash -> Format.fprintf fmt "/"
  | NExact exact -> Format.fprintf fmt "/%s" exact
  | NQuery_exact (name, value) -> Format.fprintf fmt "%s=%s" name value
  | NArg arg -> Format.fprintf fmt "/:%s" arg.name
  | NQuery_arg (name, arg) -> Format.fprintf fmt "%s=:%s" name arg.name
  | NMethod method' -> Format.fprintf fmt "%a" pp_method method'

let pp_route : Format.formatter -> 'b route -> unit =
 fun fmt (Route (method', request_target, _)) ->
  Format.fprintf fmt "%a%a" pp_method method' pp_request_target request_target

let pp fmt t =
  let rec loop qmark_printed fmt t =
    let nodes = t.node_types |> Array.to_list in
    let len = List.length nodes in
    Format.pp_print_list
      ~pp_sep:(if len > 1 then Format.pp_force_newline else fun _ () -> ())
      (fun fmt (node_type, t') ->
        Format.pp_open_vbox fmt 2;
        (match node_type with
        | NQuery_exact _ | NQuery_arg _ ->
            let qmark_printed =
              if not qmark_printed then (
                Format.fprintf fmt "?%a" pp_node_type node_type;
                true)
              else (
                Format.fprintf fmt "&%a" pp_node_type node_type;
                false)
            in
            (pp' qmark_printed) fmt t'
        | node ->
            Format.fprintf fmt "%a" pp_node_type node;
            (pp' qmark_printed) fmt t');
        Format.pp_close_box fmt ())
      fmt nodes
  and pp' qmark_printed fmt t' =
    if Array.length t'.node_types > 0 then (
      Format.pp_print_break fmt 0 0;
      (loop qmark_printed) fmt t')
  in
  loop false fmt t

(* Used by wtr/request_target ppx *)

module Private = struct
  let nil = Nil
  let rest = Rest
  let slash = Slash
  let exact s request_target = Exact (s, request_target)

  let query_exact name value request_target =
    Query_exact (name, value, request_target)

  let arg d request_target = Arg (d, request_target)
  let query_arg name d request_target = Query_arg (name, d, request_target)
  let int = int_d
  let int32 = int32_d
  let int64 = int64_d
  let float = float_d
  let string = string_d
  let bool = bool_d
end
