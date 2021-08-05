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

let create_decoder ~name ~decode =
  let id = new_id () in
  {name; decode; id}

let int = create_decoder ~name:"int" ~decode:int_of_string_opt
let int32 = create_decoder ~name:"int32" ~decode:Int32.of_string_opt
let int64 = create_decoder ~name:"int64" ~decode:Int64.of_string_opt
let float = create_decoder ~name:"float" ~decode:float_of_string_opt
let string = create_decoder ~name:"string" ~decode:(fun a -> Some a)
let bool = create_decoder ~name:"bool" ~decode:bool_of_string_opt

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

let pp_method' fmt t =
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

let method' meth =
  String.uppercase_ascii meth
  |> function
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "DELETE" -> `DELETE
  | "CONNECT" -> `CONNECT
  | "OPTIONS" -> `OPTIONS
  | "TRACE" -> `TRACE
  | header -> `Method header

(** [('a, 'b) uri] represents a uniform resource identifier. The variant members
    describe the uri component types.

    - Literal Uri path literal string component eg. 'home' in '/home'
    - Decoder Uri path argument component, i.e. the value is determined during
      runtime, eg. ':int' in '/home/:int' *)
type ('a, 'b) uri =
  | Nil : ('b, 'b) uri
  | Method : method' * ('a, 'b) uri -> ('a, 'b) uri
  | Full_splat : ('b, 'b) uri
  | Trailing_slash : ('b, 'b) uri
  | Literal : string * ('a, 'b) uri -> ('a, 'b) uri
  | Decoder : 'c decoder * ('a, 'b) uri -> ('c -> 'a, 'b) uri

let rec pp_uri : type a b. Format.formatter -> (a, b) uri -> unit =
 fun fmt -> function
  | Nil -> Format.fprintf fmt "%!"
  | Method (meth, uri) -> Format.fprintf fmt "%a%a" pp_method' meth pp_uri uri
  | Full_splat -> Format.fprintf fmt "/**%!"
  | Trailing_slash -> Format.fprintf fmt "/%!"
  | Literal (lit, uri) -> Format.fprintf fmt "/%s%a" lit pp_uri uri
  | Decoder (decoder, uri) ->
      Format.fprintf fmt "/:%s%a" decoder.name pp_uri uri

type 'c route = Route : ('a, 'c) uri * 'a -> 'c route

let route : ('a, 'b) uri list -> 'a -> 'b route list =
 fun uris f -> List.map (fun uri' -> Route (uri', f)) uris

let pp_route : Format.formatter -> 'b route -> unit =
 fun fmt (Route (uri, _)) -> pp_uri fmt uri

(** Existential to encode uri component/node type. *)
type node_type =
  | PTrailing_slash : node_type
  | PFull_splat : node_type
  | PLiteral : string -> node_type
  | PMethod : method' -> node_type
  | PDecoder : 'c decoder -> node_type

let node_type_equal a b =
  match (a, b) with
  | PTrailing_slash, PTrailing_slash -> true
  | PFull_splat, PFull_splat -> true
  | PLiteral lit', PLiteral lit -> String.equal lit lit'
  | PMethod meth1, PMethod meth2 -> method_equal meth1 meth2
  | PDecoder decoder, PDecoder decoder' -> (
    match eq decoder'.id decoder.id with Some Eq -> true | None -> false )
  | _ -> false

(* [node_type_of_uri uri] converts [uri] to [node_type list]. This is done to get around OCaml
   type inference issue when using [uri] type in the [node] function below. *)
let rec node_type_of_uri : type a b. (a, b) uri -> node_type list = function
  | Nil -> []
  | Method (meth, uri) -> PMethod meth :: node_type_of_uri uri
  | Trailing_slash -> [PTrailing_slash]
  | Full_splat -> [PFull_splat]
  | Literal (lit, uri) -> PLiteral lit :: node_type_of_uri uri
  | Decoder (decoder, uri) -> PDecoder decoder :: node_type_of_uri uri

let node_type_to_string node_type =
  match node_type with
  | PFull_splat -> Format.sprintf "/**%!"
  | PTrailing_slash -> Format.sprintf "/%!"
  | PLiteral lit -> Format.sprintf "/%s" lit
  | PDecoder decoder -> Format.sprintf "/:%s" decoder.name
  | PMethod method' -> Format.asprintf "%a" pp_method' method'

(** ['a t] is a node in a trie based router. *)
type 'a node = {route: 'a route option; node_types: (node_type * 'a node) list}

let rec node : 'a node -> 'a route -> 'a node =
 fun node' (Route (uri, _) as route) ->
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
  let node_types = node_type_of_uri uri in
  loop node' node_types

and empty_node : 'a node = {route= None; node_types= []}

(* We use array for node_types so that we get better cache locality. The hope being
   that iterating nodes via array is faster than via the list. *)
type 'a t = {route: 'a route option; node_types: (node_type * 'a t) array}

let rec create routes =
  List.concat routes |> List.fold_left node empty_node |> compile

and compile : 'a node -> 'a t =
 fun t ->
  { route= t.route
  ; node_types=
      List.rev t.node_types
      |> List.map (fun (node_type, t) -> (node_type, compile t))
      |> Array.of_list }

let pp fmt t =
  let open PPrint in
  let rec doc t =
    separate_map hardline
      (fun (node_type, t') -> string (node_type_to_string node_type) ^//^ doc t')
      (Array.to_list t.node_types)
  in
  ToFormatter.pretty 0. 80 fmt (doc t)

type decoded_value = D : 'c decoder * 'c -> decoded_value

let rec match' method' uri (t : 'a t) =
  let rec try_match t decoded_values uri_toks =
    match uri_toks with
    | [] ->
        Option.map
          (fun (Route (uri, f)) ->
            exec_route_handler f (uri, List.rev decoded_values) )
          t.route
    | uri :: uris ->
        let continue = ref true in
        let index = ref 0 in
        let matched_node = ref None in
        let full_splat_matched = ref false in
        while !continue && !index < Array.length t.node_types do
          match t.node_types.(!index) with
          | PDecoder decoder, t' -> (
            match decoder.decode uri with
            | Some v ->
                matched_node := Some (t', D (decoder, v) :: decoded_values) ;
                continue := false
            | None -> incr index )
          | PLiteral lit, t' when String.equal lit uri ->
              matched_node := Some (t', decoded_values) ;
              continue := false
          | PTrailing_slash, t' when String.equal "" uri ->
              matched_node := Some (t', decoded_values) ;
              continue := false
          | PFull_splat, t' ->
              matched_node := Some (t', decoded_values) ;
              continue := false ;
              full_splat_matched := true
          | _ -> incr index
        done ;
        Option.bind !matched_node (fun (t', decoded_values) ->
            if !full_splat_matched then
              (try_match [@tailcall]) t' decoded_values []
            else (try_match [@tailcall]) t' decoded_values uris )
  in
  (* split uri path and query into tokens *)
  let uri_toks =
    let uri = String.trim uri |> Uri.of_string in
    let uri_tokens = Uri.path uri |> String.split_on_char '/' |> List.tl in
    Uri.query uri
    |> List.map (fun (k, v) ->
           if List.length v > 0 then [k; List.hd v] else [k] )
    |> List.concat |> List.append uri_tokens
  in
  (* Matching algorithm overview:

     1. First match the HTTP method as all routes always start with a HTTP method
     2. Then follow the trie nodes as suggested by the trie algorithm.
  *)
  if List.length uri_toks > 0 then
    let n = Array.length t.node_types in
    let rec loop i =
      if i = n then None
      else
        match t.node_types.(i) with
        | PMethod method'', t' when method_equal method' method'' ->
            try_match t' [] uri_toks
        | _ -> loop (n + 1)
    in
    loop 0
  else None

and exec_route_handler : type a b. a -> (a, b) uri * decoded_value list -> b =
 fun f -> function
  | Nil, [] -> f
  | Full_splat, [] -> f
  | Trailing_slash, [] -> f
  | Method (_, uri), decoded_values -> exec_route_handler f (uri, decoded_values)
  | Literal (_, uri), decoded_values ->
      exec_route_handler f (uri, decoded_values)
  | Decoder ({id; _}, uri), D ({id= id'; _}, v) :: decoded_values -> (
    match eq id id' with
    | Some Eq -> exec_route_handler (f v) (uri, decoded_values)
    | None -> assert false )
  | _, _ -> assert false

module Private = struct
  let route = route
  let nil = Nil
  let trailing_slash = Trailing_slash
  let full_splat = Full_splat
  let lit s uri = Literal (s, uri)
  let decoder d uri = Decoder (d, uri)
  let method' meth uri = Method (meth, uri)
  let int = int
  let int32 = int32
  let int64 = int64
  let float = float
  let bool = bool
  let string = string
end
