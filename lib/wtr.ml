(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

module Decoder = struct
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

  type 'a t =
    { name: string (* name e.g. int, float, bool, string etc *)
    ; decode: string -> 'a option
    ; id: 'a id }

  let create ~name ~decode =
    let id = new_id () in
    {name; decode; id}

  let int = create ~name:"int" ~decode:int_of_string_opt
  let int32 = create ~name:"int32" ~decode:Int32.of_string_opt
  let int64 = create ~name:"int64" ~decode:Int64.of_string_opt
  let float = create ~name:"float" ~decode:float_of_string_opt
  let string = create ~name:"string" ~decode:(fun a -> Some a)
  let bool = create ~name:"bool" ~decode:bool_of_string_opt
end

type 'a decoder = 'a Decoder.t

let create_decoder = Decoder.create

module type Decoder = sig
  type t

  val t : t decoder
end

(** [('a, 'b) uri] represents a uniform resource identifier. The variant members
    describe the uri component types.

    - Literal Uri path literal string component eg. 'home' in '/home'
    - Decoder Uri path argument component, i.e. the value is determined during
      runtime, eg. ':int' in '/home/:int' *)
type ('a, 'b) uri =
  | Nil : ('b, 'b) uri
  | Full_splat : ('b, 'b) uri
  | Trailing_slash : ('b, 'b) uri
  | Literal : string * ('a, 'b) uri -> ('a, 'b) uri
  | Decoder : 'c Decoder.t * ('a, 'b) uri -> ('c -> 'a, 'b) uri

let rec pp_uri : type a b. Format.formatter -> (a, b) uri -> unit =
 fun fmt -> function
  | Nil -> Format.fprintf fmt "%!"
  | Full_splat -> Format.fprintf fmt "/**%!"
  | Trailing_slash -> Format.fprintf fmt "/%!"
  | Literal (lit, uri) -> Format.fprintf fmt "/%s%a" lit pp_uri uri
  | Decoder (decoder, uri) ->
      Format.fprintf fmt "/:%s%a" decoder.name pp_uri uri

type meth =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]

let meth_equal (meth1 : meth) (meth2 : meth) = compare meth1 meth2 = 0

let pp_meth fmt t =
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

type 'c route = Route : meth option * ('a, 'c) uri * 'a -> 'c route

let route : ?meth:meth -> ('a, 'b) uri -> 'a -> 'b route =
 fun ?meth uri f -> Route (meth, uri, f)

let ( >- ) : ('a, 'b) uri -> 'a -> 'b route =
 fun uri f -> route ?meth:None uri f

(** Existential to encode uri component/node type. *)
type node_type =
  | PTrailing_slash : node_type
  | PFull_splat : node_type
  | PLiteral : string -> node_type
  | PMethod : meth -> node_type
  | PDecoder : 'c Decoder.t -> node_type

let node_type_equal a b =
  match (a, b) with
  | PTrailing_slash, PTrailing_slash -> true
  | PFull_splat, PFull_splat -> true
  | PLiteral lit', PLiteral lit -> String.equal lit lit'
  | PMethod meth1, PMethod meth2 -> meth_equal meth1 meth2
  | PDecoder decoder, PDecoder decoder' -> (
    match Decoder.eq decoder'.id decoder.id with
    | Some Decoder.Eq -> true
    | None -> false )
  | _ -> false

(* [node_type_of_uri uri] converts [uri] to [node_type list]. This is done to get around OCaml
   type inference issue when using [uri] type in the [node] function below. *)
let rec node_type_of_uri : type a b. (a, b) uri -> node_type list = function
  | Nil -> []
  | Trailing_slash -> [PTrailing_slash]
  | Full_splat -> [PFull_splat]
  | Literal (lit, uri) -> PLiteral lit :: node_type_of_uri uri
  | Decoder (decoder, uri) -> PDecoder decoder :: node_type_of_uri uri

(** ['a t] is a node in a trie based router. *)
type 'a node = {route: 'a route option; node_types: (node_type * 'a node) list}

let rec node : 'a node -> 'a route -> 'a node =
 fun node' (Route (meth, uri, _) as route) ->
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
  let node_types =
    match meth with
    | Some meth' -> PMethod meth' :: node_types
    | None -> node_types
  in
  loop node' node_types

and empty_node : 'a node = {route= None; node_types= []}

type 'a t = {route: 'a route option; node_types: (node_type * 'a t) array}

let rec create routes = List.fold_left node empty_node routes |> compile

and compile : 'a node -> 'a t =
 fun t ->
  { route= t.route
  ; node_types=
      List.rev t.node_types
      |> List.map (fun (uri_kind, t) -> (uri_kind, compile t))
      |> Array.of_list }

type decoded_value = D : 'c Decoder.t * 'c -> decoded_value

let rec match' ?meth t uri =
  let rec loop t decoded_values = function
    | [] ->
        Option.map
          (fun (Route (_, uri, f)) ->
            exec_route_handler f (uri, List.rev decoded_values) )
          t.route
    | uri :: uris ->
        let continue = ref true in
        let index = ref 0 in
        let matched_node = ref None in
        let full_splat_matched = ref false in
        let method_matched = ref false in
        while !continue && !index < Array.length t.node_types do
          match t.node_types.(!index) with
          | PMethod meth', t' -> (
            match meth with
            | Some meth'' when meth_equal meth' meth'' ->
                matched_node := Some (t', decoded_values) ;
                continue := false ;
                method_matched := true
            | Some _ | None -> incr index )
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
            if !full_splat_matched then (loop [@tailcall]) t' decoded_values []
            else if !method_matched then
              (loop [@tailcall]) t' decoded_values (uri :: uris)
            else (loop [@tailcall]) t' decoded_values uris )
  in
  let uri = String.trim uri in
  if String.length uri > 0 then loop t [] (uri_tokens uri) else None

and uri_tokens s =
  let uri = Uri.of_string s in
  let uri_tokens = Uri.path uri |> String.split_on_char '/' |> List.tl in
  Uri.query uri
  |> List.map (fun (k, v) -> if List.length v > 0 then [k; List.hd v] else [k])
  |> List.concat |> List.append uri_tokens

and exec_route_handler : type a b. a -> (a, b) uri * decoded_value list -> b =
 fun f -> function
  | Nil, [] -> f
  | Full_splat, [] -> f
  | Trailing_slash, [] -> f
  | Literal (_, uri), decoded_values ->
      exec_route_handler f (uri, decoded_values)
  | Decoder ({id; _}, uri), D ({id= id'; _}, v) :: decoded_values -> (
    match Decoder.eq id id' with
    | Some Decoder.Eq -> exec_route_handler (f v) (uri, decoded_values)
    | None -> assert false )
  | _, _ -> assert false

module Private = struct
  let nil = Nil
  let trailing_slash = Trailing_slash
  let full_splat = Full_splat
  let lit s uri = Literal (s, uri)
  let decoder a p = Decoder (a, p)
  let int = Decoder.int
  let int32 = Decoder.int32
  let int64 = Decoder.int64
  let float = Decoder.float
  let bool = Decoder.bool
  let string = Decoder.string
end
