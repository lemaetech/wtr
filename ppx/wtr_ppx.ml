(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

open Ppxlib
module Ast_builder = Ast_builder.Default

let ( let* ) r f = Result.bind r f
let ( >>= ) = ( let* )

let rec make_route ~loc ~path:_ wtr =
  let methods, uri =
    let tokens =
      String.trim wtr |> String.split_on_char ';' |> List.map String.trim
      |> List.filter (fun s -> not (String.equal "" s))
    in
    let len = List.length tokens in
    if len < 1 || len > 2 then Location.raise_errorf ~loc "Invalid wtr: %s" wtr
    else if len = 2 then (List.nth tokens 0, List.nth tokens 1)
    else ("", List.hd tokens)
  in
  (let* uri = parse_uri uri in
   let* query_components = parse_query_tokens uri in
   let* path_components = parse_path_tokens uri in
   validate_tokens (path_components @ query_components) )
  |> function
  | Ok uri_tokens ->
      let methods' = to_methods methods in
      let uris =
        ( if List.length methods' = 0 then [uri_tokens]
        else List.map (fun m -> m :: uri_tokens) methods' )
        |> make_uris ~loc
      in
      [%expr Wtr.route [%e uris]]
  | Error msg -> Location.raise_errorf ~loc "wtr: %s" msg

and make_uris ~loc = function
  | [] -> [%expr []]
  | uri_toks :: l ->
      [%expr [%e make_uri ~loc uri_toks] :: [%e make_uris ~loc l]]

and to_methods methods_str =
  String.split_on_char ',' methods_str
  |> List.filter_map (fun s ->
         let s = String.trim s in
         if String.length s > 0 then Some ("^^" ^ String.uppercase_ascii s)
         else None )

and parse_uri wtr =
  let wtr = String.trim wtr in
  if String.length wtr > 0 then Ok (Uri.of_string wtr)
  else Error "Empty uri path specification"

and parse_query_tokens uri =
  let exception E of string in
  try
    Uri.query uri
    |> List.map (fun (k, v) ->
           if List.length v != 1 then
             raise
               (E (Printf.sprintf "Invalid query specification for key: %s" k))
           else [k; List.hd v] )
    |> List.concat |> Result.ok
  with E msg -> Error msg

and parse_path_tokens uri = Ok (Uri.path uri |> String.split_on_char '/')

and validate_tokens tokens =
  let validate_start tokens =
    match List.hd tokens with
    | "" -> Ok (List.tl tokens)
    | _ | (exception _) -> Error "Uri path specification must start with '/'"
  in
  let validate_end_slash path =
    let _, l2 = split_on (fun x -> String.equal "" x) path in
    if List.length l2 > 0 then
      Error
        "Invalid uri path specification. No tokens allowed after trailing '/' \
         token"
    else Ok path
  in
  let validate_full_splat path =
    let _, l2 = split_on (fun x -> String.equal "**" x) path in
    if List.length l2 > 0 then
      Error
        "Invalid uri path specification. No tokens allowed after full splat \
         (**) token"
    else Ok path
  in
  validate_start tokens >>= validate_end_slash >>= validate_full_splat

and findi f l =
  let rec loop n = function
    | [] -> None
    | x :: t -> if f x then Some n else loop (n + 1) t
  in
  loop 0 l

and starts_with ~prefix s =
  let len_s = String.length s and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then true
    else if String.unsafe_get s i <> String.unsafe_get prefix i then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0

and split_on f l =
  match findi f l with
  | Some n ->
      (List.filteri (fun i _ -> i < n) l, List.filteri (fun i _ -> i > n) l)
  | None -> (l, [])

and make_uri ~loc = function
  | [] -> [%expr Wtr.Private.nil]
  | [""] -> [%expr Wtr.Private.trailing_slash]
  | ["**"] -> [%expr Wtr.Private.full_splat]
  | "*" :: components ->
      [%expr
        Wtr.Private.decoder Wtr.Private.string [%e make_uri ~loc components]]
  | comp :: components when Char.equal comp.[0] ':' -> (
      (* Decoders *)
      let comp = String.sub comp 1 (String.length comp - 1) in
      match comp with
      | "int" ->
          [%expr
            Wtr.Private.decoder Wtr.Private.int [%e make_uri ~loc components]]
      | "int32" ->
          [%expr
            Wtr.Private.decoder Wtr.Private.int32 [%e make_uri ~loc components]]
      | "int64" ->
          [%expr
            Wtr.Private.decoder Wtr.Private.int64 [%e make_uri ~loc components]]
      | "float" ->
          [%expr
            Wtr.Private.decoder Wtr.Private.float [%e make_uri ~loc components]]
      | "string" ->
          [%expr
            Wtr.Private.decoder Wtr.Private.string [%e make_uri ~loc components]]
      | "bool" ->
          [%expr
            Wtr.Private.decoder Wtr.Private.bool [%e make_uri ~loc components]]
      | custom_arg when capitalized custom_arg ->
          let longident_loc = {txt= Longident.parse (custom_arg ^ ".t"); loc} in
          [%expr
            Wtr.Private.decoder
              [%e Ast_builder.pexp_ident ~loc longident_loc]
              [%e make_uri ~loc components]]
      | x ->
          Location.raise_errorf ~loc
            "wtr: Invalid custom argument name '%s'. Custom argument component \
             name must be a valid module name."
            x )
  | comp :: components when starts_with ~prefix:"^^" comp ->
      (* Methods *)
      let method' = String.(sub comp 2 (length comp - 2)) in
      let meth_expr = [%expr Wtr.meth [%e Ast_builder.estring ~loc method']] in
      [%expr Wtr.Private.method' [%e meth_expr] [%e make_uri ~loc components]]
  | comp :: components ->
      [%expr
        Wtr.Private.lit
          [%e Ast_builder.estring ~loc comp]
          [%e make_uri ~loc components]]

and capitalized s = Char.(uppercase_ascii s.[0] |> equal s.[0])

let ppx_name = "wtr"

let ext =
  Extension.declare ppx_name Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    make_route

let () = Driver.register_transformation ppx_name ~extensions:[ext]
