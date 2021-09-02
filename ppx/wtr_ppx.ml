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

let findi f l =
  let rec loop n = function
    | [] -> None
    | x :: t -> if f x then Some n else loop (n + 1) t
  in
  loop 0 l

let split_on f l =
  match findi f l with
  | Some n ->
      (List.filteri (fun i _ -> i < n) l, List.filteri (fun i _ -> i > n) l)
  | None -> (l, [])

let capitalized s = Char.(uppercase_ascii s.[0] |> equal s.[0])

let validate_path_tokens tokens =
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

let path_tokens uri =
  Uri.path uri |> String.split_on_char '/' |> validate_path_tokens

let query_tokens uri =
  let exception E of string in
  try
    Uri.query uri
    |> List.map (fun (k, v) ->
           if List.length v != 1 then
             raise
               (E (Printf.sprintf "Invalid query specification for key: %s" k))
           else (k, List.hd v) )
    |> Result.ok
  with E msg -> Error msg

let request_target_tokens uri =
  let wtr = String.trim uri in
  if String.length wtr > 0 then
    let uri = Uri.of_string wtr in
    let* path_components = path_tokens uri in
    let* query_components = query_tokens uri in
    Ok (path_components, query_components)
  else Error "Empty uri path specification"

let make_methods : loc:location -> string -> expression =
 fun ~loc methods_str ->
  String.split_on_char ',' methods_str
  |> List.filter_map (fun s ->
         let s = String.trim s in
         if String.length s > 0 then Some s else None )
  |> List.fold_left
       (fun expr method' ->
         let method' = Ast_builder.estring ~loc method' in
         [%expr Wtr.method' [%e method'] :: [%e expr]] )
       [%expr []]

let rec make_query ~loc query_tokens =
  match query_tokens with
  | [] -> [%expr Wtr.Private.nil]
  | (name, "*") :: toks ->
      [%expr
        Wtr.Private.(
          query_decode
            [%e Ast_builder.estring ~loc name]
            string [%e make_query ~loc toks])]
  | (name, uri) :: uris when Char.equal uri.[0] ':' -> (
      let comp = String.sub uri 1 (String.length uri - 1) in
      let name_expr = Ast_builder.estring ~loc name in
      match comp with
      | "int" ->
          [%expr
            Wtr.Private.(
              query_decode [%e name_expr] int [%e make_query ~loc uris])]
      | "int32" ->
          [%expr
            Wtr.Private.(
              query_decode [%e name_expr] int32 [%e make_query ~loc uris])]
      | "int64" ->
          [%expr
            Wtr.Private.(
              query_decode [%e name_expr] int64 [%e make_query ~loc uris])]
      | "float" ->
          [%expr
            Wtr.Private.(
              query_decode [%e name_expr] float [%e make_query ~loc uris])]
      | "string" ->
          [%expr
            Wtr.Private.(
              query_decode [%e name_expr] string [%e make_query ~loc uris])]
      | "bool" ->
          [%expr
            Wtr.Private.(
              query_decode [%e name_expr] bool [%e make_query ~loc uris])]
      | custom_arg when capitalized custom_arg ->
          let longident_loc = {txt= Longident.parse (custom_arg ^ ".t"); loc} in
          [%expr
            Wtr.Private.query_decode [%e name_expr]
              [%e Ast_builder.pexp_ident ~loc longident_loc]
              [%e make_query ~loc uris]]
      | x ->
          Location.raise_errorf ~loc
            "wtr: Invalid custom argument name '%s'. Custom argument component \
             name must be a valid module name."
            x )
  | (name, uri) :: uris ->
      [%expr
        Wtr.Private.query_lit
          [%e Ast_builder.estring ~loc name]
          [%e Ast_builder.estring ~loc uri]
          [%e make_query ~loc uris]]

let rec make_request_target ~loc query_tokens = function
  | [] -> make_query ~loc query_tokens
  | [""] -> [%expr Wtr.Private.t_slash]
  | ["**"] -> [%expr Wtr.Private.splat]
  | "*" :: path_tokens ->
      [%expr
        Wtr.Private.(
          decode string [%e make_request_target ~loc query_tokens path_tokens])]
  | path_tok :: path_tokens when Char.equal path_tok.[0] ':' -> (
      let comp = String.sub path_tok 1 (String.length path_tok - 1) in
      match comp with
      | "int" ->
          [%expr
            Wtr.Private.(
              decode int [%e make_request_target ~loc query_tokens path_tokens])]
      | "int32" ->
          [%expr
            Wtr.Private.(
              decode int32
                [%e make_request_target ~loc query_tokens path_tokens])]
      | "int64" ->
          [%expr
            Wtr.Private.(
              decode int64
                [%e make_request_target ~loc query_tokens path_tokens])]
      | "float" ->
          [%expr
            Wtr.Private.(
              decode float
                [%e make_request_target ~loc query_tokens path_tokens])]
      | "string" ->
          [%expr
            Wtr.Private.(
              decode string
                [%e make_request_target ~loc query_tokens path_tokens])]
      | "bool" ->
          [%expr
            Wtr.Private.(
              decode bool [%e make_request_target ~loc query_tokens path_tokens])]
      | custom_arg when capitalized custom_arg ->
          let longident_loc = {txt= Longident.parse (custom_arg ^ ".t"); loc} in
          [%expr
            Wtr.Private.decode
              [%e Ast_builder.pexp_ident ~loc longident_loc]
              [%e make_request_target ~loc query_tokens path_tokens]]
      | x ->
          Location.raise_errorf ~loc
            "wtr: Invalid custom argument name '%s'. Custom argument component \
             name must be a valid module name."
            x )
  | path_tok :: path_tokens ->
      [%expr
        Wtr.Private.lit
          [%e Ast_builder.estring ~loc path_tok]
          [%e make_request_target ~loc query_tokens path_tokens]]

let wtr ~loc ~path:_ wtr =
  let wtr = String.trim wtr in
  let methods, uri =
    let tokens =
      String.split_on_char ';' wtr
      |> List.map String.trim
      |> List.filter (fun s -> not (String.equal "" s))
    in
    let len = List.length tokens in
    if len > 2 then
      Location.raise_errorf ~loc
        "Invalid wtr: %s. Valid wtr is: [HTTP methods separated by comma (,)] \
         ; [URI]"
        wtr
    else if len = 2 then (List.nth tokens 0, List.nth tokens 1)
      (* Default method is `GET *)
    else ("get", List.nth tokens 0)
  in
  match request_target_tokens uri with
  | Ok (path_tokens, query_tokens) ->
      let methods' = make_methods ~loc methods in
      let uri = make_request_target ~loc query_tokens path_tokens in
      [%expr Wtr.routes [%e methods'] [%e uri]]
  | Error msg -> Location.raise_errorf ~loc "wtr: %s" msg

let wtr_ppx = "wtr"

let wtr_ext =
  Extension.declare wtr_ppx Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    wtr

let () = Driver.register_transformation wtr_ppx ~extensions:[wtr_ext]
