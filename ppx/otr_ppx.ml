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

let rec decode_otr_expression ~loc otr =
  (let* uri = decode_uri otr in
   let* query_components = decode_query_tokens uri in
   let* path_components = decode_path_tokens uri in
   validate_tokens (path_components @ query_components))
  |> function
  | Ok otr_tokens -> otr_expression ~loc otr_tokens
  | Error msg -> Location.raise_errorf ~loc "otr: %s" msg

and decode_uri otr =
  let otr = String.trim otr in
  if String.length otr > 0 then
    Ok (Uri.of_string otr)
  else
    Error "Empty uri path specification"

and decode_query_tokens uri =
  let exception E of string in
  try
    Uri.query uri
    |> List.map (fun (k, v) ->
           if List.length v != 1 then
             raise
               (E (Printf.sprintf "Invalid query specification for key: %s" k))
           else
             [ k; List.hd v ])
    |> List.concat
    |> Result.ok
  with
  | E msg -> Error msg

and decode_path_tokens uri = Ok (Uri.path uri |> String.split_on_char '/')

and validate_tokens tokens =
  let validate_start tokens =
    match List.hd tokens with
    | "" -> Ok (List.tl tokens)
    | _
    | (exception _) ->
      Error "Uri path specification must start with '/'"
  in
  let validate_end_slash path =
    let _, l2 = split_on (fun x -> String.equal "" x) path in
    if List.length l2 > 0 then
      Error
        "Invalid uri path specification. No tokens allowed after trailing '/' \
         token"
    else
      Ok path
  in
  let validate_full_splat path =
    let _, l2 = split_on (fun x -> String.equal "**" x) path in
    if List.length l2 > 0 then
      Error
        "Invalid uri path specification. No tokens allowed after full splat \
         (**) token"
    else
      Ok path
  in
  validate_start tokens >>= validate_end_slash >>= validate_full_splat

and findi f l =
  let rec loop n = function
    | [] -> None
    | x :: t ->
      if f x then
        Some n
      else
        loop (n + 1) t
  in
  loop 0 l

and split_on f l =
  match findi f l with
  | Some n ->
    (List.filteri (fun i _ -> i < n) l, List.filteri (fun i _ -> i > n) l)
  | None -> (l, [])

and otr_expression ~loc = function
  | [] -> [%expr Otr.Private.nil]
  | [ "" ] -> [%expr Otr.Private.trailing_slash]
  | [ "**" ] -> [%expr Otr.Private.full_splat]
  | "*" :: components ->
    [%expr
      Otr.Private.arg Otr.Private.string [%e otr_expression ~loc components]]
  | comp :: components when Char.equal comp.[0] ':' -> (
    let comp = String.sub comp 1 (String.length comp - 1) in
    match comp with
    | "int" ->
      [%expr
        Otr.Private.arg Otr.Private.int [%e otr_expression ~loc components]]
    | "float" ->
      [%expr
        Otr.Private.arg Otr.Private.float [%e otr_expression ~loc components]]
    | "string" ->
      [%expr
        Otr.Private.arg Otr.Private.string [%e otr_expression ~loc components]]
    | "bool" ->
      [%expr
        Otr.Private.arg Otr.Private.bool [%e otr_expression ~loc components]]
    | custom_arg when capitalized custom_arg ->
      let longident_loc = { txt = Longident.parse (custom_arg ^ ".t"); loc } in
      [%expr
        Otr.Private.arg
          [%e Ast_builder.pexp_ident ~loc longident_loc]
          [%e otr_expression ~loc components]]
    | x ->
      Location.raise_errorf ~loc
        "otr: Invalid custom argument name '%s'. Custom argument component \
         name must be a valid module name."
        x)
  | comp :: components ->
    [%expr
      Otr.Private.lit
        [%e Ast_builder.estring ~loc comp]
        [%e otr_expression ~loc components]]

and capitalized s = Char.(uppercase_ascii s.[0] |> equal s.[0])

let extend ~loc ~path:_ otr = decode_otr_expression ~loc otr

let ppx_name = "otr"

let ext =
  Extension.declare ppx_name Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    extend

let () = Driver.register_transformation ppx_name ~extensions:[ ext ]
