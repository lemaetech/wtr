open Ppxlib
module Ast_builder = Ast_builder.Default

let ( let* ) r f = Result.bind r f

let ( >>= ) = ( let* )

let ( let+ ) r f = Result.map f r

let rec decode_otr_expression ~loc otr =
  (let* uri = decode_uri otr in
   let* path_components = decode_path_tokens uri in
   let+ query_components = decode_query_tokens uri in
   path_components @ query_components)
  |> function
  | Ok otr_tokens -> otr_expression ~loc otr_tokens
  | Error msg -> Location.raise_errorf ~loc "otr: %s" msg

and decode_uri otr =
  if String.trim otr |> String.length > 0 then
    Ok (Uri.of_string otr)
  else
    Error "Path is empty"

and decode_path_tokens uri =
  let valid_start_token uri =
    let path = Uri.path uri |> String.split_on_char '/' in
    match List.hd path with
    | "" -> Ok (List.tl path)
    | _
    | (exception _) ->
      Error "Path specification must start with '/'"
  in
  let valid_end_token path =
    let trailing_slash_count =
      List.fold_left
        (fun acc a ->
          if String.equal a "" then
            acc + 1
          else
            acc)
        0 path
    in
    if trailing_slash_count > 1 then
      Error "Invalid uri path specification. Only one trailing '/' is allowed"
    else
      Ok path
  in
  valid_start_token uri >>= valid_end_token

and decode_query_tokens uri =
  let exception E of string in
  try
    Uri.query uri
    |> List.map (fun (k, v) ->
           if List.length v != 1 then
             raise (E (Printf.sprintf "Invalid query specification %s" k))
           else
             [ k; List.hd v ])
    |> List.concat
    |> Result.ok
  with
  | E msg -> Error msg

and otr_expression ~loc = function
  | [] -> [%expr Otr.Private.nil]
  | [ "" ] -> [%expr Otr.Private.slash_end]
  | "" :: _ ->
    Location.raise_errorf
      "otr: query specification not allowed after trailing '/' in path."
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
      let longident_loc = { txt = Longident.parse custom_arg; loc } in
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
