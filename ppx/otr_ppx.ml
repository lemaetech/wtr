open Ppxlib

let rec parse ~loc otr =
  let uri = Uri.of_string otr in
  let path = Uri.path uri |> String.split_on_char '/' in
  if starts_with_slash path then
    Ast_builder.Default.estring ~loc otr
  else
    Location.raise_errorf ~loc "Otr path must start with '/'"

and starts_with_slash path =
  try List.hd path = "" with
  | _ -> false

and query_components ~loc uri =
  Uri.query uri
  |> List.map (fun (k, v) ->
         if List.length v != 1 then
           Location.raise_errorf ~loc "Invalid query specifiction %s" k
         else
           [ k; List.hd v ])
  |> List.concat

and to_atr_ast uri_components = uri_components

let transform_otr ~loc ~path:_ otr =
  if String.trim otr |> String.length > 0 then
    parse ~loc otr
  else
    Location.raise_errorf ~loc "Invalid otr path specification"

let name = "otr"

let ext =
  Extension.declare name Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    transform_otr

let () = Driver.register_transformation name ~extensions:[ ext ]
