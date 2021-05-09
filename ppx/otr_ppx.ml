open Ppxlib

let rec parse ~loc otr =
  let uri = Uri.of_string otr in
  let path = Uri.path uri |> String.split_on_char '/' in
  if starts_with_slash path then
    let _uri_components =
      path_components ~loc path @ query_components ~loc uri
    in
    Ast_builder.Default.estring ~loc otr
  else
    Location.raise_errorf ~loc "Otr path must start with '/'"

and starts_with_slash path =
  try List.hd path = "" with
  | _ -> false

and path_components ~loc path =
  let path = List.tl path in
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
    Location.raise_errorf ~loc
      "Invalid otr uri path specification. You can only have one trailing '/'."
  else
    path

and query_components ~loc uri =
  Uri.query uri
  |> List.map (fun (k, v) ->
         if List.length v != 1 then
           Location.raise_errorf ~loc "Invalid otr query specification %s." k
         else
           [ k; List.hd v ])
  |> List.concat

and to_atr_ast uri_components = uri_components

let extend ~loc ~path:_ otr =
  if String.trim otr |> String.length > 0 then
    parse ~loc otr
  else
    Location.raise_errorf ~loc "Invalid otr path specification"

let name = "otr"

let ext =
  Extension.declare name Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    extend

let () = Driver.register_transformation name ~extensions:[ ext ]
