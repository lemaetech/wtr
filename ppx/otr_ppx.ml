open Ppxlib

let expand ~loc ~path:_ otr = Ast_builder.Default.estring ~loc otr

let name = "otr"

let ext =
  Extension.declare name Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let () = Driver.register_transformation name ~extensions:[ ext ]
