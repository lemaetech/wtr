(* Defines route handler that are used by other examples. *)

(* User defined arg *)
module Fruit = struct
  type t = Apple | Orange | Pineapple

  let t : t Wtr.arg =
    Wtr.arg "Fruit" (function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None )
end

(* Route handlers. *)
let about_page = "about page"
let prod_page i = "Int page. number : " ^ string_of_int i
let float_page f = "Float page. number : " ^ string_of_float f
let contact_page nm num = "Contact. Hi, " ^ nm ^ ". Num " ^ string_of_int num
let product1 name id q = Format.sprintf "Product1 %s. Id: %d. q = %b" name id q
let product2 name id = Format.sprintf "Product2 %s. Id: %d." name id

let fruit_page = function
  | Fruit.Apple -> "Apples are juicy!"
  | Orange -> "Orange is a citrus fruit."
  | Pineapple -> "Pineapple has scaly skin"

let faq category_id _url =
  let category_name =
    match category_id with
    | 1 -> "products"
    | 2 -> "insurance"
    | 3 -> "returns"
    | _ -> "unknown"
  in
  "FAQ page for category : " ^ category_name
