(* User defined data type. *)
module Fruit = struct
  type t = Apple | Orange | Pineapple

  let t : t Wtr.decoder =
    Wtr.create_decoder ~name:"fruit" ~decode:(function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None )
end

(* route handlers. *)
let prod_page i = "Int page. number : " ^ string_of_int i
let float_page f = "Float page. number : " ^ string_of_float f
let contact_page nm num = "Contact. Hi, " ^ nm ^ ". Num " ^ string_of_int num
let product1 name id q = Format.sprintf "Product1 %s. Id: %d. q = %b" name id q
let product2 name id = Format.sprintf "Product2 %s. Id: %d." name id

let fruit_page = function
  | Fruit.Apple -> "Apples are juicy!"
  | Orange -> "Orange is a citrus fruit."
  | Pineapple -> "Pineapple has scaly skin"

let faq category_id =
  let category_name =
    match category_id with
    | 1 -> "products"
    | 2 -> "insurance"
    | 3 -> "returns"
    | _ -> "unknown"
  in
  "FAQ page for category : " ^ category_name

(* create a router *)
let router =
  Wtr.(
    create
      [ {%wtr| get,post,head,delete  ; /home/about/:int   |} (fun _ ->
            "about page" )
      ; {%wtr| get                   ; /home/:int/        |} prod_page
      ; {%wtr| get,post              ; /home/:float/      |} float_page
      ; {%wtr| /contact/*/:int                            |} contact_page
      ; {%wtr| /product/:string?section=:int&q=:bool      |} product1
      ; {%wtr| /product/:string?section=:int&q1=yes       |} product2
      ; {%wtr| /fruit/:Fruit                              |} fruit_page
      ; {%wtr| /faq/:int/**                               |} faq ])

let () =
  Format.(fprintf std_formatter "====Routes====\n%a\n" Wtr.pp router) ;
  Printf.printf "\n====Router Match Results====\n" ;
  [ Wtr.match' ~meth:`GET router "/home/100001.1/"
  ; Wtr.match' ~meth:`GET router "/home/100001/"
  ; Wtr.match' ~meth:`GET router "/home/about/12"
  ; Wtr.match' router "/product/dyson350?section=233&q=true"
  ; Wtr.match' router "/product/dyson350?section=2&q=false"
  ; Wtr.match' router "/product/dyson350?section=2&q1=yes"
  ; Wtr.match' router "/product/dyson350?section=2&q1=no"
  ; Wtr.match' router "/fruit/apple"
  ; Wtr.match' router "/fruit/orange"
  ; Wtr.match' router "/fruit/pineapple"
  ; Wtr.match' router "/fruit/guava"
  ; Wtr.match' router "/faq/1/"
  ; Wtr.match' router "/faq/1/whatever"
  ; Wtr.match' router "/faq/2/whateasdfasdfasdf" ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
       | None -> Printf.printf "%3d: None\n" (i + 1) )

(* Should output below:

 *====Routes====
   GET
   /home
    /about
      /:int

    /:int
      /

    /:float
      /

   POST
   /home
    /about
      /:int

    /:float
      /

   HEAD
   /home
    /about
      /:int

   DELETE
   /home
    /about
      /:int

   /contact
   /:string
    /:int

   /product
   /:string
    /section
      /:int
        /q
          /:bool

        /q1
          /yes

   /fruit
   /:fruit

   /faq
   /:int
    /**

   ====Router Match Results====
   1: Float page. number : 100001.1
   2: Int page. number : 100001
   3: about page
   4: Product1 dyson350. Id: 233. q = true
   5: Product1 dyson350. Id: 2. q = false
   6: Product2 dyson350. Id: 2.
   7: None
   8: Apples are juicy!
   9: Orange is a citrus fruit.
   10: Pineapple has scaly skin
   11: None
   12: FAQ page for category : products
   13: FAQ page for category : products
   14: FAQ page for category : insurance)

 *)
