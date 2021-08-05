(* Steps to run the demo.

    1. Install dependencies.
    opam install cohttp-lwt-unix

    2. Run the server
    dune exec examples/cohttp.exe
*)

(* User defined decoder *)
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
let prod_page i = Printf.sprintf "Product Page. Product Id : %i" i
let float_page f = Printf.sprintf "Float page. number : %f" f
let contact_page nm num = Printf.sprintf "Contact. Hi, %s. Num: %i" nm num
let product1 name id q = Printf.sprintf "Product1 %s. Id: %d. q = %b" name id q
let product2 name id = Printf.sprintf "Product2 %s. Id: %d." name id

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
  Wtr.create
    [ {%wtr| get,post; /home/about                 |} "about page"
    ; {%wtr| get; /home/:int/                           |} prod_page
    ; {%wtr| get;/home/:float/                         |} float_page
    ; {%wtr| get;/contact/*/:int                       |} contact_page
    ; {%wtr| get;/product/:string?section=:int&q=:bool |} product1
    ; {%wtr| get;/product/:string?section=:int&q1=yes  |} product2
    ; {%wtr| get;/fruit/:Fruit                         |} fruit_page
    ; {%wtr| get;/faq/:int/**                          |} faq ]

let () =
  let callback _conn req _body =
    let uri = req |> Cohttp_lwt_unix.Request.uri |> Uri.path_and_query in
    let method' =
      Cohttp_lwt_unix.Request.meth req
      |> Cohttp.Code.string_of_method |> Wtr.method'
    in
    Wtr.match' method' uri router
    |> function
    | Some resp ->
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:resp ()
    | None -> Cohttp_lwt_unix.Server.respond_not_found ()
  in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port 5000))
    (Cohttp_lwt_unix.Server.make ~callback ())
  |> Lwt_main.run
