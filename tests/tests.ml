let () = Printexc.record_backtrace false

module Fruit = struct
  type t = Apple | Orange | Pineapple

  let t : t Wtr.decoder =
    Wtr.create_decoder ~name:"Fruit" ~decode:(function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None )
end

let fruit_page = function
  | Fruit.Apple -> Printf.sprintf "Apples are juicy!"
  | Orange -> Printf.sprintf "Orange is a citrus fruit."
  | Pineapple -> Printf.sprintf "Pineapple has scaly skin"

let about_page i = Format.sprintf "about_page - %d" i
let full_splat_page url = Format.sprintf "full splat page: %s" url
let home_int_page i = Printf.sprintf "Product Page. Product Id : %d" i
let home_float_page f = Printf.sprintf "Float page. number : %f" f

let wildcard_page s url =
  Printf.sprintf "Wildcard page. %s. Remaining url: %s" s url

let numbers_page id code = Printf.sprintf "int32: %ld, int64: %Ld." id code
let not_found_page = "404 Not found"

let contact_page name number =
  Printf.sprintf "Contact page. Hi, %s. Number %i" name number

let contact_page2 name call_me_later =
  Printf.sprintf "Contact Page2. Name - %s, number - %b" name call_me_later

let product_page name section_id q =
  Printf.sprintf "Product detail - %s. Section: %d. Display questions? %b" name
    section_id q

let product_page2 name section_id =
  Printf.sprintf "Product detail 2 - %s. Section: %d." name section_id

let public url = Format.sprintf "file path: %s" url

let router =
  Wtr.create
    [ {%wtr| get,post  ;         /home/about/:int          |} about_page
    ; {%wtr| head,delete;        /home/:int/               |} home_int_page
    ; {%wtr| get;   /home/:float/                          |} home_float_page
    ; {%wtr| get;   /contact/*/:int                        |} contact_page
    ; {%wtr| post;  /home/products/**                      |} full_splat_page
    ; {%wtr| get;   /home/*/**                             |} wildcard_page
    ; {%wtr| get;   /contact/:string/:bool                 |} contact_page2
    ; {%wtr| post;  /product/:string?section=:int&q=:bool  |} product_page
    ; {%wtr| get;   /product/:string?section=:int&q1=yes   |} product_page2
    ; {%wtr| get;   /fruit/:Fruit                          |} fruit_page
    ; {%wtr| get;   /                                      |} not_found_page
    ; {%wtr| get;   /public/**                             |} public
    ; {%wtr| head;  /numbers/:int32/code/:int64/           |} numbers_page ]

let pp_route r = List.hd r |> Wtr.pp_route Format.std_formatter

let pp_match method' uri =
  Wtr.match' method' uri router
  |> function
  | Some s -> Printf.printf {|"%s%!"|} s | None -> Printf.printf "None%!"

let%expect_test _ =
  pp_match `GET "/public/css/style.css" ;
  [%expect {|
       "file path: css/style.css" |}]

let%expect_test _ =
  pp_match `GET "/public/js/prog.js" ;
  [%expect {|
       "file path: js/prog.js" |}]

let%expect_test _ =
  pp_match `GET "/public/images/image1.jpg" ;
  [%expect {|
       "file path: images/image1.jpg" |}]

let%expect_test _ =
  pp_match `GET "/home/100001.1/" ;
  [%expect {|
       "Float page. number : 100001.100000" |}]

let%expect_test _ =
  pp_match `POST "/home/100001.1" ;
  [%expect {|
     None |}]

let%expect_test _ =
  pp_match `HEAD "/home/100001/" ;
  [%expect {| "Product Page. Product Id : 100001" |}]

let%expect_test _ =
  pp_match `POST "/home/about" ;
  [%expect {|
     None |}]

let%expect_test _ =
  pp_match `GET "/home/about/1" ;
  [%expect {| "about_page - 1" |}]

let%expect_test _ =
  pp_match `POST "/home/about/3" ;
  [%expect {| "about_page - 3" |}]

let%expect_test _ =
  pp_match `HEAD "/home/about/3" ;
  [%expect {| None |}]

let%expect_test _ =
  pp_match `DELETE "/home/about/3" ;
  [%expect {| None |}]

let%expect_test _ =
  pp_match `GET "/contact/bikal/123456" ;
  [%expect {|
       "Contact page. Hi, bikal. Number 123456" |}]

let%expect_test _ =
  pp_match `POST "/home/products/asdfasdf?a=1&b=2" ;
  [%expect {|
       "full splat page: asdfasdf?a=1&b=2" |}]

let%expect_test _ =
  pp_match `POST "/home/products/product1/locate" ;
  [%expect {|
       "full splat page: product1/locate" |}]

let%expect_test _ =
  pp_match `GET "/home/product1/" ;
  [%expect {|
       "Wildcard page. product1. Remaining url: " |}]

let%expect_test _ =
  pp_match `GET "/contact/bikal/true" ;
  [%expect {|
       "Contact Page2. Name - bikal, number - true" |}]

let%expect_test _ =
  pp_match `GET "/contact/bob/false" ;
  [%expect {|
       "Contact Page2. Name - bob, number - false" |}]

let%expect_test _ =
  pp_match `POST "/product/dyson350?section=233&q=true" ;
  [%expect
    {|
         "Product detail - dyson350. Section: 233. Display questions? true" |}]

let%expect_test _ =
  pp_match `POST "/product/dyson350?section=2&q=false" ;
  [%expect
    {|
         "Product detail - dyson350. Section: 2. Display questions? false" |}]

let%expect_test _ =
  pp_match `GET "/product/dyson350?section=2&q1=no" ;
  [%expect {|
       None |}]

let%expect_test _ =
  pp_match `GET "/product/dyson350?section=2&q1=yes" ;
  [%expect {|
       "Product detail 2 - dyson350. Section: 2." |}]

let%expect_test _ =
  pp_match `GET "/fruit/apple" ;
  [%expect {|
       "Apples are juicy!" |}]

let%expect_test _ =
  pp_match `GET "/fruit/pineapple" ;
  [%expect {|
       "Pineapple has scaly skin" |}]

let%expect_test _ =
  pp_match `GET "/fruit/orange" ;
  [%expect {|
       "Orange is a citrus fruit." |}]

let%expect_test _ =
  pp_match `GET "/fruit/guava" ;
  [%expect {|
     None |}]

let%expect_test _ =
  pp_match `GET "/" ;
  [%expect {|
     "404 Not found" |}]

let%expect_test _ =
  pp_match `GET "" ;
  [%expect {|
     None |}]

let%expect_test _ =
  pp_match `HEAD "/numbers/23/code/6888/" ;
  [%expect {|
       "int32: 23, int64: 6888." |}]

let%expect_test _ =
  pp_match `HEAD "/numbers/23.01/code/6888/" ;
  [%expect {|
       None |}]

let%expect_test _ =
  pp_match `HEAD "/numbers/23/code/6888.222/" ;
  [%expect {|
       None |}]

let%expect_test _ =
  pp_route ({%wtr| get; /home/about/:bool|} (fun _ -> ())) ;
  [%expect {| GET/home/about/:bool |}]

let%expect_test _ =
  pp_route ({%wtr| post; /home/about/:int/:string/:Fruit|} (fun _ _ _ -> ())) ;
  [%expect {| POST/home/about/:int/:string/:Fruit |}]

let%expect_test _ =
  pp_route ({%wtr| head;/home/:int/:int32/:int64/:Fruit|} (fun _ _ _ _ -> ())) ;
  [%expect {| HEAD/home/:int/:int32/:int64/:Fruit |}]

let%expect_test _ =
  Wtr.pp Format.std_formatter router ;
  [%expect
    {|
    GET
      /home
        /about
          /:int
        /:float
          /
        /:string
          /**
      /contact
        /:string
          /:int
          /:bool
      /product
        /:string
          /section
            /:int
              /q1
                /yes
      /fruit
        /:Fruit
      /
      /public
        /**
    POST
      /home
        /about
          /:int
        /products
          /**
      /product
        /:string
          /section
            /:int
              /q
                /:bool
    HEAD
      /home
        /:int
          /
      /numbers
        /:int32
          /code
            /:int64
              /
    DELETE
      /home
        /:int
          / |}]
