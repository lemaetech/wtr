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

let router =
  Wtr.create
    [ {%wtr| get,post;            /home/about/:int  |} (fun _ -> "about page")
    ; {%wtr| head,delete;         /home/:int/       |} (fun i ->
          Printf.sprintf "Product Page. Product Id : %d" i )
    ; {%wtr| head,delete;         /home/about/      |} "head,delete about page"
    ; {%wtr| /home/:float/                          |} (fun f ->
          Printf.sprintf "Float page. number : %f" f )
    ; {%wtr| /contact/*/:int                        |} (fun name number ->
          Printf.sprintf "Contact page. Hi, %s. Number %i" name number )
    ; {%wtr| /home/products/**                      |} "full splat page"
    ; {%wtr| /home/*/**                             |} (fun s ->
          Printf.sprintf "Wildcard page. %s" s )
    ; {%wtr| /contact/:string/:bool                 |}
        (fun name call_me_later ->
          Printf.sprintf "Contact Page2. Name - %s, number - %b" name
            call_me_later )
    ; {%wtr| /product/:string?section=:int&q=:bool  |} (fun name section_id q ->
          Printf.sprintf
            "Product detail - %s. Section: %d. Display questions? %b" name
            section_id q )
    ; {%wtr| /product/:string?section=:int&q1=yes   |} (fun name section_id ->
          Printf.sprintf "Product detail 2 - %s. Section: %d." name section_id )
    ; {%wtr| /fruit/:Fruit                          |} fruit_page
    ; {%wtr| /                                      |} "404 Not found"
    ; {%wtr| /numbers/:int32/code/:int64/           |} (fun id code ->
          Printf.sprintf "int32: %ld, int64: %Ld." id code ) ]

let pp_route r = List.hd r |> Wtr.pp_route Format.std_formatter

let%expect_test _ =
  Wtr.pp Format.std_formatter router ;
  [%expect
    {|
    GET
      /home
        /about
          /:int

    POST
      /home
        /about
          /:int

    HEAD
      /home
        /:int
          /

        /about
          /

    DELETE
      /home
        /:int
          /

        /about
          /

    /home
      /:float
        /

      /products
        /**

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
            /q
              /:bool

            /q1
              /yes

    /fruit
      /:Fruit

    /

    /numbers
      /:int32
        /code
          /:int64
            / |}]

let pp_match ?method' uri =
  Wtr.match' ?method' router uri
  |> function
  | Some s -> Printf.printf {|"%s%!"|} s | None -> Printf.printf "None%!"

(* let%expect_test _ = *)
(*   pp_match "/home/100001.1/" ; *)
(* [%expect {| *)
   (*     "Float page. number : 100001.100000" |}] *)

(* let%expect_test _ = pp_match "/home/100001.1" ; [%expect {| *)
   (*   None |}] *)

let%expect_test _ =
  pp_match ~method':`HEAD "/home/100001/" ;
  [%expect {|
    /:int32
      /code
        /:int64
          /



    /:Fruit


    /:string
      /section
        /:int
          /q
            /:bool

          /q1
            /yes


    /:string
      /:int

      /:bool


    /:float
      /

    /products
      /**

    /:string
      /**


    "Wildcard page. home"/home
      /:int
        /

      /about
        / |}]

(* let%expect_test "about route" = pp_match "/home/about" ; [%expect {| *)
   (*   None |}] *)

(* let%expect_test "about route" = *)
(*   pp_match ~method':`GET "/home/about/2" ; *)
(*   [%expect {| "Wildcard page. home" |}] *)

(* let%expect_test "about route" = *)
(*   pp_match ~method':`POST "/home/about/3" ; *)
(*   [%expect {| "Wildcard page. home" |}] *)

(* let%expect_test "about route" = *)
(*   pp_match ~method':`HEAD "/home/about/3" ; *)
(*   [%expect {| "Wildcard page. home" |}] *)

(* let%expect_test "about route" = *)
(*   pp_match ~method':`HEAD "/home/about/" ; *)
(*   [%expect {| "Wildcard page. home" |}] *)

(* let%expect_test "about route" = *)
(*   pp_match ~method':`DELETE "/home/about/" ; *)
(*   [%expect {| "Wildcard page. home" |}] *)

(* let%expect_test "about_route2" = *)
(*   pp_match ~method':`GET "/home/about/" ; *)
(*   [%expect {| "Wildcard page. home" |}] *)

(* let%expect_test "about_route2" = *)
(* pp_match "/home/about/" ; [%expect {| *)
   (*     "Wildcard page. about" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/contact/bikal/123456" ; *)
(* [%expect {| *)
   (*     "Contact page. Hi, bikal. Number 123456" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/home/products/asdfasdf\nasdfasdfasd" ; *)
(* [%expect {| *)
   (*     "full splat page" |}] *)

(* let%expect_test _ = *)
(* pp_match "/home/products/" ; [%expect {| *)
   (*     "full splat page" |}] *)

(* let%expect_test _ = *)
(* pp_match "/home/product1/" ; [%expect {| *)
   (*     "Wildcard page. product1" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/contact/bikal/true" ; *)
(* [%expect {| *)
   (*     "Contact Page2. Name - bikal, number - true" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/contact/bob/false" ; *)
(* [%expect {| *)
   (*     "Contact Page2. Name - bob, number - false" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/product/dyson350?section=233&q=true" ; *)
(*   [%expect *)
(* {| *)
   (*       "Product detail - dyson350. Section: 233. Display questions? true" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/product/dyson350?section=2&q=false" ; *)
(*   [%expect *)
(* {| *)
   (*       "Product detail - dyson350. Section: 2. Display questions? false" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/product/dyson350?section=2&q1=no" ; *)
(* [%expect {| *)
   (*     None |}] *)

(* let%expect_test _ = *)
(*   pp_match "/product/dyson350?section=2&q1=yes" ; *)
(* [%expect {| *)
   (*     "Product detail 2 - dyson350. Section: 2." |}] *)

(* let%expect_test _ = *)
(* pp_match "/fruit/apple" ; [%expect {| *)
   (*     "Apples are juicy!" |}] *)

(* let%expect_test _ = *)
(*   pp_match "/fruit/pineapple" ; *)
(* [%expect {| *)
   (*     "Pineapple has scaly skin" |}] *)

(* let%expect_test _ = *)
(* pp_match "/fruit/orange" ; [%expect {| *)
   (*     "Orange is a citrus fruit." |}] *)

(* let%expect_test _ = pp_match "/fruit/guava" ; [%expect {| *)
   (*   None |}] *)
(* let%expect_test _ = pp_match "/" ; [%expect {| *)
   (*   "404 Not found" |}] *)
(* let%expect_test _ = pp_match "" ; [%expect {| *)
   (*   None |}] *)

(* let%expect_test _ = *)
(*   pp_match "/numbers/23/code/6888/" ; *)
(* [%expect {| *)
   (*     "int32: 23, int64: 6888." |}] *)

(* let%expect_test _ = *)
(*   pp_match "/numbers/23.01/code/6888/" ; *)
(* [%expect {| *)
   (*     None |}] *)

(* let%expect_test _ = *)
(*   pp_match "/numbers/23/code/6888.222/" ; *)
(* [%expect {| *)
   (*     None |}] *)

(* let%expect_test _ = *)
(*   pp_route ([%wtr "/home/about/:bool"] (fun _ -> ())) ; *)
(*   [%expect {| /home/about/:bool |}] *)

(* let%expect_test _ = *)
(*   pp_route ([%wtr "/home/about/:int/:string/:Fruit"] (fun _ _ _ -> ())) ; *)
(*   [%expect {| /home/about/:int/:string/:Fruit |}] *)

(* let%expect_test _ = *)
(*   pp_route ([%wtr "/home/:int/:int32/:int64/:Fruit"] (fun _ _ _ _ -> ())) ; *)
(*   [%expect {| /home/:int/:int32/:int64/:Fruit |}] *)
