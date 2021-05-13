let () = Printexc.record_backtrace false

module Fruit = struct
  type t =
    | Apple
    | Orange
    | Pineapple

  let t : t Wtr.decoder =
    Wtr.create_decoder ~name:"Fruit" ~decode:(function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None)
end

let sf = Printf.sprintf

let fruit_page = function
  | Fruit.Apple -> sf "Apples are juicy!"
  | Orange -> sf "Orange is a citrus fruit."
  | Pineapple -> sf "Pineapple has scaly skin"

let router =
  Wtr.(
    create
      [ {%wtr|  /home/about                           |} >- sf "about page"
      ; ({%wtr| /home/:int/                           |}
        >- fun i -> sf "Product Page. Product Id : %d" i)
      ; ({%wtr| /home/:float/                         |}
        >- fun f -> sf "Float page. number : %f" f)
      ; ({%wtr| /contact/*/:int                       |}
        >- fun name number -> sf "Contact page. Hi, %s. Number %i" name number)
      ; {%wtr|  /home/products/**                     |} >- sf "full splat page"
      ; ({%wtr| /home/*/**                            |}
        >- fun s -> sf "Wildcard page. %s" s)
      ; ({%wtr| /contact/:string/:bool                |}
        >- fun name call_me_later ->
        sf "Contact Page2. Name - %s, number - %b" name call_me_later)
      ; ({%wtr| /product/:string?section=:int&q=:bool |}
        >- fun name section_id q ->
        sf "Product detail - %s. Section: %d. Display questions? %b" name
          section_id q)
      ; ({%wtr| /product/:string?section=:int&q1=yes  |}
        >- fun name section_id ->
        sf "Product detail 2 - %s. Section: %d." name section_id)
      ; {%wtr|  /fruit/:Fruit                         |} >- fruit_page
      ; {%wtr|  /                                     |} >- sf "404 Not found"
      ; ({%wtr| /numbers/:int32/code/:int64/          |}
        >- fun id code -> sf "int32: %ld, int64: %Ld." id code)
      ])

let pp_uri p = Wtr.pp_uri Format.std_formatter p

let pp_match uri =
  Wtr.match' router uri
  |> function
  | Some s -> Printf.printf {|"%s%!"|} s
  | None -> Printf.printf "None%!"

let%expect_test _ =
  pp_match "/home/100001.1/";
  [%expect {| "Float page. number : 100001.100000" |}]

let%expect_test _ =
  pp_match "/home/100001.1";
  [%expect {| None |}]

let%expect_test _ =
  pp_match "/home/100001/";
  [%expect {| "Product Page. Product Id : 100001" |}]

let%expect_test _ =
  pp_match "/home/about";
  [%expect {| "about page" |}]

let%expect_test _ =
  pp_match "/home/about/";
  [%expect {| None |}]

let%expect_test _ =
  pp_match "/contact/bikal/123456";
  [%expect {| "Contact page. Hi, bikal. Number 123456" |}]

let%expect_test _ =
  pp_match "/home/products/asdfasdf\nasdfasdfasd";
  [%expect {| "full splat page" |}]

let%expect_test _ =
  pp_match "/home/products/";
  [%expect {| "full splat page" |}]

let%expect_test _ =
  pp_match "/home/product1/";
  [%expect {| "Wildcard page. product1" |}]

let%expect_test _ =
  pp_match "/contact/bikal/true";
  [%expect {| "Contact Page2. Name - bikal, number - true" |}]

let%expect_test _ =
  pp_match "/contact/bob/false";
  [%expect {| "Contact Page2. Name - bob, number - false" |}]

let%expect_test _ =
  pp_match "/product/dyson350?section=233&q=true";
  [%expect
    {| "Product detail - dyson350. Section: 233. Display questions? true" |}]

let%expect_test _ =
  pp_match "/product/dyson350?section=2&q=false";
  [%expect
    {| "Product detail - dyson350. Section: 2. Display questions? false" |}]

let%expect_test _ =
  pp_match "/product/dyson350?section=2&q1=no";
  [%expect {| None |}]

let%expect_test _ =
  pp_match "/product/dyson350?section=2&q1=yes";
  [%expect {| "Product detail 2 - dyson350. Section: 2." |}]

let%expect_test _ =
  pp_match "/fruit/apple";
  [%expect {| "Apples are juicy!" |}]

let%expect_test _ =
  pp_match "/fruit/pineapple";
  [%expect {| "Pineapple has scaly skin" |}]

let%expect_test _ =
  pp_match "/fruit/orange";
  [%expect {| "Orange is a citrus fruit." |}]

let%expect_test _ =
  pp_match "/fruit/guava";
  [%expect {| None |}]

let%expect_test _ =
  pp_match "/";
  [%expect {| "404 Not found" |}]

let%expect_test _ =
  pp_match "";
  [%expect {| None |}]

let%expect_test _ =
  pp_match "/numbers/23/code/6888/";
  [%expect {| "int32: 23, int64: 6888." |}]

let%expect_test _ =
  pp_match "/numbers/23.01/code/6888/";
  [%expect {| None |}]

let%expect_test _ =
  pp_match "/numbers/23/code/6888.222/";
  [%expect {| None |}]

let%expect_test _ =
  pp_uri [%wtr "/home/about/:bool"];
  [%expect {| /home/about/:bool |}]

let%expect_test _ =
  pp_uri [%wtr "/home/about/:int/:string/:Fruit"];
  [%expect {| /home/about/:int/:string/:Fruit |}]

let%expect_test _ =
  pp_uri [%wtr "/home/:int/:int32/:int64/:Fruit"];
  [%expect {| /home/:int/:int32/:int64/:Fruit |}]
