(* Demonstrates using wtr combinators approach to constructing routes and router. *)

let router =
  Wtr.(
    router'
      [ routes
          [`GET; `POST; `HEAD; `DELETE]
          (exact "home" / exact "about" /. slash)
          Route_handler.about_page
      ; routes [`HEAD; `DELETE]
          (exact "home" / int /. slash)
          Route_handler.prod_page
      ; routes [`GET; `POST]
          (exact "home" / float /. slash)
          Route_handler.float_page
      ; routes [`GET]
          (exact "contact" / string / int /. pend)
          Route_handler.contact_page
      ; routes [`GET]
          (exact "product" / string /? qint "section" /& qbool "q" /?. ())
          Route_handler.product1
      ; routes [`GET]
          ( exact "product"
          / string
          /? qint "section"
          /& qexact ("q1", "yes")
          /?. () )
          Route_handler.product2
      ; routes [`GET]
          (exact "fruit" / parg Route_handler.Fruit.t /. pend)
          Route_handler.fruit_page
      ; routes [`GET] (exact "faq" / int /. splat) Route_handler.faq ])

let () =
  Printexc.record_backtrace true ;
  Format.(fprintf std_formatter "====Routes====@.%a" Wtr.pp router) ;
  Format.(fprintf std_formatter "@.@.====Router Match Results====@.") ;
  [ Wtr.match' `GET "/home/100001.1/" router
  ; Wtr.match' `DELETE "/home/100001/" router
  ; Wtr.match' `GET "/home/about/" router
  ; Wtr.match' `GET "/product/dyson350?section=233&q=true" router
  ; Wtr.match' `GET "/product/dyson350?section=2&q=false" router
  ; Wtr.match' `GET "/product/dyson350?section=2&q1=yes" router
  ; Wtr.match' `GET "/product/dyson350?section=2&q1=no" router
  ; Wtr.match' `GET "/fruit/apple" router
  ; Wtr.match' `GET "/fruit/orange" router
  ; Wtr.match' `GET "/fruit/pineapple" router
  ; Wtr.match' `GET "/fruit/guava" router
  ; Wtr.match' `GET "/faq/1/" router
  ; Wtr.match' `GET "/faq/1/whatever" router
  ; Wtr.match' `GET "/faq/2/whateasdfasdfasdf" router ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
       | None -> Printf.printf "%3d: None\n" (i + 1) )

(** Should output below:

    {v
====Routes====
GET
  /home
    /about
      /
    /:float
      /
  /contact
    /:string
      /:int
  /product
    /:string
      ?section=:int
        &q=:bool
        &q1=yes
  /fruit
    /:Fruit
  /faq
    /:int
      /**
POST
  /home
    /about
      /
    /:float
      /
HEAD
  /home
    /about
      /
    /:int
      /
DELETE
  /home
    /about
      /
    /:int
      /

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
 14: FAQ page for category : insurance
    v} *)
