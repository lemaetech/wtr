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
