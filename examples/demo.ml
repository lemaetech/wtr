let router =
  Otr.(
    create
      [ {%otr|/home/about|} >- "about page"
      ; ([%otr "/home/:int/"]
        >- fun i -> "Product Page. Product Id : " ^ string_of_int i)
      ; ([%otr "/home/:float/"]
        >- fun f -> "Float page. number : " ^ string_of_float f)
      ; ([%otr "/contact/*/:int"]
        >- fun name number ->
        "Contact page. Hi, " ^ name ^ ". Number " ^ string_of_int number)
      ])

let () =
  List.iteri
    (fun i -> function
      | Some s -> Printf.printf "%d: %s\n" (i + 1) s
      | None -> Printf.printf "%d: None\n" (i + 1))
    [ Otr.match' router "/home/100001.1/"
    ; Otr.match' router "/home/100001/"
    ; Otr.match' router "/home/about"
    ]
