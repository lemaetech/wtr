open! Otr

let router =
  Private.(
    create
      [ lit "home" (lit "about" nil) >- "about"
      ; (lit "home" (arg int nil) >- fun i -> "int " ^ string_of_int i)
      ; (arg string (arg int nil) >- fun s i -> s ^ string_of_int i)
      ; (lit "home" (arg float nil) >- fun f -> "float " ^ string_of_float f)
      ])

let () =
  assert (Some "float 100001.1" = match' router "/home/100001.1");
  assert (Some "int 100001" = match' router "/home/100001");
  assert (Some "about" = match' router "/home/about")
