(* Steps to run the demo.

    1. Install dependencies.
    opam install cohttp-lwt-unix

    2. Run the server
    dune exec examples/cohttp.exe
*)

open Route_handler

(* create a router *)
let router =
  Wtr.router'
    [ {%routes| get,post; /home/about                     |} about_page
    ; {%routes| get; /home/:int/                          |} prod_page
    ; {%routes| get;/home/:float/                         |} float_page
    ; {%routes| get;/contact/*/:int                       |} contact_page
    ; {%routes| get;/product/:string?section=:int&q=:bool |} product1
    ; {%routes| get;/product/:string?section=:int&q1=yes  |} product2
    ; {%routes| get;/fruit/:Fruit                         |} fruit_page
    ; {%routes| get;/faq/:int/**                          |} faq ]

let () =
  let callback _conn req _body =
    let uri = req |> Cohttp_lwt_unix.Request.uri |> Uri.path_and_query in
    let method' =
      Cohttp_lwt_unix.Request.meth req
      |> Cohttp.Code.string_of_method
      |> Wtr.method'
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
