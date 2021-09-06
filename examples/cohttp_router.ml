(* Steps to run the demo.

    1. Install dependencies.
    opam install cohttp-lwt-unix

    2. Run the server
    dune exec examples/cohttp.exe
*)

let () =
  let callback _conn req _body =
    let uri = req |> Cohttp_lwt_unix.Request.uri |> Uri.path_and_query in
    let method' =
      Cohttp_lwt_unix.Request.meth req
      |> Cohttp.Code.string_of_method
      |> Wtr.method'
    in
    Wtr.match' method' uri Router.combinator_router
    |> function
    | Some resp ->
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:resp ()
    | None -> Cohttp_lwt_unix.Server.respond_not_found ()
  in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port 5000))
    (Cohttp_lwt_unix.Server.make ~callback ())
  |> Lwt_main.run
