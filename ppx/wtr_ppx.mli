(** {1 Wtr ppx}

    Specifying a Route in a [%wtr] ppx follows the following syntax:

    - [wtr syntax = http methods separated by comma ';' http uri]
    - [uri syntax = http uri]
    - [http uri = HTTP request path syntax]
    - [http uri = HTTP request path syntax ? request query]

    A URI in a [%wtr] ppx is syntactically and sematically a HTTP URI with the
    addition of decoders and some some useful additions listed below:

    - The default HTTP method if none is specified is GET method.
    - {b Full splat [**]} - Full spat operator matches any/all path following a
      full splat. For example [/home/**] matches the following uri paths,
      [/home/about/, home/contact, /home/product] etc. Full splat must be the
      last component of an uri. It is an error to specify other uri path
      component after full splat operator. Additionally [wtr] decodes the
      remaining matched url in the route handler. For example,

    {[
      let r =
        Wtr.t [{%wtr|get; /public/** |} (fun url -> Format.sprintf "%s" url)]
      in
      let s = Wtr.match' `GET "/public/css/style.css" in
      s = Some "css/style.css"
    ]}
    - {b Wildward [*]} - A wildcard operator matches any text appearing on the
      path component position. For example, uri [/home/*/page1] matches the
      following [/home/23/page1, /home/true/page1, /home/234.4/page1] etc. The
      semantics of wildcard operator is the same as using [:string] decoder in a
      uri, i.e. it affects the route handler function signature.
    - {b Trailing slash [/]} - A trailing slash ensures that Wtr will match a
      trailing [/] in a uri. For example, uri [/home/about/] matches
      [/home/about/] but not [/home/about]. *)

(** {2 Custom Decoder}

    The convention for creating a custom decoder that can be used in wtr ppxes
    are as follows:

    + Encapsulated in a module
    + The module defines a type called `t`
    + The module defines a value called [t] which is of type [t Wtr.decoder].

    As an example, we define a custom decoder called [Fruit] and use it in wtr
    ppxes:

    {v
      module Fruit = struct
        type t = Apple | Orange | Pineapple

        let t : t Wtr.decoder =
          Wtr.decoder ~name:"fruit" ~decode:(function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None )
      end
    v}

    The custom decoder thus defined can then can be used in [%wtr] ppx as
    follows:

    {v [%wtr "get; /fruit/:Fruit"] fruit_page v}

    Note the name following the colon [:] must match the name of the module,
    i.e. [:Fruit] in the above example. *)

(** {2 Route Handlers}

    Route handlers are functions that accepts the decoded data from URI. A HTTP
    method, a URI and a route handler makes a {!type:route}. The use of decoders
    \- both built-in and custom - affect the function signature of a route
    handler. For e.g.

    - A uri spec [/home/:int/:bool] expects a route handler as
      [fun (i:int) (b:bool) -> ....]

    - A uri spec [/home/:string] expects a route handler as
      [(fun (s:string) -> ...)] *)
