(** [%routes ""] ppx presents an alternate and productive way of specifying a
    {i route} value.

    <code class="language-html"> routes = [http-methods ";" ] http-path
    ["?" http-query]

    http-methods = ; Note comparison are case-insensitive "GET" / "HEAD" /
    "POST" / "PUT" / "DELETE" / "CONNECT" / "OPTIONS" / "TRACE" /
    other-http-method ; Will be converted to "`Method other-http-method" in
    OCaml

    other-http-method = 1*ALPHA

    http-path = "/" wtr-segment

    wtr-segment = wtr-arg / splat / wildcard / [segment-nz *( "/" segment)]
    wtr-arg = ":" 1( "int" / "int32" / "int64" / "float" / "bool" / "string" )
    splat = "**" wildcard = "*" segment = *pchar segment-nz = 1*pchar pchar =
    unreserved / pct-encoded / sub-delims / ":" / "\@" unreserved = ALPHA /
    DIGIT / "-" / "." / "_" / "~" pct-encoded = "%" HEXDIG HEXDIG sub-delims =
    "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="

    http-query = query-key-value *("&" query-key-value) query-key-value =
    query-name "=" query-value query-value = 1*pchar query-name = 1( pchar / "/"
    / "?" ) qchar = unreserved / pct-encoded / qsub-delims / ":" / "\@"
    qsub-delims = "!" / "$" / "'" / "(" / ")" / "*" / "+" / "," / ";"

    ALPHA = %x41-5A / %x61-7A ; A-Z / a-z DIGIT = %x30-39 ; 0-9 HEXDIG = DIGIT /
    "A" / "B" / "C" / "D" / "E" / "F" </code>

    - [%routes] = [http methods separated by comma ; http uri]
    - [http uri = RFC3986 URI syntax for Path and Query with addition of wtr {i arg} syntax]

    A request target/uri in a [%routes] ppx is syntactically and sematically a
    HTTP URI with the addition of {i arg} components and some useful additions
    listed below:

    - {b GET} is the default HTTP method if none is specified.
    - {b Full splat [**]} - Full spat operator matches any/all path following a
      full splat. For example [/home/**] matches the following uri paths,
      [/home/about/, home/contact, /home/product] etc. Full splat must be the
      last component of an uri. [wtr] decodes the remaining matched uri in the
      route handler. For example,

    {[
      let r =
        Wtr.t [{%routes|get; /public/** |} (fun url -> Format.sprintf "%s" url)]
      in
      let s = Wtr.match' `GET "/public/css/style.css" in
      s = Some "css/style.css"
    ]}
    - {b Wildward [*]} - A wildcard operator matches any text appearing on the
      path component position. For example, uri [/home/*/page1] matches the
      following [/home/23/page1, /home/true/page1, /home/234.4/page1] etc. The
      semantics of wildcard operator is the same as using [:string] string in a
      uri.
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

    The custom decoder thus defined can then can be used in [%routes] ppx as
    follows:

    {v [%routes "get; /fruit/:Fruit"] fruit_page v}

    Note the name following the colon [:] must match the name of the module,
    i.e. [:Fruit] in the above example. *)
