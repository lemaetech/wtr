(lang dune 2.8)

(name wtr)

(version 3.0.0)

(using mdx 0.1)

(generate_opam_files false)

(implicit_transitive_deps true)

(source
 (github lemaetech/wtr))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name wtr)
 (synopsis "Well Typed Router")
 (description "Wtr - A typed router for OCaml web applications")
 (depends
  (ocaml
   (>= 4.12.0))
  (uri
   (>= 4.2.0))
  (ppx_expect :with-test)
  (mdx :with-test)))

(package
 (name wtr-ppx)
 (synopsis "Ppx to create routers")
 (description
  "wtr-ppx provides a ppx driven alternative to Wtr router\n              construction.")
 (depends
  (ocaml
   (>= 4.12.0))
  (uri
   (>= 4.2.0))
  (wtr
   (= :version))
  (ppxlib
   (>= 0.22.0))
  (ppx_expect :with-test)
  (mdx :with-test)))
