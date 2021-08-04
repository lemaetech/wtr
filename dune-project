(lang dune 2.8)

(name wtr)

(version 1.0.0)

(using mdx 0.1)

(generate_opam_files true)

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
  (pprint
   (>= 20200410))
  (ppxlib
   (>= 0.22.0))
  (ppx_expect :with-test)
  (mdx :with-test)))
