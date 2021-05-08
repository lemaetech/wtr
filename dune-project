(lang dune 2.8)

(name otr)

(version 1.0.0)

(generate_opam_files true)

(implicit_transitive_deps true)

(source
 (github lemaetech/otr))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name otr)
 (synopsis "A typed router for OCaml web applications.")
 (description "A typed router for OCaml web applications.")
 (depends
  (ocaml
   (>= 4.11.1))
  ppxlib))
