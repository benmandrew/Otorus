(lang dune 3.0)

(name otorus)

(generate_opam_files true)

(source
 (github benmandrew/Otorus))

(authors "Ben Andrew")
(maintainers "Ben Andrew")

(license MIT)

(package
 (name otorus)
 (synopsis "Simple torus renderer")
 (description "Simple torus renderer")
 (depends (ocaml (>= :"4.08")) dune core graphics ppx_inline_test))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
