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
 (synopsis "Parallelised torus ray-tracer")
 (description "Parallelised torus ray-tracer")
 (depends (ocaml (>= :"5.00")) dune core graphics (domainslib (>= :"0.5.0")) ppx_inline_test))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
