(lang dune 3.16)

(name rescriptdep)

(generate_opam_files true)

(source
 (github mununki/rescriptdep))

(authors "Woonki Moon")

(maintainers "Woonki Moon")

(license MIT)

(documentation https://github.com/mununki/rescriptdep)

(package
 (name rescriptdep)
 (version 0.1.0)
 (synopsis "A dependency analyzer for ReScript modules")
 (description
  "A tool to analyze dependencies between ReScript modules using cmi files")
 (depends
  (ocaml
   (= 5.3.0))
  dune
  eio
  eio_main
  (odoc :with-doc))
 (tags
  (topics "dependency analysis" "ReScript" "OCaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
