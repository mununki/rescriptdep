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
 (synopsis "A dependency analyzer for ReScript modules")
 (description
  "A tool to analyze dependencies between ReScript modules using cmi files")
 (depends
  (ocaml
   (= 4.14.0))
  (dune
   (= 3.16.0))
  (odoc :with-doc))
 (tags
  (topics "dependency analysis" "ReScript" "OCaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
