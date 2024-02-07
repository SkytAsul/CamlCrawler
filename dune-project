(lang dune 3.13)

(name CamlCrawler)

(generate_opam_files true)

(source
 (github SkytAsul/CamlCrawler))

(authors "SkytAsul")

(maintainers "SkytAsul")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name CamlCrawler)
 (synopsis "Small textual adventure game in functional programming")
 (description
 "A small textual game made for school and to improve myself in functional programming.
 Also contains whole modular interactive terminal interface, along with Markdown to Notty translator.
 ")
 (depends ocaml dune notty omd yaml)
 (tags
  (adventure console textual)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
