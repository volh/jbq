(lang dune 3.17)

(name jbq)
(version 0.1.0)

(generate_opam_files true)

(license MIT)

(cram enable)

(authors "Roman Sever")

(source
 (uri "https://github.com/romansever/jbq"))

(package
 (name jbq)
 (synopsis "A better query language for JSON")
 (description
  "jbq (JSON Bourne Query) is a jq replacement with a genuinely better query language. Readable, concise, unambiguous. Not jq-in-another-language — a new language for JSON.")
 (depends
  (ocaml (>= 5.4.0))
  (dune (>= 3.17))
  (cmdliner (>= 1.3.0))
  sedlex
  zarith
  (alcotest :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))))
