(lang dune 3.17)

(name jx)
(version 0.1.0)

(generate_opam_files true)

(license MIT)

(cram enable)

(authors "Roman Sever")

(source
 (uri "https://github.com/romansever/jx"))

(package
 (name jx)
 (synopsis "A better query language for JSON")
 (description
  "jx is a jq replacement with a genuinely better query language. Readable, concise, unambiguous. Not jq-in-another-language — a new language for JSON.")
 (depends
  (ocaml (>= 5.4.0))
  (dune (>= 3.17))
  (cmdliner (>= 1.3.0))
  sedlex
  (yojson (>= 2.0))
  (alcotest :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))))
