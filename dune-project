(lang dune 3.17)

(name jbq)
(version 0.1.0)

(generate_opam_files true)

(license MIT)

(cram enable)

(authors "Roman Sever")

(source
 (uri "https://github.com/volh/jbq"))

(package
 (name jbq)
 (synopsis "JSON query and schema inference CLI")
 (description
  "jbq (JSON Bourne Query) is a JSON query CLI with a compact, explicit language for filtering, reshaping, inspecting JSON, and inferring JSON Schema.")
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
