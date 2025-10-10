(lang dune 3.20)

(name catch-the-bunny)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/catch-the-bunny))

(documentation "https://mbarbin.github.io/catch-the-bunny/")

(implicit_transitive_deps false)

(package
 (name catch-the-bunny)
 (synopsis "Resolving a small logic puzzle to catch a bunny")
 (depends
  (ocaml
   (>= 5.3))
  (base
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))))

(package
 (name catch-the-bunny-tests)
 (synopsis "Tests for catch-the-bunny")
 (depends
  (ocaml
   (>= 5.3))
  (base
   (>= v0.17))
  (catch-the-bunny
   (= :version))
  (expect_test_helpers_core
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))))

(package
 (name catch-the-bunny-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.3))
  (ocamlformat
   (= 0.27.0))
  (base
   (>= v0.17))
  (bisect_ppx
   (>= 2.8.3))
  (catch-the-bunny
   (= :version))
  (catch-the-bunny-tests
   (= :version))
  (expect_test_helpers_core
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))
  (sherlodoc
   (>= 0.2))))
