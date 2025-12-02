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
  (bitv
   (>= 2.1))
  (dyn
   (>= 3.20))
  (ordering
   (>= 3.20))
  (pp
   (>= 2.0.0))))

(package
 (name catch-the-bunny-tests)
 (synopsis "Tests for catch-the-bunny")
 (depends
  (ocaml
   (>= 5.3))
  (catch-the-bunny
   (= :version))
  (ppx_expect
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
   (= 0.28.1))
  (bisect_ppx
   (>= 2.8.3))
  (catch-the-bunny
   (= :version))
  (catch-the-bunny-tests
   (= :version))
  (ppx_expect
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (ppxlib
   (>= 0.35.0))
  (sherlodoc
   (>= 0.2))))
