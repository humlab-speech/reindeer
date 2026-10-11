# CRAN: non-goal for now (decision 2026-10-11)

Blockers, in order of cost:

1. `Remotes: github::humlab-speech/superassp`. superassp supplies every DSP
   routine `quantify()` runs; CRAN rejects packages whose dependencies are not
   on CRAN or Bioconductor (it is in Suggests, but the examples, vignettes and
   most tests need it).
2. Suggests on a private repository (`eggstract`) and on packages that are not
   needed for CRAN checks.
3. Tarball is 6.4 MB (limit 5 MB): the vendored Artic dist is 3.4 MB.
   A download-on-first-`annotate()` dist would fix it; not worth building
   while (1) stands.

`http://` strings in `R/reindeer_cmdi.R` and `R/interop_elan.R` are XML
namespace identifiers and must stay `http`.

Revisit when superassp is on CRAN. Until then the supported install is
`remotes::install_github("humlab-speech/reindeer")`, and `R CMD check
--as-cran` is kept clean apart from the Remotes NOTE.
