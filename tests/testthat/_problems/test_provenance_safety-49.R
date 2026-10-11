# Extracted from test_provenance_safety.R:49

# prequel ----------------------------------------------------------------------
prov_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  list(corp = corp, segs = collect(query(corp, "Phonetic == n")))
}

# test -------------------------------------------------------------------------
skip_if_no_emuR()
fx <- prov_fixture()
both <- dplyr::bind_rows(fx$segs, fx$segs)
