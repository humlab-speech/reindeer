# Extracted from test_provenance_safety.R:58

# prequel ----------------------------------------------------------------------
prov_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  list(corp = corp, segs = collect(query(corp, "Phonetic == n")))
}

# test -------------------------------------------------------------------------
skip_if_no_emuR()
corp <- create_isolated_ae_corpus()
out <- collect(scout(query(corp, "Phonetic == n"), 1, collect = FALSE))
expect_true("scout" %in% provenance(out)$verb)
