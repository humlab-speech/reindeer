# Extracted from test_lazy_fork.R:55

# prequel ----------------------------------------------------------------------
marker_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE,
                       verbose = FALSE, ...) {
  data.frame(marker = 7)
}
lazy_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  list(corp = corp, lz = query(corp, "Phonetic == t"))
}

# test -------------------------------------------------------------------------
skip_if_no_emuR()
fx <- lazy_fixture()
n_in <- nrow(collect(fx$lz))
calls <- new.env()
calls$n <- 0L
counting <- function(listOfFiles, beginTime, endTime, toFile = FALSE,
                       verbose = FALSE, ...) {
    calls$n <- calls$n + 1L
    data.frame(marker = 1)
  }
out <- collect(scout(quantify(fx$lz, counting), 1, collect = FALSE))
expect_equal(calls$n, n_in)
