# Property: for every bundle, a field resolves to bundle > session > database,
# and to NA when no level sets it, whichever levels were written (F3.4).
test_that("metadata precedence holds for random writes at the three levels", {
  set.seed(42)
  for (round in 1:4) {
    corp <- create_isolated_ae_corpus()
    b <- get_metadata(corp)
    sess <- unique(b$session)
    expected <- stats::setNames(rep(NA_real_, nrow(b)), paste(b$session, b$bundle))
    db_v <- if (runif(1) < 0.7) round * 1000 else NA_real_
    if (!is.na(db_v)) {
      set_metadata(corp, list(Prop = db_v))
      expected[] <- db_v
    }
    for (s in sess[runif(length(sess)) < 0.6]) {
      v <- round * 100 + match(s, sess)
      set_metadata(corp, list(Prop = v), session = s)
      expected[b$session == s] <- v
    }
    for (i in which(runif(nrow(b)) < 0.3)) {
      v <- round * 10 + i
      set_metadata(corp, list(Prop = v), session = b$session[i], bundle = b$bundle[i])
      expected[i] <- v
    }
    got <- get_metadata(corp)
    got <- stats::setNames(if ("Prop" %in% names(got)) as.numeric(got$Prop) else rep(NA_real_, nrow(got)),
                           paste(got$session, got$bundle))
    expect_equal(got[names(expected)], expected, ignore_attr = TRUE,
                 info = paste("round", round))
  }
})
