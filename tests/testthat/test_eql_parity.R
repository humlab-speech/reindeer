# Parity with emuR::query() on `ae`. Known failures are listed in
# `known_failures` with the work package (dev/REINDEER_AMENDMENT_PLAN_2.2.0.md)
# that fixes them; they are skipped, not dropped.
known_failures <- list(
  "[[#Phonetic == n -> Phonetic == t] ^ Syllable == W]" = "A6/A7: WP3.3-3.4",
  "[[Syllable == S -> Syllable == W] ^ Phonetic == @]" = "A6/A7: WP3.3-3.4"
)

test_that("EQL results match emuR (unsorted, 1e-9)", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  for (q in names(eql_golden())) {
    if (!is.null(known_failures[[q]])) next
    expect_eql_parity(corp, q)
  }
})

for (q in names(known_failures)) {
  local({
    q <- q
    test_that(paste("parity:", q), {
      skip(paste0(known_failures[[q]], ": ", q))
    })
  })
}
