test_that("erb() and semitones() warn and equal the superassp values", {
  skip_if_no_superassp()
  expect_warning(e <- erb(c(500, 1000, 2000)), class = "reindeer_deprecated")
  expect_equal(e, c(10.29531, 15.29265, 20.93034), tolerance = 1e-5)
  expect_warning(s <- semitones(c(200, 400), ref = 100), class = "reindeer_deprecated")
  expect_equal(s, c(12, 24))
})
