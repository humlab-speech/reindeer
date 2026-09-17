# Metadata values must survive the round trip through the SQLite cache.
#
# B1: nested values (project / funding / team) were flattened to an unnamed
# character vector by serialize_metadata_value(), then recycled into a length-1
# slot, emitting "number of items to replace is not a multiple of replacement
# length" and silently keeping only the first element. METADATA.json was
# unaffected, so the damage was confined to the cache - and to everything that
# reads through get_metadata().

test_that("nested metadata values survive the cache round trip", {
  skip("PENDING WP1.7 - see dev/REINDEER_REMEDIATION_PLAN.md")
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()

  expect_silent(
    add_metadata(corp, list(
      project = list(name = "TestProj", description = "A test corpus.",
                     startDate = "2025-01-15"),
      funding = list(funder = "TestFunder", grantNumber = "G-001")
    ))
  )

  md <- get_metadata(corp)
  expect_true(all(c("project", "funding") %in% names(md)))

  project <- md$project[[1]]
  expect_equal(project$name, "TestProj")
  expect_equal(project$description, "A test corpus.")
  expect_equal(project$startDate, "2025-01-15")

  funding <- md$funding[[1]]
  expect_equal(funding$funder, "TestFunder")
  expect_equal(funding$grantNumber, "G-001")
})

test_that("scalar and vector metadata still round-trip", {
  skip("PENDING WP1.7 - see dev/REINDEER_REMEDIATION_PLAN.md")
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()

  add_metadata(corp, list(
    Project = "ScalarProj",
    Year = 2026,
    Verified = TRUE,
    Tags = c("alpha", "beta")
  ))

  md <- get_metadata(corp)
  expect_equal(md$Project[1], "ScalarProj")
  expect_equal(md$Year[1], 2026)
  expect_true(md$Verified[1])
  expect_equal(md$Tags[[1]], c("alpha", "beta"))
})
