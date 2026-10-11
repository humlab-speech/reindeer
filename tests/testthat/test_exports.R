test_that("the public surface is the documented one", {
  skip_if_not(nzchar(system.file(package = "reindeer")))
  ex <- getNamespaceExports("reindeer")
  expect_lte(length(ex), 33)
  gone <- c("serve", "serve_app", "semitones", "erb", "review", "ascend_to", "descend_to",
            "add_metadata", "gather_metadata", "load_metadata", "import_bundle",
            "quantify_egg", "validate_corpus", "inspect_cache", "manage_cache")
  expect_false(any(gone %in% ex))
  expect_true(all(c("corpus", "query", "quantify", "set_metadata", "annotate", "move_to") %in% ex))
})
