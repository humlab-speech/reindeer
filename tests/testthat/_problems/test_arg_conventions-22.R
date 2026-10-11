# Extracted from test_arg_conventions.R:22

# test -------------------------------------------------------------------------
expect_equal(reindeer:::.artic_args(list(auto_open_url = "", use_viewer = FALSE), "annotate"),
               list(autoOpenURL = "", useViewer = FALSE))
expect_warning(
    a <- reindeer:::.artic_args(list(autoOpenURL = ""), "annotate"),
    class = "reindeer_deprecated")
expect_equal(a, list(autoOpenURL = ""))
expect_error(reindeer:::.artic_args(list(bogus = 1), "annotate"), "Unknown argument")
