# End-to-end tests for the persistent quantify cache.
#
# Written because no test in the suite exercised `.use_cache` through the public
# API: the executor for 21-100 segments took no cache arguments at all (E2), and
# the cache key did not carry the DSP function identity (X7). Both defects were
# invisible to the suite and are pinned here.

# Two DSP stubs with identical formals but different output. Identical formals
# matter: derived parameters then hash the same, so any two functions collide
# unless the key carries the function's identity.
probe_dsp <- function(value) {
  force(value)
  function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(probe = rep(value, length(listOfFiles)))
  }
}

cache_fixture <- function() {
  skip_if_no_emuR()
  skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))
  list(corp = corp, segs = segs)
}

test_that("the cache is honoured in every size band", {
  # PENDING WP1.6 - red until cache-enabled calls stop bypassing the executor
  # that implements caching for 21-100 segment inputs.

  skip("PENDING WP1.6 - see dev/REINDEER_REMEDIATION_PLAN.md")
  fx <- cache_fixture()
  dsp <- probe_dsp(1)

  for (n in c(5L, 50L, 150L)) {
    segs_n <- fx$segs[seq_len(n), ]
    cold <- quantify(segs_n, dsp, .use_cache = TRUE,
                     .parallel = FALSE, .verbose = FALSE)
    expect_true(".cache_status" %in% names(cold),
                info = paste0("no cache accounting at n = ", n))
    expect_true(all(cold$.cache_status == "miss"),
                info = paste0("cold call reported hits at n = ", n))

    warm <- quantify(segs_n, dsp, .use_cache = TRUE,
                     .parallel = FALSE, .verbose = FALSE)
    expect_true(all(warm$.cache_status == "hit"),
                info = paste0("warm call missed at n = ", n))
  }
})

test_that("cache keys distinguish DSP functions", {
  # PENDING WP1.5 - red until the key carries the function identity.

  skip("PENDING WP1.5 - see dev/REINDEER_REMEDIATION_PLAN.md")
  fx <- cache_fixture()
  segs_n <- fx$segs[seq_len(150L), ]

  first <- quantify(segs_n, probe_dsp(1), .use_cache = TRUE,
                    .parallel = FALSE, .verbose = FALSE)
  expect_equal(unique(first$probe), 1)

  second <- quantify(segs_n, probe_dsp(2), .use_cache = TRUE,
                     .parallel = FALSE, .verbose = FALSE)
  expect_equal(unique(second$probe), 2)
})

test_that("changing speaker metadata invalidates only the affected rows", {
  skip("PENDING WP1.6 - see dev/REINDEER_REMEDIATION_PLAN.md")
  fx <- cache_fixture()
  dsp <- probe_dsp(1)
  segs_n <- fx$segs[seq_len(150L), ]

  quantify(segs_n, dsp, .use_cache = TRUE, .parallel = FALSE, .verbose = FALSE)

  bndl <- fx$segs$bundle[1]
  sess <- fx$segs$session[1]
  set_metadata(fx$corp, list(Age = 7, Gender = "Male"),
               session = sess, bundle = bndl)

  after <- quantify(segs_n, dsp, .use_cache = TRUE,
                    .parallel = FALSE, .verbose = FALSE)
  affected <- after$session == sess & after$bundle == bndl
  expect_true(any(after$.cache_status[affected] == "miss"))
  expect_true(all(after$.cache_status[!affected] == "hit"))
})
