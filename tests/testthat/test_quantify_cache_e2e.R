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
  fx <- cache_fixture()
  dsp <- probe_dsp(1)

  for (n in c(5L, 50L, 150L)) {
    segs_n <- fx$segs[seq_len(n), ]
    first <- quantify(segs_n, dsp, .use_cache = TRUE,
                      .parallel = FALSE, .verbose = FALSE)
    expect_true(".cache_status" %in% names(first),
                info = paste0("no cache accounting at n = ", n))
    expect_true(any(first$.cache_status == "miss"),
                info = paste0("nothing was computed at n = ", n))

    warm <- quantify(segs_n, dsp, .use_cache = TRUE,
                     .parallel = FALSE, .verbose = FALSE)
    expect_true(all(warm$.cache_status == "hit"),
                info = paste0("warm call missed at n = ", n))
  }
})

test_that("cache keys distinguish DSP functions", {
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
  fx <- cache_fixture()
  segs_n <- fx$segs[seq_len(150L), ]

  # A routine whose formals expose the norm parameters, so Age/Gender change the
  # derived DSP parameters and therefore the cache key. superassp >= 3.0.0
  # exposes these on its trk_* wrappers; older builds expose only (listOfFiles,
  # ...) and cannot receive norms at all.
  norm_dsp <- function(listOfFiles, beginTime, endTime, nominalF1 = 500,
                       toFile = FALSE, verbose = FALSE, ...) {
    data.frame(probe = rep(nominalF1, length(listOfFiles)))
  }

  first <- quantify(segs_n, norm_dsp, .use_cache = TRUE,
                    .parallel = FALSE, .verbose = FALSE)

  bndl <- fx$segs$bundle[1]
  sess <- fx$segs$session[1]
  set_metadata(fx$corp, list(Age = 7, Gender = "Male"),
               session = sess, bundle = bndl)

  after <- quantify(segs_n, norm_dsp, .use_cache = TRUE,
                    .parallel = FALSE, .verbose = FALSE)

  affected <- after$session == sess & after$bundle == bndl
  expect_true(any(affected))
  expect_true(all(after$.cache_status[affected] == "miss"),
              info = "edited bundle kept its cached rows")
  expect_true(all(after$.cache_status[!affected] == "hit"),
              info = "untouched bundles were recomputed")
  expect_false(identical(unique(after$probe[affected]),
                         unique(first$probe[affected])))
})
