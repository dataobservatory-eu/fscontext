# Controlled mutation at the signature stage (no timing-dependent races).
# Requires testthat local_mocked_bindings(), available in recent testthat.

race_fixture <- function() {
  root <- tempfile("fscontext_race_")
  dir.create(root, recursive = TRUE)
  root
}

race_scan <- function(root) {
  suppressMessages(scan_storage(
    root,
    storage_id = "race-store", person_id = "tester",
    compute_signature = TRUE
  ))
}

race_mock_available <- function() {
  testthat::skip_if_not(
    exists("local_mocked_bindings",
      envir = asNamespace("testthat"),
      inherits = FALSE
    ),
    "testthat::local_mocked_bindings() unavailable"
  )
}

test_that("signature error is represented as missing signature, not scan failure", {
  race_mock_available()
  root <- race_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("data", file.path(root, "item.txt"))
  testthat::local_mocked_bindings(
    quick_signature = function(p) stop("simulated transient read error"),
    .package = "fscontext"
  )
  snap <- race_scan(root)
  expect_equal(nrow(snap), 1L)
  expect_true(is.na(snap$quick_sig[[1]]))
  expect_identical(snap$rel_path[[1]], "item.txt")
})

test_that("deletion after metadata collection leaves an observed row", {
  race_mock_available()
  root <- race_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "vanishing.txt")
  writeLines("data", file)
  called <- FALSE
  testthat::local_mocked_bindings(
    quick_signature = function(p) {
      called <<- TRUE
      unlink(p)
      stop("file disappeared before signature")
    },
    .package = "fscontext"
  )
  snap <- race_scan(root)
  expect_true(called)
  expect_false(file.exists(file))
  expect_equal(nrow(snap), 1L)
  expect_identical(snap$rel_path[[1]], "vanishing.txt")
  expect_true(is.na(snap$quick_sig[[1]]))
})

test_that("file modified after metadata collection retains earlier size", {
  race_mock_available()
  root <- race_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "changing.txt")
  writeBin(charToRaw("a"), file)
  testthat::local_mocked_bindings(
    quick_signature = function(p) {
      writeBin(charToRaw("much longer"), p)
      "simulated-signature"
    },
    .package = "fscontext"
  )
  snap <- race_scan(root)
  expect_equal(nrow(snap), 1L)
  expect_equal(snap$size[[1]], 1)
  expect_equal(as.numeric(file.info(file)$size), 11)
  expect_identical(snap$quick_sig[[1]], "simulated-signature")
})

test_that("signature size threshold avoids reading oversized files", {
  race_mock_available()
  root <- race_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("long content", file.path(root, "large.txt"))
  called <- FALSE
  testthat::local_mocked_bindings(
    quick_signature = function(p) {
      called <<- TRUE
      "unexpected"
    },
    .package = "fscontext"
  )
  snap <- suppressMessages(scan_storage(
    root,
    storage_id = "race-store", person_id = "tester",
    compute_signature = TRUE, max_signature_size = 0
  ))
  expect_equal(nrow(snap), 1L)
  expect_false(called)
  expect_true(is.na(snap$quick_sig[[1]]))
})
