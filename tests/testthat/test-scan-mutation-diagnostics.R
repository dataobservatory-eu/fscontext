# Diagnostic tests for changes across sequential filesystem scans.
# These are deterministic before/after tests, NOT concurrent-race simulations.

mutation_root <- function() {
  root <- tempfile("fscontext_mutation_")
  dir.create(root, recursive = TRUE)
  root
}
mutation_scan <- function(root, at) {
  suppressMessages(scan_storage(
    root,
    storage_id = "mutation-store", person_id = "tester",
    scan_time = at, compute_signature = FALSE
  ))
}

test_that("a file added between scans appears only in the later observation", {
  root <- mutation_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("first", file.path(root, "first.txt"))
  t1 <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  before <- mutation_scan(root, t1)
  writeLines("second", file.path(root, "second.txt"))
  after <- mutation_scan(root, t1 + 60)
  expect_setequal(before$rel_path, "first.txt")
  expect_setequal(after$rel_path, c("first.txt", "second.txt"))
  expect_equal(nrow(before), 1L)
  expect_equal(nrow(after), 2L)
})

test_that("a file removed between scans remains in the earlier snapshot", {
  root <- mutation_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "gone.txt")
  writeLines("present", file)
  before <- mutation_scan(root, Sys.time())
  expect_true(file.remove(file))
  after <- mutation_scan(root, Sys.time() + 60)
  expect_true("gone.txt" %in% before$rel_path)
  expect_false("gone.txt" %in% after$rel_path)
  expect_equal(nrow(after), 0L)
})

test_that("rename between scans changes path identity without rewriting old snapshot", {
  root <- mutation_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  old <- file.path(root, "old.txt")
  new <- file.path(root, "new.txt")
  writeLines("same bytes", old)
  before <- mutation_scan(root, Sys.time())
  expect_true(file.rename(old, new))
  after <- mutation_scan(root, Sys.time() + 60)
  expect_identical(before$rel_path, "old.txt")
  expect_identical(after$rel_path, "new.txt")
  expect_false(identical(before$storage_path_id, after$storage_path_id))
})

test_that("changed size is captured in the later observation only", {
  root <- mutation_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "edited.txt")
  writeBin(charToRaw("a"), file)
  before <- mutation_scan(root, Sys.time())
  writeBin(charToRaw("a much longer file"), file)
  after <- mutation_scan(root, Sys.time() + 60)
  expect_equal(before$size, 1)
  expect_equal(after$size, 18)
  expect_identical(before$storage_path_id, after$storage_path_id)
})

test_that("empty directory scans produce a zero-row snapshot", {
  root <- mutation_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  snap <- mutation_scan(root, Sys.time())
  expect_s3_class(snap, "data.frame")
  expect_equal(nrow(snap), 0L)
  expect_true(all(c("full_path", "rel_path", "size") %in% names(snap)))
})

test_that("scan metadata records supplied scan time and root", {
  root <- mutation_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("x", file.path(root, "item.txt"))
  at <- as.POSIXct("2026-11-19 12:34:56", tz = "UTC")
  snap <- mutation_scan(root, at)
  expect_equal(attr(snap, "created_at"), at)
  expect_equal(snap$scan_time, at)
  expect_true(nzchar(attr(snap, "scan_root")))
  expect_identical(attr(snap, "signature_enabled"), FALSE)
})
