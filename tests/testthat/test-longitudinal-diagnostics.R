# Longitudinal filesystem observation diagnostics (tests only).
# Run: devtools::test(filter = "^longitudinal-diagnostics$")

longitudinal_fixture <- function() {
  root <- tempfile("fscontext_long_source_")
  saved <- tempfile("fscontext_long_saved_")
  dir.create(root, recursive = TRUE)
  dir.create(saved, recursive = TRUE)
  list(root = root, saved = saved)
}

longitudinal_scan <- function(root, at, storage = "long-store", person = "observer") {
  scan_storage(root,
    storage_id = storage, person_id = person,
    scan_time = as.POSIXct(at, tz = "UTC"),
    compute_signature = TRUE
  )
}

test_that("repeated scans preserve path identity but distinguish observation events", {
  fixture <- longitudinal_fixture()
  on.exit(unlink(c(fixture$root, fixture$saved), recursive = TRUE), add = TRUE)
  writeLines("unchanged", file.path(fixture$root, "stable.txt"))
  first <- longitudinal_scan(fixture$root, "2026-11-19 12:00:00")
  second <- longitudinal_scan(fixture$root, "2026-11-19 12:00:02")
  p1 <- save_scan(first, "long-store", fixture$saved)
  p2 <- save_scan(second, "long-store", fixture$saved)
  observations <- read_snapshot(c(p1, p2))
  expect_equal(nrow(observations), 2L)
  expect_length(unique(observations$storage_path_id), 1L)
  expect_length(unique(observations$observation_id), 2L)
  expect_length(unique(observations$snapshot_file), 2L)
  expect_equal(length(unique(observations$quick_sig)), 1L)
})

test_that("additions and deletions remain distinct historical observations", {
  fixture <- longitudinal_fixture()
  on.exit(unlink(c(fixture$root, fixture$saved), recursive = TRUE), add = TRUE)
  writeLines("retained", file.path(fixture$root, "retained.txt"))
  writeLines("removed", file.path(fixture$root, "removed.txt"))
  first <- longitudinal_scan(fixture$root, "2026-11-19 12:00:00")
  p1 <- save_scan(first, "long-store", fixture$saved)
  unlink(file.path(fixture$root, "removed.txt"))
  writeLines("added", file.path(fixture$root, "added.txt"))
  second <- longitudinal_scan(fixture$root, "2026-11-19 12:00:02")
  p2 <- save_scan(second, "long-store", fixture$saved)
  observations <- read_snapshot(c(p1, p2))
  expect_setequal(first$rel_path, c("retained.txt", "removed.txt"))
  expect_setequal(second$rel_path, c("retained.txt", "added.txt"))
  expect_equal(sum(observations$rel_path == "retained.txt"), 2L)
  expect_equal(sum(observations$rel_path == "removed.txt"), 1L)
  expect_equal(sum(observations$rel_path == "added.txt"), 1L)
  expect_equal(nrow(observations), 4L)
})

test_that("content changes retain path identity and change signature", {
  fixture <- longitudinal_fixture()
  on.exit(unlink(c(fixture$root, fixture$saved), recursive = TRUE), add = TRUE)
  target <- file.path(fixture$root, "changing.txt")
  writeLines("original content", target)
  first <- longitudinal_scan(fixture$root, "2026-11-19 12:00:00")
  p1 <- save_scan(first, "long-store", fixture$saved)
  writeLines("different content and length", target)
  second <- longitudinal_scan(fixture$root, "2026-11-19 12:00:02")
  p2 <- save_scan(second, "long-store", fixture$saved)
  observations <- read_snapshot(c(p1, p2))
  expect_equal(nrow(observations), 2L)
  expect_length(unique(observations$storage_path_id), 1L)
  expect_length(unique(observations$observation_id), 2L)
  expect_false(is.na(first$quick_sig[1]))
  expect_false(is.na(second$quick_sig[1]))
  expect_false(identical(first$quick_sig[1], second$quick_sig[1]))
  expect_false(identical(first$size[1], second$size[1]))
})

test_that("storage contexts separate identical relative paths", {
  fixture <- longitudinal_fixture()
  on.exit(unlink(c(fixture$root, fixture$saved), recursive = TRUE), add = TRUE)
  writeLines("shared", file.path(fixture$root, "same.txt"))
  a <- longitudinal_scan(fixture$root, "2026-11-19 12:00:00", "storage-A")
  b <- longitudinal_scan(fixture$root, "2026-11-19 12:00:00", "storage-B")
  p1 <- save_scan(a, "storage-A", fixture$saved)
  p2 <- save_scan(b, "storage-B", fixture$saved)
  observations <- read_snapshot(c(p1, p2))
  expect_equal(nrow(observations), 2L)
  expect_length(unique(observations$rel_path), 1L)
  expect_length(unique(observations$storage_path_id), 2L)
  expect_length(unique(observations$observation_id), 2L)
})

test_that("observe_universe counts repeated snapshot files rather than unique paths", {
  fixture <- longitudinal_fixture()
  on.exit(unlink(c(fixture$root, fixture$saved), recursive = TRUE), add = TRUE)
  writeLines("persistent", file.path(fixture$root, "persistent.txt"))
  first <- longitudinal_scan(fixture$root, "2026-11-19 12:00:00")
  save_scan(first, "long-store", fixture$saved)
  second <- longitudinal_scan(fixture$root, "2026-11-19 12:00:02")
  save_scan(second, "long-store", fixture$saved)
  result <- observe_universe(fixture$saved, max_aggregation_depth = 2)
  expect_true(nrow(result) >= 1L)
  expect_true(any(result$n_observations == 2L))
  expect_true(any(result$total_files_unit == 2L))
})
