# Snapshot persistence diagnostics: sequential collision and recovery.
# No production code changes, parallel writes, or process interruption.

persistence_fixture <- function() {
  root <- tempfile("fscontext_persist_")
  dir.create(root, recursive = TRUE)
  root
}

persistence_snapshot <- function(root, content, at) {
  source <- file.path(root, "source")
  dir.create(source, showWarnings = FALSE)
  writeLines(content, file.path(source, "item.txt"))

  suppressMessages(scan_storage(
    source,
    storage_id = "persist-store", person_id = "tester",
    scan_time = at, compute_signature = FALSE
  ))
}

persistence_save <- function(x, folder, label = NULL) {
  suppressMessages(save_scan(
    x,
    storage_id = "persist-store",
    path = folder, label = label
  ))
}

test_that("saved snapshot can be read back with metadata intact", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 09:00:00", tz = "UTC")
  x <- persistence_snapshot(root, "first", at)
  path <- persistence_save(x, file.path(root, "snapshots"))
  restored <- readRDS(path)

  expect_true(file.exists(path))
  expect_equal(restored$rel_path, x$rel_path)
  expect_equal(restored$size, x$size)
  expect_equal(attr(restored, "created_at"), at)
  expect_equal(attr(restored, "scan_root"), attr(x, "scan_root"))
})

test_that("different observation times preserve both snapshot artefacts", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 09:00:00", tz = "UTC")
  x <- persistence_snapshot(root, "first", at)
  y <- persistence_snapshot(root, "longer second content", at + 120)
  folder <- file.path(root, "snapshots")

  first <- persistence_save(x, folder)
  second <- persistence_save(y, folder)

  expect_false(identical(first, second))
  expect_true(file.exists(first))
  expect_true(file.exists(second))
  expect_equal(readRDS(first)$size, x$size)
  expect_equal(readRDS(second)$size, y$size)
})

test_that("same-time independent scans do not silently overwrite evidence", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 09:00:00", tz = "UTC")
  x <- persistence_snapshot(root, "first", at)
  y <- persistence_snapshot(root, "longer second content", at)
  folder <- file.path(root, "snapshots")

  first <- persistence_save(x, folder)
  before <- readRDS(first)

  # A filename collision must be rejected without replacing evidence.
  expect_error(
    persistence_save(y, folder),
    "snapshot already exists"
  )

  expect_true(file.exists(first))
  expect_identical(readRDS(first), before)
  expect_equal(readRDS(first)$size, x$size)

  # No second snapshot should have been published.
  expect_length(list.files(folder, pattern = "\\.rds$"), 1L)
})

test_that("save_scan rejects missing observation timestamp", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  x <- persistence_snapshot(root, "first", Sys.time())
  attr(x, "created_at") <- NULL

  expect_error(
    persistence_save(x, file.path(root, "snapshots")),
    "created_at"
  )
})

test_that("corrupt snapshot artefact is not accepted as a valid observation", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  folder <- file.path(root, "snapshots")
  dir.create(folder)

  corrupt <- file.path(folder, "truncated.rds")
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03)), corrupt)

  expect_error(readRDS(corrupt))
  expect_error(read_snapshot(corrupt))
})

# Additional regression tests for issue #5.

test_that("repeated save of the same snapshot is rejected", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 09:04:00", tz = "UTC")
  x <- persistence_snapshot(root, "first", at)
  folder <- file.path(root, "snapshots")

  first <- persistence_save(x, folder)

  expect_error(
    persistence_save(x, folder),
    "snapshot already exists"
  )

  expect_identical(readRDS(first), x)
  expect_length(list.files(folder, pattern = "\\.rds$"), 1L)
})

test_that("different scope labels allow independent snapshot files", {
  root <- persistence_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 09:05:00", tz = "UTC")
  x <- persistence_snapshot(root, "first", at)
  y <- persistence_snapshot(root, "longer second content", at)
  folder <- file.path(root, "snapshots")

  first <- persistence_save(x, folder, label = "scope_a")
  second <- persistence_save(y, folder, label = "scope_b")

  expect_false(identical(first, second))
  expect_true(file.exists(first))
  expect_true(file.exists(second))
  expect_identical(readRDS(first), x)
  expect_identical(readRDS(second), y)
})
