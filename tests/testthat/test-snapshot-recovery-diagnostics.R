# Tests-only diagnostics for snapshot recovery and validation.
# No repair or mutation of persisted artefacts is attempted.

recovery_fixture <- function() {
  root <- tempfile("fscontext_recovery_")
  dir.create(root, recursive = TRUE)
  root
}

recovery_snapshot <- function(schema = "0.1.3", created = TRUE) {
  x <- data.frame(
    storage_id = "recovery-store",
    person_id = "observer",
    rel_path = "item.txt",
    full_path = "C:/observed/item.txt",
    size = 12,
    scan_time = as.POSIXct("2026-11-19 10:00:00", tz = "UTC"),
    stringsAsFactors = FALSE
  )

  if (!is.null(schema)) {
    attr(x, "schema_version") <- schema
  }
  if (created) {
    attr(x, "created_at") <- x$scan_time[[1]]
  }

  x
}

recovery_write <- function(root, name, x) {
  path <- file.path(root, name)
  saveRDS(x, path)
  path
}

test_that("valid snapshot retains artefact provenance when read", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  x <- recovery_snapshot()
  path <- recovery_write(root, "valid.rds", x)
  observed <- read_snapshot(path)

  expect_s3_class(observed, "data.frame")
  expect_equal(nrow(observed), 1L)
  expect_equal(observed$snapshot_schema_version, "0.1.3")
  expect_equal(observed$snapshot_created_at, attr(x, "created_at"))
  expect_equal(observed$snapshot_file, normalizePath(path, winslash = "/"))
})

test_that("missing snapshot path is rejected explicitly", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  path <- file.path(root, "absent.rds")
  expect_error(read_snapshot(path), "Snapshot files do not exist")
})

test_that("unreadable truncated RDS cannot enter reconstruction", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  path <- file.path(root, "partial.rds")
  writeBin(as.raw(c(0, 1, 2)), path)

  expect_error(read_snapshot(path))
})

test_that("serialized non-data-frame is rejected explicitly", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  path <- recovery_write(root, "wrong-type.rds", list(size = 12))
  expect_error(read_snapshot(path), "not a data.frame")
})

test_that("legacy snapshot without schema uses documented fallback", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  x <- recovery_snapshot(schema = NULL)
  path <- recovery_write(root, "legacy.rds", x)
  observed <- read_snapshot(path)

  expect_equal(observed$snapshot_schema_version, "0.1.0")
  expect_equal(nrow(observed), 1L)
})

test_that("missing creation metadata does not produce an invented timestamp", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  x <- recovery_snapshot(created = FALSE)
  path <- recovery_write(root, "no-created-at.rds", x)
  result <- tryCatch(read_snapshot(path), error = identity)

  if (inherits(result, "error")) {
    expect_s3_class(result, "error")
  } else {
    has_timestamp <- "snapshot_created_at" %in% names(result) &&
      any(!is.na(result$snapshot_created_at))

    expect_false(
      has_timestamp,
      info = "Missing created_at must not become a fabricated timestamp"
    )
  }
})

test_that("mixed valid and corrupt inputs do not return partial results", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  valid <- recovery_write(root, "valid.rds", recovery_snapshot())
  corrupt <- file.path(root, "corrupt.rds")
  writeBin(as.raw(c(0, 1, 2)), corrupt)

  expect_error(read_snapshot(c(valid, corrupt)))
})

test_that("unsupported future schema is not silently accepted", {
  root <- recovery_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  x <- recovery_snapshot(schema = "999.0.0")
  path <- recovery_write(root, "future.rds", x)

  # Unknown schemas must not be interpreted using current assumptions.
  expect_error(
    read_snapshot(path),
    "schema|version|unsupported|unknown",
    info = "Unknown future schema was accepted without validation"
  )
})
