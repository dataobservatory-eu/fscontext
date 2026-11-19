# fscontext 0.2.2: tests-only characterization of snapshot persistence,
# schema normalization, and observation identity.
# Install at tests/testthat/test-snapshot-diagnostics.R.
# No production code is modified.

test_that("save_scan preserves original snapshot data and attributes", {
  root <- fs::file_temp("fscontext_source_")
  dest <- fs::file_temp("fscontext_saved_")
  fs::dir_create(root)
  fs::dir_create(dest)
  withr::defer(fs::dir_delete(root))
  withr::defer(fs::dir_delete(dest))

  writeLines("archival observation", fs::path(root, "document.txt"))
  at <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  original <- scan_storage(root,
    storage_id = "store-A",
    person_id = "observer-A", scan_time = at,
    compute_signature = FALSE
  )
  saved_path <- save_scan(original, storage_id = "store-A", path = dest)
  expect_true(file.exists(saved_path))
  expect_identical(readRDS(saved_path), original)
})

test_that("read_snapshot retains source paths and observational provenance", {
  root <- fs::file_temp("fscontext_source_")
  dest <- fs::file_temp("fscontext_saved_")
  fs::dir_create(root)
  fs::dir_create(dest)
  withr::defer(fs::dir_delete(root))
  withr::defer(fs::dir_delete(dest))

  writeLines("test", fs::path(root, "item.txt"))
  at <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  original <- scan_storage(root,
    storage_id = "store-A",
    person_id = "observer-A", scan_time = at,
    compute_signature = FALSE
  )
  saved_path <- save_scan(original, storage_id = "store-A", path = dest)
  restored <- read_snapshot(saved_path)
  expect_equal(nrow(restored), nrow(original))
  expect_identical(restored$rel_path, original$rel_path)
  expect_identical(restored$full_path, original$full_path)
  expect_identical(restored$storage_path_id, original$storage_path_id)
  expect_identical(
    restored$snapshot_schema_version,
    rep(attr(original, "schema_version"), nrow(original))
  )
  expect_true(all(file.exists(restored$snapshot_file)))
  expect_true(all(restored$snapshot_created_at == at))
  expect_true(all(!is.na(restored$observation_id)))
})

test_that("legacy schema normalization is idempotent and conservative", {
  legacy <- data.frame(
    rel_path = c("folder/item.txt", "README"),
    file = c("item.txt", "README"),
    folder = c("folder", ""),
    stringsAsFactors = FALSE
  )
  attr(legacy, "schema_version") <- "0.1.0"
  first <- normalise_snapshot_schema(legacy)
  second <- normalise_snapshot_schema(first)
  expect_identical(second, first)
  expect_identical(first$filename, c("item.txt", "README"))
  expect_identical(first$group_path, legacy$folder)
  expect_identical(first$file, legacy$file)
  expect_identical(first$folder, legacy$folder)
  expect_identical(attr(first, "schema_version"), "0.1.2")
  expect_identical(attr(first, "normalised_from"), "0.1.0")
})

test_that("schema normalization retains canonical columns when conflicting", {
  legacy <- data.frame(
    rel_path = "folder/item.txt",
    filename = "canonical.txt",
    file = "legacy.txt",
    group_path = "canonical-folder",
    folder = "legacy-folder",
    stringsAsFactors = FALSE
  )
  attr(legacy, "schema_version") <- "0.1.0"
  warnings <- character()

  normalised <- withCallingHandlers(
    normalise_snapshot_schema(legacy),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 2L)
  expect_true(any(grepl(
    "Conflict between 'file' and 'filename'",
    warnings,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Conflict between 'folder' and 'group_path'",
    warnings,
    fixed = TRUE
  )))
  expect_identical(normalised$filename, "canonical.txt")
  expect_identical(normalised$group_path, "canonical-folder")
  expect_identical(normalised$file, "legacy.txt")
  expect_identical(normalised$folder, "legacy-folder")
})

test_that("current schema normalization leaves snapshots unchanged", {
  current <- data.frame(rel_path = "item.txt", filename = "item.txt")
  attr(current, "schema_version") <- "0.1.3"
  expect_identical(normalise_snapshot_schema(current), current)
})

test_that("observation IDs distinguish scans at different seconds", {
  root <- fs::file_temp("fscontext_identity_")
  fs::dir_create(root)
  withr::defer(fs::dir_delete(root))
  writeLines("stable", fs::path(root, "item.txt"))
  t1 <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  t2 <- t1 + 2
  first <- add_snapshot_context(scan_storage(
    root,
    storage_id = "store-A", scan_time = t1,
    compute_signature = FALSE
  ))
  second <- add_snapshot_context(scan_storage(
    root,
    storage_id = "store-A", scan_time = t2,
    compute_signature = FALSE
  ))
  expect_identical(first$storage_path_id, second$storage_path_id)
  expect_false(identical(first$observation_id, second$observation_id))
})

# Deliberately not asserting uniqueness for two scans in the SAME second.
# The current implementation formats observation_id to whole seconds;
# a same-second collision should be recorded as a separate issue if relevant.
