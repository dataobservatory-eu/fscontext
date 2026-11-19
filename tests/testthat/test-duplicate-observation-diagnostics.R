# Tests-only diagnostics: repeated inputs and duplicate observational rows.
# Run: devtools::test(filter = "^duplicate-observation-diagnostics$")
# These tests distinguish snapshot artefacts, observational rows, and logical paths.

snapshot_duplicate_fixture <- function() {
  root <- tempfile("fscontext_duplicates_")
  dir.create(root, recursive = TRUE)
  root
}

snapshot_duplicate_rows <- function(paths, sizes = rep(10, length(paths))) {
  data.frame(
    full_path = as.character(paths),
    rel_path = basename(paths),
    size = as.numeric(sizes),
    storage_id = rep("storage-A", length(paths)),
    person_id = rep("person-A", length(paths)),
    scan_time = as.POSIXct(rep("2026-11-19 12:00:00", length(paths)), tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

test_that("re-reading one persisted snapshot is deterministic", {
  root <- snapshot_duplicate_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "snapshot.rds")
  saveRDS(snapshot_duplicate_rows(file.path(root, "source", "a.txt")), file)
  a <- read_snapshot(file)
  b <- read_snapshot(file)
  expect_identical(a$storage_path_id, b$storage_path_id)
  expect_identical(a$observation_id, b$observation_id)
  expect_identical(a$snapshot_file, b$snapshot_file)
  expect_equal(nrow(a), 1L)
})

test_that("supplying the same snapshot twice retains both input occurrences", {
  root <- snapshot_duplicate_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "snapshot.rds")
  saveRDS(snapshot_duplicate_rows(file.path(root, "source", "a.txt")), file)
  result <- read_snapshot(c(file, file))
  expect_equal(nrow(result), 2L)
  expect_identical(result$snapshot_file[1], result$snapshot_file[2])
  expect_identical(result$storage_path_id[1], result$storage_path_id[2])
  expect_identical(result$observation_id[1], result$observation_id[2])
})

test_that("two distinct snapshot artefacts containing identical rows remain traceable", {
  root <- snapshot_duplicate_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  x <- snapshot_duplicate_rows(file.path(root, "source", "a.txt"))
  first <- file.path(root, "first.rds")
  second <- file.path(root, "second.rds")
  saveRDS(x, first)
  saveRDS(x, second)
  result <- read_snapshot(c(first, second))
  expect_equal(nrow(result), 2L)
  expect_equal(length(unique(result$snapshot_file)), 2L)
  expect_equal(length(unique(result$storage_path_id)), 1L)
  # Same recorded timestamp and path: the current identifier cannot
  # establish whether these artefacts represent separate scan events.
  expect_identical(result$observation_id[1], result$observation_id[2])
})

test_that("observe_universe counts snapshot artefacts separately from rows", {
  root <- snapshot_duplicate_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  x <- snapshot_duplicate_rows(file.path(root, "source", "a.txt"))
  saveRDS(x, file.path(root, "first.rds"))
  saveRDS(x, file.path(root, "second.rds"))
  result <- observe_universe(root, max_aggregation_depth = 100)
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_observations, 2L)
  expect_equal(result$total_files_unit, 2L)
  expect_equal(result$total_size_unit, 20)
})

test_that("duplicate rows inside one snapshot do not inflate snapshot count", {
  root <- snapshot_duplicate_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  x <- snapshot_duplicate_rows(file.path(root, "source", "a.txt"))
  saveRDS(rbind(x, x), file.path(root, "one.rds"))
  result <- observe_universe(root, max_aggregation_depth = 100)
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_observations, 1L)
  # total_files_unit counts rows, not distinct storage_path_id values.
  expect_equal(result$total_files_unit, 2L)
  expect_equal(result$total_size_unit, 20)
})

test_that("context enrichment is idempotent for existing identifiers", {
  x <- snapshot_duplicate_rows("C:/source/a.txt")
  first <- add_snapshot_context(x)
  second <- add_snapshot_context(first)
  expect_identical(first$storage_path_id, second$storage_path_id)
  expect_identical(first$observation_id, second$observation_id)
  expect_equal(nrow(first), nrow(second))
})
