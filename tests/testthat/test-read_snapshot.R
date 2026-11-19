# Tests for fscontext::read_snapshot().
# Existing observational and repository metadata tests are preserved.
# Additional tests cover snapshot schema-version validation.

test_that("read_snapshot reads multiple snapshot schemas", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_s3_class(res, "data.frame")
  expect_equal(
    nrow(res),
    nrow(fscontextdemo_snapshot_01) + nrow(fscontextdemo_snapshot_02)
  )
})

test_that("read_snapshot preserves observational uniqueness", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_equal(anyDuplicated(res$observation_id), 0)
  expect_gt(anyDuplicated(res$storage_path_id), 0)
})

test_that("read_snapshot normalises missing schema versions", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_true("snapshot_schema_version" %in% names(res))
  expect_true(all(!is.na(res$snapshot_schema_version)))
})

test_that("read_snapshot adds provenance columns", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_true("snapshot_file" %in% names(res))
  expect_true("snapshot_created_at" %in% names(res))
  expect_true("snapshot_schema_version" %in% names(res))
})

test_that("read_snapshot adds contextual identifiers", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_true("storage_full_path" %in% names(res))
  expect_true("storage_path_id" %in% names(res))
  expect_true("observation_id" %in% names(res))
})

test_that("read_snapshot errors on missing files", {
  expect_error(
    read_snapshot("does-not-exist.rds"),
    "Snapshot files do not exist"
  )
})

test_that("read_snapshot errors on non-dataframe snapshots", {
  f <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1), f)

  expect_error(read_snapshot(f), "Snapshot is not a data.frame")
})

test_that("read_snapshot does not materialize repo metadata by default", {
  data("fscontextdemo_snapshot_01")

  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_01, f)

  res <- read_snapshot(f)

  expect_false("git_remote" %in% names(res))
  expect_false("git_branch" %in% names(res))
  expect_false("git_repo_id" %in% names(res))
})

test_that("read_snapshot materializes repo metadata when requested", {
  data("fscontextdemo_snapshot_02")

  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, f)

  res <- read_snapshot(f, include_repo_metadata = TRUE)

  expect_true("git_remote" %in% names(res))
  expect_true("git_branch" %in% names(res))
  expect_true("git_repo_id" %in% names(res))
  expect_true(any(grepl("github.com", res$git_remote)))
})

# Schema-version validation tests (issue #7).

test_that("read_snapshot accepts legacy schema version 0.1.0", {
  data("fscontextdemo_snapshot_01")

  x <- fscontextdemo_snapshot_01
  attr(x, "schema_version") <- "0.1.0"

  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)

  res <- read_snapshot(f)

  expect_s3_class(res, "data.frame")
  expect_true(all(res$snapshot_schema_version == "0.1.0"))
})

test_that("read_snapshot accepts schema version 0.1.2", {
  data("fscontextdemo_snapshot_01")

  x <- fscontextdemo_snapshot_01
  attr(x, "schema_version") <- "0.1.2"

  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)

  res <- read_snapshot(f)

  expect_s3_class(res, "data.frame")
  expect_true(all(res$snapshot_schema_version == "0.1.2"))
})

test_that("read_snapshot accepts schema version 0.1.3", {
  data("fscontextdemo_snapshot_01")

  x <- fscontextdemo_snapshot_01
  attr(x, "schema_version") <- "0.1.3"

  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)

  res <- read_snapshot(f)

  expect_s3_class(res, "data.frame")
  expect_true(all(res$snapshot_schema_version == "0.1.3"))
})

test_that("read_snapshot uses legacy fallback for missing schema", {
  data("fscontextdemo_snapshot_01")

  x <- fscontextdemo_snapshot_01
  attr(x, "schema_version") <- NULL

  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)

  res <- read_snapshot(f)

  expect_true(all(res$snapshot_schema_version == "0.1.0"))
})

test_that("read_snapshot rejects unsupported future schema versions", {
  data("fscontextdemo_snapshot_01")

  x <- fscontextdemo_snapshot_01
  attr(x, "schema_version") <- "999.0.0"

  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)

  expect_error(read_snapshot(f), "Unsupported snapshot schema version")
})

test_that("read_snapshot rejects malformed schema metadata", {
  data("fscontextdemo_snapshot_01")

  invalid_versions <- list(
    NA_character_,
    character(0),
    c("0.1.2", "0.1.3"),
    123
  )

  for (version in invalid_versions) {
    x <- fscontextdemo_snapshot_01
    attr(x, "schema_version") <- version

    f <- tempfile(fileext = ".rds")
    saveRDS(x, f)

    expect_error(
      read_snapshot(f),
      "Unsupported snapshot schema version"
    )
  }
})

test_that("read_snapshot rejects mixed supported and future schemas", {
  data("fscontextdemo_snapshot_01")

  valid <- fscontextdemo_snapshot_01
  invalid <- fscontextdemo_snapshot_01

  attr(valid, "schema_version") <- "0.1.3"
  attr(invalid, "schema_version") <- "999.0.0"

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(valid, f1)
  saveRDS(invalid, f2)

  expect_error(
    read_snapshot(c(f1, f2)),
    "Unsupported snapshot schema version"
  )
})
