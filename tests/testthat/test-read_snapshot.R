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
    nrow(fscontextdemo_snapshot_01) +
      nrow(fscontextdemo_snapshot_02)
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

  expect_equal(
    anyDuplicated(res$observation_id),
    0
  )

  expect_gt(
    anyDuplicated(res$storage_path_id),
    0
  )
})

test_that("read_snapshot normalises missing schema versions", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_true(
    "snapshot_schema_version" %in% names(res)
  )

  expect_true(
    all(!is.na(res$snapshot_schema_version))
  )
})

test_that("read_snapshot adds provenance columns", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_true(
    "snapshot_file" %in% names(res)
  )

  expect_true(
    "snapshot_created_at" %in% names(res)
  )

  expect_true(
    "snapshot_schema_version" %in% names(res)
  )
})

test_that("read_snapshot adds contextual identifiers", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  f1 <- tempfile(fileext = ".rds")
  f2 <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_01, f1)
  saveRDS(fscontextdemo_snapshot_02, f2)

  res <- read_snapshot(c(f1, f2))

  expect_true(
    "storage_full_path" %in% names(res)
  )

  expect_true(
    "storage_path_id" %in% names(res)
  )

  expect_true(
    "observation_id" %in% names(res)
  )
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

  expect_error(
    read_snapshot(f),
    "Snapshot is not a data.frame"
  )
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

  res <- read_snapshot(
    f,
    include_repo_metadata = TRUE
  )

  expect_true("git_remote" %in% names(res))
  expect_true("git_branch" %in% names(res))
  expect_true("git_repo_id" %in% names(res))

  expect_true(
    any(
      grepl(
        "github.com",
        res$git_remote
      )
    )
  )
})
