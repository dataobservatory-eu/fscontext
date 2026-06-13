library(testthat)

# Helper minimal dataset ----------------------------------------------

make_test_df <- function() {
  df <- data.frame(
    rel_path = c(
      "_eviota/reporting/R/a.R",
      "_eviota/reporting/R/b.R",
      "_eviota/reporting/tests/test-a.R",
      "_packages/iotables/R/c.R",
      "_packages/iotables/data-raw/d.bak",
      "_packages/iotables/R/e.R"
    ),
    extension = c("r", "r", "r", "r", "bak", "r"),
    mtime = as.POSIXct(c(
      "2026-04-28",
      "2026-04-29",
      "2026-04-29",
      "2026-04-30",
      "2026-04-30",
      "2026-04-30"
    )),
    git_tracked = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  df$filename <- basename(df$rel_path)
  df
}

# Structure ------------------------------------------------------------

test_that("summarise_activity returns expected structure", {
  df <- make_test_df()

  res <- summarise_activity(df, time_unit = "week")

  expect_s3_class(res, "data.frame")

  expect_true(all(c(
    "period", "group_path", "start", "end",
    "file_names", "n_files", "n_unique_files", "untracked"
  ) %in% names(res)))
})

# Determinism ----------------------------------------------------------

test_that("summarise_activity is deterministic", {
  df <- make_test_df()

  res1 <- summarise_activity(df, time_unit = "week")
  res2 <- summarise_activity(df, time_unit = "week")

  expect_equal(res1, res2)
})

# Extension filtering --------------------------------------------------

test_that("extensions filter works", {
  df <- make_test_df()

  res <- summarise_activity(df, extensions = "r")

  expect_false(any(grepl("d.bak", res$file_names)))
})

# Time unit ------------------------------------------------------------

test_that("time_unit changes aggregation", {
  df <- make_test_df()

  res_week <- summarise_activity(df, time_unit = "week")
  res_month <- summarise_activity(df, time_unit = "month")

  expect_true(nrow(res_month) <= nrow(res_week))
})

# Untracked counting ---------------------------------------------------

test_that("untracked files counted correctly", {
  df <- make_test_df()

  res <- summarise_activity(df)

  expect_true(any(res$untracked > 0, na.rm = TRUE))
})

# Unique file counting -------------------------------------------------

test_that("n_unique_files counts distinct files correctly", {
  df <- make_test_df()

  res <- summarise_activity(df)

  expect_true(all(res$n_unique_files <= res$n_files))
  expect_true(all(res$n_unique_files > 0))
})

# File aggregation -----------------------------------------------------

test_that("file_names are non-empty and aggregated", {
  df <- make_test_df()

  res <- summarise_activity(df)

  expect_true(all(nchar(res$file_names) > 0))
})

# Missing git_tracked --------------------------------------------------

test_that("summarise_activity works without git_tracked", {
  df <- make_test_df()
  df$git_tracked <- NULL

  res <- summarise_activity(df)

  expect_true("untracked" %in% names(res))
  expect_true(all(is.na(res$untracked)))
})

# Realistic snapshot (legacy) ------------------------------------------

test_that("summarise_activity works on canonical snapshot", {
  data(fscontextdemo_snapshot_02, package = "fscontext")

  res <- summarise_activity(
    fscontextdemo_snapshot_02,
    extensions = c("html", "css", "js")
  )

  expect_s3_class(res, "data.frame")

  expect_true(nrow(res) > 0)

  expect_true(all(!is.na(res$period)))

  expect_true(all(!is.na(res$group_path)))
})

# Realistic snapshot (canonical) ---------------------------------------

test_that("summarise_activity works on canonical snapshot", {
  data(fscontextdemo_snapshot_02, package = "fscontext")

  res <- summarise_activity(test_snapshot_12)

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)

  expect_true(all(c(
    "period", "group_path", "start", "end",
    "file_names", "n_files", "n_unique_files", "untracked"
  ) %in% names(res)))
})

# Normalisation --------------------------------------------------------

test_that("normalisation derives filename from legacy 'file'", {
  df <- make_test_df()
  df$file <- basename(df$rel_path)
  df$filename <- NULL

  res <- normalise_snapshot_schema(df)

  expect_true("filename" %in% names(res))
})


#  The same test in application for the main function

test_that("group_path never contains file extensions", {
  df <- tibble::tibble(
    rel_path = c("pkg/R/hello.R", "pkg/tests/testthat/test-hello.R"),
    filename = c("hello.R", "test-hello.R"),
    extension = c("r", "r"),
    mtime = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-02 10:00:00"
    )),
    git_tracked = c(TRUE, FALSE)
  )

  res <- summarise_activity(df)

  expect_false(any(grepl("\\.[A-Za-z0-9]+$", res$group_path)))
})
