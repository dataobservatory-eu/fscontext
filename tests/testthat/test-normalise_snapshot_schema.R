test_that("normalisation is idempotent", {
  df <- data.frame(
    rel_path = c("a/b/c.R"),
    filename = "c.R",
    stringsAsFactors = FALSE
  )
  attr(df, "schema_version") <- "0.1.2"

  out1 <- normalise_snapshot_schema(df)
  out2 <- normalise_snapshot_schema(out1)

  expect_equal(out1, out2)
})

test_that("filename is derived from rel_path if missing", {
  df <- data.frame(
    rel_path = c("x/y/z.R"),
    stringsAsFactors = FALSE
  )

  out <- normalise_snapshot_schema(df)

  expect_equal(out$filename, "z.R")
})


test_that("dir_path is derived correctly", {
  df <- data.frame(
    rel_path = c("a/b/c.R", "file.R"),
    stringsAsFactors = FALSE
  )

  out <- normalise_snapshot_schema(df)

  expect_equal(out$dir_path, c("a/b", ""))
})


test_that("legacy 'file' column is mapped to filename", {
  df <- data.frame(
    rel_path = c("a/b/c.R"),
    file = "c.R",
    stringsAsFactors = FALSE
  )

  out <- normalise_snapshot_schema(df)

  expect_equal(out$filename, "c.R")
})


test_that("legacy 'folder' column is mapped to group_path", {
  df <- data.frame(
    rel_path = c("a/b/c.R"),
    filename = "c.R",
    folder = "a/b",
    stringsAsFactors = FALSE
  )

  out <- normalise_snapshot_schema(df)

  expect_equal(out$group_path, "a/b")
})


test_that("conflict between file and filename triggers warning", {
  df <- data.frame(
    rel_path = c("a/b/c.R"),
    filename = "c.R",
    file = "different.R",
    stringsAsFactors = FALSE
  )

  expect_warning(
    normalise_snapshot_schema(df),
    "Conflict between 'file' and 'filename'"
  )
})


test_that("missing rel_path triggers error", {
  df <- data.frame(
    filename = "a.R",
    stringsAsFactors = FALSE
  )

  expect_error(
    normalise_snapshot_schema(df),
    "Missing required columns"
  )
})


test_that("already normalised data passes through unchanged", {
  df <- data.frame(
    rel_path = c("a/b/c.R"),
    filename = "c.R",
    dir_path = "a/b",
    stringsAsFactors = FALSE
  )
  attr(df, "schema_version") <- "0.1.2"

  out <- normalise_snapshot_schema(df)

  expect_equal(df, out)
})


test_that("schema metadata is attached", {
  df <- data.frame(
    rel_path = c("a/b/c.R"),
    stringsAsFactors = FALSE
  )

  out <- normalise_snapshot_schema(df)

  expect_equal(attr(out, "schema_version"), "0.1.2")
  expect_equal(attr(out, "normalised_from"), "0.1.0")
})
