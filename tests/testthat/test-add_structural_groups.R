# Basic structure ------------------------------------------------------

test_that("add_structural_groups adds expected columns", {
  df <- data.frame(
    rel_path = c(
      "_packages/eviota/R/a.R",
      "_markdown/report/b.qmd"
    ),
    stringsAsFactors = FALSE
  )

  res <- add_structural_groups(df)

  expect_true(
    all(c("structural_group", "component") %in% names(res))
  )

  expect_equal(nrow(res), nrow(df))
})


# Structural grouping derivation ---------------------------------------

test_that("add_structural_groups derives structural grouping heuristics", {
  df <- data.frame(
    rel_path = "_packages/eviota/R/a.R",
    stringsAsFactors = FALSE
  )

  res <- add_structural_groups(df)

  expect_equal(
    res$structural_group,
    "_packages/eviota"
  )

  expect_equal(
    res$component,
    "R"
  )
})


# Minimal paths --------------------------------------------------------

test_that("add_structural_groups handles shallow paths", {
  df <- data.frame(
    rel_path = "README.md",
    stringsAsFactors = FALSE
  )

  res <- add_structural_groups(df)

  expect_equal(
    res$structural_group,
    "README.md"
  )

  expect_true(is.na(res$component))
})


# Error handling -------------------------------------------------------

test_that("add_structural_groups fails without rel_path", {
  df <- data.frame(x = 1)

  expect_error(
    add_structural_groups(df),
    "rel_path"
  )
})


# Integration with snapshot --------------------------------------------

test_that("add_structural_groups works on test_snapshot_12", {
  data("test_snapshot_12", package = "fscontext")

  res <- add_structural_groups(test_snapshot_12)

  expect_true(
    all(c("structural_group", "component") %in% names(res))
  )

  expect_equal(
    nrow(res),
    nrow(test_snapshot_12)
  )
})
