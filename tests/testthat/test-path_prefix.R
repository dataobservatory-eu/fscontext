library(testthat)

test_that("path_prefix basic behaviour", {
  x <- "_eviota/reporting/R/file.R"

  expect_equal(path_prefix(x, 1), "_eviota")
  expect_equal(path_prefix(x, 2), "_eviota/reporting")
  expect_equal(path_prefix(x, 3), "_eviota/reporting/R")
})

test_that("path_prefix handles short paths", {
  x <- "file.R"

  expect_equal(path_prefix(x, 1), "file.R")
  expect_equal(path_prefix(x, 2), "file.R")
})

test_that("path_prefix handles multiple paths", {
  x <- c(
    "_eviota/reporting/R/a.R",
    "_packages/iotables/R/b.R"
  )

  expect_equal(
    path_prefix(x, 2),
    c("_eviota/reporting", "_packages/iotables")
  )
})

test_that("path_prefix normalises backslashes", {
  x <- "_eviota\\reporting\\R\\file.R"

  expect_equal(
    path_prefix(x, 2),
    "_eviota/reporting"
  )
})

test_that("path_prefix handles depth 0", {
  x <- "_eviota/reporting/R/file.R"

  expect_equal(path_prefix(x, 0), "")
})

test_that("path_prefix rejects invalid depth", {
  expect_error(path_prefix("a/b/c", -1))
  expect_error(path_prefix("a/b/c", NA))
})
