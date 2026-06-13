# Group path derivation  -----------------------------------------------

test_that("derive_group_path extracts project and module", {
  x <- "_packages/iotables/R/file.R"

  res <- derive_group_path(x)

  expect_equal(res, "_packages/iotables/R")
})

test_that("derive_group_path falls back to project if no module", {
  x <- "_packages/iotables/utils.R"

  res <- derive_group_path(x)

  expect_equal(res, "_packages/iotables")
})

test_that("derive_group_path is vectorised", {
  x <- c(
    "_packages/iotables/R/a.R",
    "_packages/iotables/tests/test-a.R"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c("_packages/iotables/R", "_packages/iotables/tests")
  )
})

test_that("derive_group_path does not leak filenames into group_path", {
  x <- "_packages/iotables/utils.R"

  res <- derive_group_path(x)

  expect_false(grepl("\\.[A-Za-z0-9]+$", res))
})

test_that("derive_group_path keeps _packages and packages distinct", {
  x <- c(
    "_packages/iotables/a.R",
    "packages/iotables/a.R"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c("_packages/iotables", "packages/iotables")
  )
})

test_that("derive_group_path does not leak root-level filenames", {
  x <- c(
    "_packages/iotables/a.R",
    "packages/iotables/a.R",
    "_eviota/iotables/DESCRIPTION"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c(
      "_packages/iotables",
      "packages/iotables",
      "_eviota/iotables"
    )
  )
})

test_that("derive_group_path keeps module folders", {
  x <- c(
    "_packages/iotables/R/a.R",
    "_packages/iotables/tests/testthat/test-a.R",
    "packages/filmledgerimport/data-raw/input.csv"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c(
      "_packages/iotables/R",
      "_packages/iotables/tests",
      "packages/filmledgerimport/data-raw"
    )
  )
})

test_that("derive_group_path normalises Windows separators", {
  x <- c(
    "_packages\\iotables\\R\\a.R",
    "packages\\iotables\\a.R"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c("_packages/iotables/R", "packages/iotables")
  )
})

test_that("derive_group_path handles short and empty paths", {
  x <- c("a.R", "", NA_character_)

  res <- derive_group_path(x)

  expect_true(is.na(res[1]))
  expect_true(is.na(res[2]))
  expect_true(is.na(res[3]))
})

test_that("derive_group_path handles short paths", {
  expect_true(
    is.na(
      derive_group_path("file.R")
    )
  )
})
