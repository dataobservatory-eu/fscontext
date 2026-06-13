# input validation testing -------------------------------------------
test_that("x must be a data.frame or tibble", {
  expect_error(
    classify_operational_file_type(
      x = c("R", "csv")
    ),
    "x must be a data.frame or tibble"
  )
})

test_that("missing extension column errors", {
  toy_files <- tibble::tibble(
    file_ext = c("R")
  )

  expect_error(
    classify_operational_file_type(
      toy_files,
      extension = "extension"
    ),
    "Column not found"
  )
})

# basic functionality testing ----------------------------------------------
test_that("classify_operational_file_type classifies R workflow files", {
  toy_files <- tibble::tibble(
    extension = c(
      "R",
      "qmd",
      "csv",
      "png",
      "woff2",
      "unknown"
    )
  )

  res <- classify_operational_file_type(
    toy_files,
    profile = "r_development"
  )

  expect_equal(
    res,
    c(
      "code",
      "markdown",
      "data",
      "artifact",
      "website_generated",
      "other"
    )
  )
})

test_that("classification is case insensitive", {
  toy_files <- tibble::tibble(
    extension = c(
      "R",
      "QMD",
      "Csv",
      "PNG"
    )
  )

  res <- classify_operational_file_type(
    toy_files,
    profile = "r_development"
  )

  expect_equal(
    res,
    c(
      "code",
      "markdown",
      "data",
      "artifact"
    )
  )
})

test_that("unknown extensions become other", {
  toy_files <- tibble::tibble(
    extension = c(
      "abcxyz",
      "foobar"
    )
  )

  res <- classify_operational_file_type(
    toy_files,
    profile = "r_development"
  )

  expect_true(
    all(res == "other")
  )
})

test_that("unknown profile falls back to other", {
  toy_files <- tibble::tibble(
    extension = c("R", "csv")
  )

  res <- classify_operational_file_type(
    toy_files,
    profile = "unknown_profile"
  )

  expect_equal(
    res,
    c("other", "other")
  )
})
