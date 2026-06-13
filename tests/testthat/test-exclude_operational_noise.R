test_that("exclude_operational_noise removes generic noise", {
  toy_files <- tibble::tibble(
    filename = c(
      ".DS_Store",
      "analysis.R"
    ),
    extension = c(
      "",
      "R"
    )
  )

  res <- exclude_operational_noise(
    toy_files,
    profiles = "generic"
  )

  expect_equal(
    nrow(res),
    1
  )

  expect_equal(
    res$filename,
    "analysis.R"
  )
})

test_that("exclude_operational_noise removes rstudio noise", {
  toy_files <- tibble::tibble(
    filename = c(
      ".Rhistory",
      "report.qmd"
    ),
    extension = c(
      "",
      "qmd"
    )
  )

  res <- exclude_operational_noise(
    toy_files,
    profiles = "rstudio"
  )

  expect_equal(
    nrow(res),
    1
  )

  expect_equal(
    res$filename,
    "report.qmd"
  )
})

test_that("exclude_operational_noise keeps normal files", {
  toy_files <- tibble::tibble(
    filename = c(
      "analysis.R",
      "report.qmd"
    ),
    extension = c(
      "R",
      "qmd"
    )
  )

  res <- exclude_operational_noise(
    toy_files
  )

  expect_equal(
    nrow(res),
    2
  )
})

test_that("exclude_operational_noise validates x", {
  expect_error(
    exclude_operational_noise(
      c("a", "b")
    ),
    "x must be a data.frame or tibble"
  )
})

test_that("exclude_operational_noise validates filename column", {
  toy_files <- tibble::tibble(
    file = c("analysis.R")
  )

  expect_error(
    exclude_operational_noise(
      toy_files
    ),
    "Column not found"
  )
})
