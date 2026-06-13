test_that("detect_generated_artifacts detects generated artifacts", {
  toy_files <- tibble::tibble(
    filename = c(
      ".Rhistory",
      "app.css",
      "analysis.R",
      "font.woff2"
    ),
    extension = c(
      "",
      "css",
      "R",
      "woff2"
    )
  )

  res <- detect_generated_artifacts(
    toy_files
  )

  expect_equal(
    res,
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("detect_generated_artifacts validates input", {
  expect_error(
    detect_generated_artifacts(
      c("R")
    ),
    "x must be a data.frame or tibble"
  )
})

test_that("detect_generated_artifacts validates columns", {
  toy_files <- tibble::tibble(
    file = c("test.R")
  )

  expect_error(
    detect_generated_artifacts(
      toy_files
    ),
    "Column not found"
  )
})
