test_that("coverage_rules_path matches recursive structural rules", {
  small_snapshot <- tibble::tibble(
    full_path = c(
      "D:/proj/demo/R/a.R",
      "D:/proj/demo/tests/testthat/b.R",
      "D:/proj/demo/data-raw/c.csv"
    ),
    rel_path = c(
      "R/a.R",
      "tests/testthat/b.R",
      "data-raw/c.csv"
    )
  )

  small_test_context <- list(
    contexts = list(
      demo = list(
        roots = "D:/proj/demo",
        rules = list(
          path = c(
            "R" = "software_development",
            "tests/testthat" = "unit_testing",
            "data-raw" = "etl"
          )
        )
      )
    )
  )

  res <- coverage_rules_path(
    snapshot = small_snapshot,
    contexts = small_test_context
  )

  matched <- dplyr::filter(res, matched)

  expect_true(any(matched$activity == "software_development"))
  expect_true(any(matched$activity == "unit_testing"))
  expect_true(any(matched$activity == "etl"))
})
