test_that(
  "coverage_rules_path matches recursive structural rules",
  {
    small_snapshot <- tibble::tibble(
      full_path = c(
        "D:/packages/fscontext/R/import/helpers.R",
        "D:/packages/fscontext/tests/testthat/test-import.R",
        "D:/packages/fscontext/data-raw/input.csv"
      ),
      rel_path = c(
        "R/import/helpers.R",
        "tests/testthat/test-import.R",
        "data-raw/input.csv"
      )
    )

    small_test_context <- list(
      contexts = list(
        fscontext = list(
          roots =
            "D:/packages/fscontext",
          rules = list(
            path = c(
              "R" =
                "software_development",
              "tests/testthat" =
                "unit_testing",
              "data-raw" =
                "etl"
            )
          )
        )
      )
    )

    res <- coverage_rules_path(
      snapshot = small_snapshot,
      contexts = small_test_context
    )

    matched <- res |>
      dplyr::filter(matched)

    expect_true(
      any(
        matched$activity ==
          "software_development"
      )
    )

    expect_true(
      any(
        matched$activity ==
          "unit_testing"
      )
    )

    expect_true(
      any(
        matched$activity ==
          "etl"
      )
    )
  }
)
