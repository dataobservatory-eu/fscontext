test_that("construct_structural_paths validates inputs", {
  expect_error(
    construct_structural_paths(
      snapshot = "not_a_dataframe",
      contexts = list()
    ), "must be a data.frame"
  )

  expect_error(
    construct_structural_paths(
      snapshot = tibble::tibble(
        rel_path = "R/test.R"
      ),
      contexts = "not_a_list"
    ), "must be a list"
  )

  expect_error(
    construct_structural_paths(
      snapshot = tibble::tibble(
        rel_path = "R/test.R"
      ),
      contexts = list()
    ),
    "Missing required columns"
  )

  expect_error(
    construct_structural_paths(
      snapshot = tibble::tibble(
        full_path = "D:/pkg/R/test.R"
      ),
      contexts = list()
    ),
    "Missing required columns"
  )

  expect_error(
    construct_structural_paths(
      snapshot = tibble::tibble(
        full_path = "D:/pkg/R/test.R",
        rel_path = "R/test.R"
      ),
      contexts = "not_a_list"
    )
  )
})

test_that(
  "construct_structural_paths creates context-relative recursive structures",
  {
    data("fscontextdemo_snapshot_02")

    mini_context <- list(
      alpha = "D:/_packages/fscontextdemo"
    )

    mini_snapshot <- fscontextdemo_snapshot_02[c(1, 3, 5, 10), ]

    mini_paths <- construct_structural_paths(
      snapshot = mini_snapshot,
      contexts = mini_context
    )

    # ----------------------------------------------------------
    # Structural paths must be context-relative
    # ----------------------------------------------------------

    expect_false(
      any(
        mini_paths$explored_path == "project-alpha"
      )
    )

    # ----------------------------------------------------------
    # Recursive structural expansion must occur
    # ----------------------------------------------------------

    expect_true(
      "docs" %in%
        mini_paths$explored_path
    )

    # ----------------------------------------------------------
    # Context-relative structural paths must be preserved
    # ----------------------------------------------------------

    expect_true(
      "docs/404.html" %in%
        mini_paths$structural_path
    )

    # ----------------------------------------------------------
    # Expected recursive structural layers must exist
    # ----------------------------------------------------------

    expect_true(
      "docs" %in%
        mini_paths$explored_path
    )

    # ----------------------------------------------------------
    # Structural abstraction should remove contextual roots
    # ----------------------------------------------------------

    expect_false(
      any(
        grepl(
          "^fscontextdemo",
          mini_paths$structural_path
        )
      )
    )
  }
)

test_that(
  "construct_structural_paths matches exact contextual roots",
  {
    snapshot <- tibble::tibble(
      full_path = "D:/project",
      rel_path = "project"
    )

    contexts <- list(
      alpha = "D:/project"
    )

    res <- construct_structural_paths(
      snapshot = snapshot,
      contexts = contexts
    )

    expect_s3_class(res, "data.frame")
  }
)

test_that(
  "construct_structural_paths returns empty tibble when no roots match",
  {
    snapshot <- tibble::tibble(
      full_path = "D:/other/file.txt",
      rel_path = "other/file.txt"
    )

    contexts <- list(
      alpha = "D:/project"
    )

    res <- construct_structural_paths(
      snapshot = snapshot,
      contexts = contexts
    )

    expect_equal(
      nrow(res),
      0
    )
  }
)

test_that(
  "construct_structural_paths normalizes backslashes",
  {
    snapshot <- tibble::tibble(
      full_path = "D:\\project\\R\\test.R",
      rel_path = "R\\test.R"
    )

    contexts <- list(
      alpha = "D:/project"
    )

    res <- construct_structural_paths(
      snapshot = snapshot,
      contexts = contexts
    )

    expect_true(
      all(
        !grepl(
          "\\\\",
          res$structural_path
        )
      )
    )
  }
)

test_that(
  "construct_structural_paths does not expand root-level files",
  {
    snapshot <- tibble::tibble(
      full_path = "D:/project/README.md",
      rel_path = "README.md"
    )

    contexts <- list(
      alpha = "D:/project"
    )

    res <- construct_structural_paths(
      snapshot = snapshot,
      contexts = contexts
    )

    expect_equal(
      nrow(res),
      0
    )
  }
)

test_that(
  "construct_structural_paths supports multiple contexts",
  {
    snapshot <- tibble::tibble(
      full_path = c(
        "D:/alpha/R/test.R",
        "D:/beta/tests/testthat/test.R"
      ),
      rel_path = c(
        "R/test.R",
        "tests/testthat/test.R"
      )
    )

    contexts <- list(
      alpha = "D:/alpha",
      beta = "D:/beta"
    )

    res <- construct_structural_paths(
      snapshot = snapshot,
      contexts = contexts
    )

    expect_true(
      all(
        c("alpha", "beta") %in%
          res$context
      )
    )
  }
)
