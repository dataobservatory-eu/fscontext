test_that(
  "context_roots extracts normalized roots from context objects",
  {
    mini_context <- list(
      title = "Minimal reconstruction example",
      version = "0.1.0",
      record_set_id = "minimal_example",
      contexts = list(
        packages = list(
          roots = c(
            "D:/packages/examplepkg",
            "C:/packages/examplepkg"
          ),
          role = "Example package development"
        ),
        research = list(
          roots = c(
            "D:/research/projectA"
          ),
          role = "Example research workflow"
        )
      )
    )

    res <- context_roots(mini_context)

    expect_type(
      res,
      "character"
    )

    expect_true(
      all(
        c(
          "D:/packages/examplepkg",
          "C:/packages/examplepkg",
          "D:/research/projectA"
        ) %in% res
      )
    )

    expect_false(
      any(
        grepl("\\\\", res)
      )
    )

    expect_false(
      any(
        grepl("/$", res)
      )
    )
  }
)
