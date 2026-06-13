test_that(
  "compile_rulebook() validates required columns",
  {
    invalid_rulebook <- data.frame(
      refine_id = "refine_1",
      variable = "extension",
      pattern = "png",
      stringsAsFactors = FALSE
    )

    expect_error(
      compile_rulebook(invalid_rulebook),
      "Missing required columns"
    )
  }
)

test_that(
  "compile_rulebook() compiles grouped refinement rules",
  {
    rulebook <- data.frame(
      refine_id = c(
        "refine_1",
        "refine_1",
        "refine_2"
      ),
      variable = c(
        "extension",
        "filename",
        "extension"
      ),
      match = c(
        "exact",
        "starts_with",
        "exact"
      ),
      pattern = c(
        "png",
        "film",
        "csv"
      ),
      refined_assertion = c(
        "visualisation",
        "visualisation",
        "tabular_data"
      ),
      stringsAsFactors = FALSE
    )

    compiled <- compile_rulebook(rulebook)

    expect_type(
      compiled,
      "list"
    )

    expect_length(
      compiled,
      2
    )

    expect_named(
      compiled[[1]],
      c(
        "refine_id",
        "rules",
        "by",
        "match",
        "assertion"
      )
    )

    expect_equal(
      compiled[[1]]$refine_id,
      "refine_1"
    )

    expect_equal(
      compiled[[1]]$by,
      c(
        "extension",
        "filename"
      )
    )

    expect_equal(
      compiled[[1]]$match,
      c(
        "exact",
        "starts_with"
      )
    )

    expect_equal(
      compiled[[1]]$assertion,
      "visualisation"
    )

    expect_equal(
      compiled[[2]]$assertion,
      "tabular_data"
    )
  }
)

test_that(
  "compile_rulebook() creates rule tables correctly",
  {
    rulebook <- data.frame(
      refine_id = c(
        "refine_1",
        "refine_1"
      ),
      variable = c(
        "extension",
        "filename"
      ),
      match = c(
        "exact",
        "starts_with"
      ),
      pattern = c(
        "png",
        "film"
      ),
      refined_assertion = c(
        "visualisation",
        "visualisation"
      ),
      stringsAsFactors = FALSE
    )

    compiled <- compile_rulebook(rulebook)

    rules_tbl <- compiled[[1]]$rules

    expect_s3_class(
      rules_tbl,
      "data.frame"
    )

    expect_equal(
      names(rules_tbl),
      c(
        "extension",
        "filename"
      )
    )

    expect_equal(
      rules_tbl$extension,
      "png"
    )

    expect_equal(
      rules_tbl$filename,
      "film"
    )
  }
)
