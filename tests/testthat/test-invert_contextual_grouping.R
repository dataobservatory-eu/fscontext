test_that(
  "invert_contextual_grouping creates relational projection",
  {
    record_sets <- list(
      conceptualisation = c(
        "D:/_package/alpha",
        "D:/_markdown/alpha-methodology"
      ),
      betaR = c(
        "D:/_packages/beta",
        "D:/_packages/prebeta"
      )
    )

    out <- invert_contextual_grouping(record_sets)

    expect_s3_class(
      out,
      "tbl_df"
    )

    expect_true(
      all(
        c("member", "group") %in% names(out)
      )
    )

    expect_equal(
      nrow(out),
      4
    )
  }
)

test_that(
  "invert_contextual_grouping preserves grouping structure",
  {
    record_sets <- list(
      conceptualisation = c(
        "D:/_package/alpha",
        "D:/_markdown/alpha-methodology"
      ),
      betaR = c(
        "D:/_packages/beta",
        "D:/_packages/prebeta"
      )
    )

    out <- invert_contextual_grouping(record_sets)

    expect_equal(
      out$group,
      c(
        "conceptualisation",
        "conceptualisation",
        "betaR",
        "betaR"
      )
    )

    expect_equal(
      out$member,
      c(
        "D:/_package/alpha",
        "D:/_markdown/alpha-methodology",
        "D:/_packages/beta",
        "D:/_packages/prebeta"
      )
    )
  }
)

test_that(
  "invert_contextual_grouping supports canonical roundtrip",
  {
    record_sets <- list(
      conceptualisation = c(
        "D:/_package/alpha",
        "D:/_markdown/alpha-methodology"
      ),
      betaR = c(
        "D:/_packages/beta",
        "D:/_packages/prebeta"
      )
    )

    roundtrip <- record_sets |>
      invert_contextual_grouping() |>
      as_value_key()

    expect_equal(
      roundtrip,
      as_value_key(record_sets)
    )
  }
)

test_that(
  "invert_contextual_grouping handles singleton groups",
  {
    record_sets <- list(
      alpha = "D:/_package/alpha",
      beta = c(
        "D:/_packages/beta",
        "D:/_packages/prebeta"
      )
    )

    out <- invert_contextual_grouping(record_sets)

    expect_equal(
      nrow(out),
      3
    )

    expect_equal(
      out$group[1],
      "alpha"
    )

    expect_equal(
      out$member[1],
      "D:/_package/alpha"
    )
  }
)
