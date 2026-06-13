test_that(
  "validate_journal validates correct journal objects",
  {
    data("fscontextdemo_snapshot_02")

    snapshot_df <- fscontextdemo_snapshot_02

    events <-
      project_record_events(snapshot_df)

    expect_invisible(
      validate_journal(events)
    )
  }
)

test_that(
  "validate_journal errors on non-data.frame input",
  {
    expect_error(
      validate_journal("not_a_dataframe"),
      "x must be a data.frame"
    )
  }
)

test_that(
  "validate_journal errors on missing required columns",
  {
    data("fscontextdemo_snapshot_02")

    snapshot_df <- fscontextdemo_snapshot_02

    events <-
      project_record_events(snapshot_df)

    invalid_events <-
      events %>%
      dplyr::select(
        -resource_id
      )

    expect_error(
      validate_journal(invalid_events),
      "Missing required columns"
    )
  }
)
