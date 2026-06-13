test_that(
  "project_record_events creates filesystem event journal",
  {
    data("fscontextdemo_snapshot_02")

    snapshot_df <- fscontextdemo_snapshot_02

    result <-
      project_record_events(snapshot_df)

    expect_s3_class(
      result,
      "data.frame"
    )

    expect_true(
      nrow(result) > 0
    )

    expect_true(
      all(
        c(
          "storage_id",
          "full_path",
          "filename",
          "extension",
          "size",
          "quick_sig",
          "time_type",
          "event_time",
          "event_date",
          "year",
          "month",
          "day",
          "event_type",
          "event_source",
          "event_evidence",
          "event_actor",
          "resource_name",
          "resource_id",
          "observation_time"
        ) %in% names(result)
      )
    )

    expect_true(
      all(
        result$event_source == "filesystem"
      )
    )

    expect_true(
      all(
        result$event_actor ==
          "unknown_local_user"
      )
    )

    expect_true(
      all(
        result$event_type %in%
          c(
            "birth_time",
            "ctime",
            "mtime",
            "atime"
          )
      )
    )

    expect_true(
      all(
        result$event_type ==
          result$event_evidence
      )
    )

    expect_true(
      all(
        result$resource_name ==
          result$filename
      )
    )

    expect_true(
      all(
        result$resource_id ==
          result$full_path
      )
    )

    expect_true(
      inherits(
        result$event_time,
        "POSIXct"
      )
    )
  }
)
