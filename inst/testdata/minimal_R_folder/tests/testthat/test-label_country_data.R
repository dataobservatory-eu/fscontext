test_that(
  "label_country_data adds semantic labels",
  {

    country_map <- c(
      "Andorra" = "AD",
      "Liechtenstein" = "LI",
      "San Marino" = "SM"
    )

    labelled_df <-
      label_country_data(
        fsdemo_country_data,
        prelabel_map = country_map
      )

    expect_true(
      inherits(
        labelled_df$country,
        "prelabelled"
      )
    )

    expect_equal(
      attr(
        labelled_df$country,
        "prelabel"
      ),
      c(
        "Andorra" = "AD",
        "Liechtenstein" = "LI",
        "San Marino" = "SM",
        "LI" = "LI",
        "SM" = "SM"
      )
    )
  }
)

test_that(
  "label_country_data errors without country column",
  {

    test_df <- data.frame(
      value = 1:3
    )

    expect_error(
      label_country_data(
        test_df,
        prelabel_map = c(
          "Andorra" = "AD"
        )
      ),
      "country column"
    )
  }
)
