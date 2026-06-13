test_that(
  "commit_select returns all rows when below max_commits",
  {
    commits <- data.frame(
      commit_time = Sys.time() + 1:5
    )

    res <- commit_select(commits, max_commits = 10)

    expect_equal(nrow(res), 5)
  }
)


test_that(
  "commit_select truncates to max_commits",
  {
    commits <- data.frame(
      commit_time = Sys.time() + 1:10
    )

    res <- commit_select(commits, max_commits = 3)

    expect_equal(nrow(res), 3)
  }
)


test_that(
  "commit_select filters by since",
  {
    commits <- data.frame(
      commit_time = as.POSIXct(
        c(
          "2025-01-01",
          "2025-06-01",
          "2025-12-01"
        ),
        tz = "UTC"
      )
    )

    res <- commit_select(commits, max_commits = 1, since = "2025-05-01")

    expect_equal(nrow(res), 2)

    expect_true(
      all(
        res$commit_time >=
          as.POSIXct(
            "2025-05-01",
            tz = "UTC"
          )
      )
    )
  }
)
