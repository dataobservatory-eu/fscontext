test_that("derive_reuse_metrics derives reuse statistics", {
  toy_resources <- tibble::tibble(
    resource_id = c(
      "res_001",
      "res_001",
      "res_002"
    ),
    record_set_id = c(
      "project_a",
      "project_b",
      "project_a"
    ),
    storage_path_id = c(
      "laptop::analysis.R",
      "backup::analysis.R",
      "laptop::report.qmd"
    ),
    mtime = as.POSIXct(c(
      "2025-01-01",
      "2025-01-03",
      "2025-01-02"
    )),
    full_path = c(
      "D:/project/analysis.R",
      "E:/backup/analysis.R",
      "D:/project/report.qmd"
    )
  )

  res <- derive_reuse_metrics(
    toy_resources
  )

  expect_s3_class(
    res,
    "data.frame"
  )

  expect_equal(
    nrow(res),
    2
  )

  expect_true(
    all(c(
      "n_observations",
      "n_record_sets",
      "n_paths",
      "first_seen",
      "last_seen",
      "locations"
    ) %in% names(res))
  )
})

test_that("derive_reuse_metrics computes reuse correctly", {
  toy_resources <- tibble::tibble(
    resource_id = c(
      "res_001",
      "res_001"
    ),
    record_set_id = c(
      "project_a",
      "project_b"
    ),
    storage_path_id = c(
      "path_1",
      "path_2"
    ),
    mtime = as.POSIXct(c(
      "2025-01-01",
      "2025-01-03"
    )),
    full_path = c(
      "D:/analysis.R",
      "E:/analysis.R"
    )
  )

  res <- derive_reuse_metrics(
    toy_resources
  )

  expect_equal(
    res$n_observations,
    2
  )

  expect_equal(
    res$n_record_sets,
    2
  )

  expect_equal(
    res$n_paths,
    2
  )

  expect_equal(
    as.character(res$first_seen),
    "2025-01-01"
  )

  expect_equal(
    as.character(res$last_seen),
    "2025-01-03"
  )
})

test_that("derive_reuse_metrics validates x", {
  expect_error(
    derive_reuse_metrics(
      c("a", "b")
    ),
    "x must be a data.frame or tibble"
  )
})

test_that("derive_reuse_metrics validates required columns", {
  toy_resources <- tibble::tibble(
    resource = c("a")
  )

  expect_error(
    derive_reuse_metrics(
      toy_resources
    ),
    "Missing columns"
  )
})
