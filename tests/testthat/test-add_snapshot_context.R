library(testthat)

test_that("add_snapshot_context adds contextual columns", {
  data("schema_012_sample")

  res <- add_snapshot_context(schema_012_sample)

  expect_true("storage_full_path" %in% names(res))
  expect_true("storage_path_id" %in% names(res))
  expect_true("observation_id" %in% names(res))
})

test_that("storage_full_path combines storage and full path", {
  data("schema_012_sample")

  res <- add_snapshot_context(schema_012_sample)

  expect_equal(
    res$storage_full_path[1],
    paste(
      res$storage_id[1],
      res$full_path[1],
      sep = "::"
    )
  )
})

test_that("storage_path_id combines storage and relative path", {
  data("schema_012_sample")

  res <- add_snapshot_context(schema_012_sample)

  expect_equal(
    res$storage_path_id[1],
    paste(
      res$storage_id[1],
      res$rel_path[1],
      sep = "::"
    )
  )
})

test_that("observation_id includes scan time", {
  data("schema_012_sample")

  res <- add_snapshot_context(schema_012_sample)

  expected <- paste(
    res$storage_id[1],
    res$rel_path[1],
    format(res$scan_time[1], "%Y%m%d-%H%M%S"),
    sep = "::"
  )

  expect_equal(
    res$observation_id[1],
    expected
  )
})

test_that("observation_id distinguishes repeated observations", {
  tmp <- tempfile()
  dir.create(tmp)

  file.create(file.path(tmp, "test.txt"))

  s1 <- scan_storage(
    root = tmp,
    storage_id = "test-storage",
    scan_time = as.POSIXct("2026-01-01 10:00:00")
  )

  s2 <- scan_storage(
    root = tmp,
    storage_id = "test-storage",
    scan_time = as.POSIXct("2026-01-02 10:00:00")
  )

  combined <- dplyr::bind_rows(s1, s2)

  res <- add_snapshot_context(combined)

  expect_true(any(duplicated(res$storage_path_id)))

  expect_false(any(duplicated(res$observation_id)))
})

test_that("add_snapshot_context preserves row count", {
  data("schema_012_sample")

  res <- add_snapshot_context(schema_012_sample)

  expect_equal(
    nrow(res),
    nrow(schema_012_sample)
  )
})
