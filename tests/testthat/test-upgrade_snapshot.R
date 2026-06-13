library(testthat)

test_that("upgrade_snapshot upgrades schema version", {
  data(schema_012_sample)

  upgraded <- upgrade_snapshot(schema_012_sample)

  expect_equal(
    attr(upgraded, "schema_version"),
    "0.1.3"
  )
})

test_that("upgrade_snapshot upgrades full_path semantics", {
  data(schema_012_sample)

  upgraded <- upgrade_snapshot(schema_012_sample)

  expect_true(
    all(grepl(
      "^l460-broken-ssd::",
      upgraded$full_path
    ))
  )
})

test_that("upgrade_snapshot is idempotent", {
  data(schema_012_sample)

  upgraded_once <- upgrade_snapshot(schema_012_sample)

  upgraded_twice <- upgrade_snapshot(upgraded_once)

  expect_identical(
    upgraded_once$full_path,
    upgraded_twice$full_path
  )

  expect_identical(
    upgraded_once$local_path,
    upgraded_twice$local_path
  )
})

test_that("upgrade_snapshot preserves row count", {
  data(schema_012_sample)

  upgraded <- upgrade_snapshot(schema_012_sample)

  expect_equal(
    nrow(upgraded),
    nrow(schema_012_sample)
  )
})
