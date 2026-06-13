test_that("create_observation_set validates inputs", {
  expect_error(
    create_observation_set(
      snapshot_files = 1,
      roots = "D:/research"
    )
  )

  expect_error(
    create_observation_set(
      snapshot_files = "test.rds",
      roots = 1
    )
  )
})
