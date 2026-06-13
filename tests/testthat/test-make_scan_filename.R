test_that("make_scan_filename returns expected structure without label", {
  ts <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  fname <- make_scan_filename("l480-ssd", ts)

  expect_true(grepl("^scan_l480-ssd_20260430-160059_[a-f0-9]{6}\\.rds$", fname))
})


test_that("make_scan_filename includes label", {
  ts <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  fname <- make_scan_filename("l480-ssd", ts, label = "d_eviota")

  expect_true(grepl("^scan_l480-ssd_d_eviota_20260430-160059_", fname))
})


test_that("make_scan_filename generates different hashes for different times", {
  ts <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  f1 <- make_scan_filename("l480-ssd", ts)
  f2 <- make_scan_filename("l480-ssd", ts + 1)

  expect_false(f1 == f2)
})

test_that("make_scan_filename hash depends on label", {
  ts <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  f1 <- make_scan_filename("l480-ssd", ts, label = "a")
  f2 <- make_scan_filename("l480-ssd", ts, label = "b")

  expect_false(f1 == f2)
})


test_that("make_scan_filename handles empty label", {
  ts <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  f1 <- make_scan_filename("l480-ssd", ts, label = NULL)
  f2 <- make_scan_filename("l480-ssd", ts, label = "")

  expect_equal(f1, f2)
})


test_that("make_scan_filename sanitises label", {
  ts <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  fname <- make_scan_filename(
    "l480-ssd",
    ts,
    label = "D:/My Project!"
  )

  # check that label is normalised correctly
  expect_true(grepl("d_my_project", fname))

  # ensure no illegal characters remain
  expect_false(grepl("[/:! ]", fname))
})
