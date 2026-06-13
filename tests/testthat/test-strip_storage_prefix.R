test_that("strip_storage_prefix removes storage prefix", {
  x <- "l460-broken-ssd::C:/_packages/eviota/R/file.R"

  expect_equal(
    strip_storage_prefix(x),
    "C:/_packages/eviota/R/file.R"
  )
})

test_that("strip_storage_prefix handles vectors", {
  x <- c(
    "l460::C:/test.txt",
    "l480::D:/data.csv"
  )

  expect_equal(
    strip_storage_prefix(x),
    c(
      "C:/test.txt",
      "D:/data.csv"
    )
  )
})

test_that("strip_storage_prefix leaves plain paths unchanged", {
  x <- "C:/_packages/eviota/R/file.R"

  expect_equal(
    strip_storage_prefix(x),
    x
  )
})

test_that("strip_storage_prefix handles NA values", {
  x <- c(
    "l460::C:/test.txt",
    NA_character_
  )

  expect_equal(
    strip_storage_prefix(x),
    c(
      "C:/test.txt",
      NA_character_
    )
  )
})
