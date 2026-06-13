test_that("hello_world returns English greeting", {
  expect_equal(
    hello_world(),
    "Hello world!"
  )

  expect_equal(
    hello_world("en"),
    "Hello world!"
  )
})

test_that("hello_world returns Hungarian greeting", {
  expect_equal(
    hello_world("hu"),
    "Helló világ!"
  )
})

test_that("hello_world errors on unsupported language", {
  expect_error(
    hello_world("de"),
    "Unrecognised language code"
  )
})
