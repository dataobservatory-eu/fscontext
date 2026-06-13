test_that("quick_signature returns a character string", {
  tmp <- fs::file_temp()
  writeBin(charToRaw("hello world"), tmp)

  sig <- quick_signature(tmp)

  expect_type(sig, "character")
  expect_length(sig, 1)
})

test_that("quick_signature identical files have same signature", {
  tmp1 <- fs::file_temp()
  tmp2 <- fs::file_temp()

  content <- charToRaw(paste(rep("abc", 1000), collapse = ""))

  writeBin(content, tmp1)
  writeBin(content, tmp2)

  sig1 <- quick_signature(tmp1)
  sig2 <- quick_signature(tmp2)

  expect_equal(sig1, sig2)
})


test_that("quick_signature different files have different signatures", {
  tmp1 <- fs::file_temp()
  tmp2 <- fs::file_temp()

  writeBin(charToRaw("hello world"), tmp1)
  writeBin(charToRaw("hello world!"), tmp2)

  sig1 <- quick_signature(tmp1)
  sig2 <- quick_signature(tmp2)

  expect_false(sig1 == sig2)
})


test_that("quick_signature handles small files correctly", {
  tmp <- fs::file_temp()

  writeBin(charToRaw("small"), tmp)

  sig <- quick_signature(tmp, n = 1024)

  expect_type(sig, "character")
  expect_length(sig, 1)
})


test_that("quick_signature uses both start and end of large files", {
  tmp1 <- fs::file_temp()
  tmp2 <- fs::file_temp()

  # same prefix, different suffix
  content1 <- c(
    charToRaw(paste(rep("A", 2000), collapse = "")),
    charToRaw("END1")
  )

  content2 <- c(
    charToRaw(paste(rep("A", 2000), collapse = "")),
    charToRaw("END2")
  )

  writeBin(content1, tmp1)
  writeBin(content2, tmp2)

  sig1 <- quick_signature(tmp1, n = 1024)
  sig2 <- quick_signature(tmp2, n = 1024)

  expect_false(sig1 == sig2)
})


test_that("quick_signature is deterministic", {
  tmp <- fs::file_temp()
  writeBin(charToRaw("repeatable content"), tmp)

  sig1 <- quick_signature(tmp)
  sig2 <- quick_signature(tmp)

  expect_equal(sig1, sig2)
})
