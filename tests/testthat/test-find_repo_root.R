test_that("find_repo_root returns nearest matching repo", {
  repos <- c("/proj", "/proj/sub")

  res <- find_repo_root("/proj/sub/file.R", repos)

  expect_equal(res, "/proj/sub")
})

test_that("find_repo_root returns NA when no match", {
  repos <- c("/proj", "/proj/sub")

  res <- find_repo_root("/other/file.R", repos)

  expect_true(is.na(res))
})


test_that("find_repo_root does not match partial prefixes", {
  repos <- c("/proj")

  res <- find_repo_root("/project/file.R", repos)

  expect_true(is.na(res))
})
