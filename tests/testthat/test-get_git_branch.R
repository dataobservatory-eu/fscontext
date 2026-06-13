test_that("get_git_branch returns branch name", {
  tmp <- fs::dir_create(fs::path_temp("repo_test_branch"))

  git_dir <- fs::dir_create(fs::path(tmp, ".git"))

  head <- fs::path(git_dir, "HEAD")

  writeLines("ref: refs/heads/main", head)

  res <- get_git_branch(tmp)

  expect_equal(res, "main")
})

test_that("get_git_branch detects detached HEAD", {
  tmp <- fs::dir_create(fs::path_temp("repo_test_detached"))

  git_dir <- fs::dir_create(fs::path(tmp, ".git"))

  head <- fs::path(git_dir, "HEAD")

  writeLines("abc123def456", head)

  res <- get_git_branch(tmp)

  expect_equal(res, "DETACHED")
})

test_that("get_git_branch returns NA if HEAD missing", {
  tmp <- fs::dir_create(fs::path_temp("repo_test_nohead"))

  fs::dir_create(fs::path(tmp, ".git"))

  res <- get_git_branch(tmp)

  expect_true(is.na(res))
})
