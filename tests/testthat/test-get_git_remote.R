test_that("get_git_remote extracts origin URL", {
  tmp <- fs::dir_create(fs::path_temp("repo_test"))

  git_dir <- fs::dir_create(fs::path(tmp, ".git"))

  config <- fs::path(git_dir, "config")

  writeLines(c(
    "[core]",
    "repositoryformatversion = 0",
    "",
    "[remote \"origin\"]",
    "    url = https://github.com/user/repo.git"
  ), config)

  res <- get_git_remote(tmp)

  expect_equal(res, "https://github.com/user/repo.git")
})

test_that("get_git_remote returns NA if no config", {
  tmp <- fs::dir_create(fs::path_temp("repo_test_empty"))

  res <- get_git_remote(tmp)

  expect_true(is.na(res))
})
