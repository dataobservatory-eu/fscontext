library(testthat)

# ------------------------------------------------------------------
# standardize_context_roots()
# ------------------------------------------------------------------

test_that("standardize_context_roots returns character vectors unchanged", {
  roots <- c(
    "D:/_packages/eviota",
    "D:/_packages/iotables"
  )

  out <- standardize_context_roots(roots)

  expect_equal(out, roots)
})

test_that("standardize_context_roots extracts roots from context objects", {
  context_obj <- list(
    contexts = list(
      packages = list(
        roots = c(
          "D:/_packages/eviota",
          "D:/_packages/iotables"
        )
      ),
      eviota = list(
        roots = c(
          "D:/_eviota/filmledgerimport"
        )
      )
    )
  )

  out <- standardize_context_roots(context_obj)

  expect_equal(
    out,
    c(
      "D:/_packages/eviota",
      "D:/_packages/iotables",
      "D:/_eviota/filmledgerimport"
    )
  )
})

test_that(
  "standardize_context_roots errors on unsupported input",
  {
    expect_error(
      standardize_context_roots(42),
      "roots must be either"
    )
  }
)

# ------------------------------------------------------------------
# flatten_context_roots()
# ------------------------------------------------------------------

test_that("flatten_context_roots flattens context roots", {
  contexts <- list(
    packages = list(
      roots = c(
        "D:/_packages/eviota",
        "D:/_packages/iotables"
      )
    ),
    eviota = list(
      roots = c(
        "D:/_eviota/filmledgerimport"
      )
    )
  )

  out <- flatten_context_roots(contexts)

  expect_equal(
    out,
    c(
      "D:/_packages/eviota",
      "D:/_packages/iotables",
      "D:/_eviota/filmledgerimport"
    )
  )
})


test_that("flatten_context_roots returns character vector", {
  contexts <- list(
    a = list(
      roots = "D:/test"
    )
  )

  out <- flatten_context_roots(contexts)

  expect_true(is.character(out))
})


# ------------------------------------------------------------------
# normalize_context_roots()
# ------------------------------------------------------------------

test_that("normalize_context_roots converts backslashes", {
  roots <- c(
    "D:\\_packages\\eviota",
    "D:\\_packages\\iotables"
  )

  out <- normalize_context_roots(roots)

  expect_equal(
    out,
    c(
      "D:/_packages/eviota",
      "D:/_packages/iotables"
    )
  )
})


test_that("normalize_context_roots removes trailing slashes", {
  roots <- c(
    "D:/_packages/eviota/",
    "D:/_packages/iotables///"
  )

  out <- normalize_context_roots(roots)

  expect_equal(
    out,
    c(
      "D:/_packages/eviota",
      "D:/_packages/iotables"
    )
  )
})


test_that("normalize_context_roots errors on non-character input", {
  expect_error(
    normalize_context_roots(1),
    "is.character"
  )
})


# ------------------------------------------------------------------
# assert_unique_context_roots()
# ------------------------------------------------------------------

test_that("assert_unique_context_roots passes on unique roots", {
  roots <- c(
    "D:/_packages/eviota",
    "D:/_packages/iotables"
  )

  expect_invisible(
    assert_unique_context_roots(roots)
  )
})


test_that("assert_unique_context_roots errors on duplicated roots", {
  roots <- c(
    "D:/_packages/eviota",
    "D:/_packages/eviota"
  )

  expect_error(
    assert_unique_context_roots(roots),
    "Duplicated roots"
  )
})


# ------------------------------------------------------------------
# prepare_context_roots()
# ------------------------------------------------------------------

test_that("prepare_context_roots standardizes and normalizes roots", {
  roots <- c(
    "D:\\_packages\\eviota\\",
    "D:\\_packages\\iotables///"
  )

  out <- prepare_context_roots(roots)

  expect_equal(
    out,
    c(
      "D:/_packages/eviota",
      "D:/_packages/iotables"
    )
  )
})


test_that("prepare_context_roots works with context objects", {
  context_obj <- list(
    contexts = list(
      packages = list(
        roots = c(
          "D:\\_packages\\eviota\\",
          "D:\\_packages\\iotables///"
        )
      )
    )
  )

  out <- prepare_context_roots(context_obj)

  expect_equal(
    out,
    c(
      "D:/_packages/eviota",
      "D:/_packages/iotables"
    )
  )
})


test_that("prepare_context_roots errors on duplicated roots", {
  roots <- c(
    "D:/_packages/eviota",
    "D:/_packages/eviota"
  )

  expect_error(
    prepare_context_roots(roots),
    "Duplicated roots"
  )
})

# ------------------------------------------------------------------
# path_depth()
# ------------------------------------------------------------------

test_that(
  "path_depth calculates filesystem depth",
  {
    x <- c(
      "D:",
      "D:/packages",
      "D:/packages/eviota",
      "D:/packages/eviota/R"
    )

    expect_equal(
      path_depth(x),
      c(1, 2, 3, 4)
    )
  }
)


test_that(
  "path_depth normalizes backslashes",
  {
    x <- c(
      "D:\\packages",
      "D:\\packages\\eviota"
    )

    expect_equal(
      path_depth(x),
      c(2, 3)
    )
  }
)


test_that(
  "path_depth removes trailing slashes",
  {
    x <- c(
      "D:/packages/",
      "D:/packages/eviota///"
    )

    expect_equal(
      path_depth(x),
      c(2, 3)
    )
  }
)


test_that(
  "path_depth works on single paths",
  {
    expect_equal(
      path_depth(
        "D:/packages/eviota"
      ),
      3
    )
  }
)


test_that(
  "path_depth returns integer vector",
  {
    out <- path_depth(
      c(
        "D:/packages",
        "D:/packages/eviota"
      )
    )

    expect_true(
      is.integer(out)
    )
  }
)


test_that(
  "path_depth preserves vector length",
  {
    x <- c(
      "D:/a",
      "D:/a/b",
      "D:/a/b/c"
    )

    expect_equal(
      length(path_depth(x)),
      length(x)
    )
  }
)

# ------------------------------------------------------------------
# normalize_git_remote()
# ------------------------------------------------------------------

test_that("normalize_git_remote normalizes Git remotes", {
  x <- c(
    "git@github.com:example/project-alpha.git",
    "https://github.com/example/project-beta.git",
    "https://github.com/example/project-gamma/",
    "https://github.com/example/project-delta"
  )

  res <- normalize_git_remote(x)

  expect_equal(
    res,
    c(
      "https://github.com/example/project-alpha",
      "https://github.com/example/project-beta",
      "https://github.com/example/project-gamma",
      "https://github.com/example/project-delta"
    )
  )
})

test_that("normalize_git_remote preserves NA values", {
  x <- c(
    NA_character_,
    "https://github.com/example/project-alpha.git"
  )

  res <- normalize_git_remote(x)

  expect_true(is.na(res[1]))

  expect_equal(
    res[2],
    "https://github.com/example/project-alpha"
  )
})

test_that("normalize_git_remote returns character vector", {
  x <- "https://github.com/example/project-alpha.git"

  res <- normalize_git_remote(x)

  expect_type(res, "character")

  expect_length(res, 1)
})
