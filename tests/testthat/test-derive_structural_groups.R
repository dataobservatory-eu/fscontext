# Basic behaviour ------------------------------------------------------

test_that("derive_structural_groups  returns expected structure", {
  rel_path <- c(
    "a/b/c.txt",
    "x/y",
    "single"
  )

  res <- derive_structural_groups(rel_path)

  expect_s3_class(res, "data.frame")

  expect_setequal(names(res), c("structural_group", "component"))

  expect_equal(nrow(res), length(rel_path))
})


# Standard paths -------------------------------------------------------

test_that("derive_structural_groups  parses standard paths correctly", {
  rel_path <- c(
    "_packages/eviota/R/file.R",
    "_markdown/report/analysis.qmd"
  )

  res <- derive_structural_groups(rel_path)

  expect_equal(res$structural_group[1], "_packages/eviota")
  expect_equal(res$component[1], "R")

  expect_equal(res$structural_group[2], "_markdown/report")
  expect_equal(res$component[2], "analysis.qmd")
})


# Edge cases -----------------------------------------------------------

test_that("derive_structural_groups  handles short paths", {
  rel_path <- c(
    "file.R",
    "folder/file.R"
  )

  res <- derive_structural_groups(rel_path)

  # length 1 → no component
  expect_equal(res$structural_group[1], "file.R")
  expect_true(is.na(res$component[1]))

  # length 2 → no component
  expect_equal(res$structural_group[2], "folder/file.R")
  expect_true(is.na(res$component[2]))
})


test_that("derive_structural_groups  handles empty or malformed input", {
  rel_path <- c("", NA)

  res <- derive_structural_groups(rel_path)

  expect_equal(nrow(res), 2)
})


# Determinism ----------------------------------------------------------

test_that("derive_structural_groups  is deterministic", {
  rel_path <- c(
    "a/b/c.txt",
    "x/y/z.txt"
  )

  res1 <- derive_structural_groups(rel_path)
  res2 <- derive_structural_groups(rel_path)

  expect_equal(res1, res2)
})


# Integration with snapshot --------------------------------------------

test_that("derive_structural_groups  works on fscontextdemo_snapshot_02", {
  data(fscontextdemo_snapshot_02, package = "fscontext")

  res <- derive_structural_groups(fscontextdemo_snapshot_02$rel_path)

  expect_equal(nrow(res), nrow(fscontextdemo_snapshot_02))

  expect_true(all(!is.na(res$structural_group)))
})


# Conceptual consistency -----------------------------------------------

test_that("structural_group is consistent with rel_path prefix", {
  rel_path <- c(
    "a/b/c/d.txt",
    "x/y/z.txt"
  )

  res <- derive_structural_groups(rel_path)

  expect_true(all(startsWith(rel_path, res$structural_group)))
})


test_that("derive_group_path handles _packages and packages consistently", {
  x <- c(
    "_packages/iotables/R/a.R",
    "packages/iotables/R/a.R"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c("_packages/iotables/R", "packages/iotables/R")
  )
})


# Basic behaviour ------------------------------------------------------

test_that("derive_structural_groups  returns expected structure", {
  rel_path <- c(
    "a/b/c.txt",
    "x/y",
    "single"
  )

  res <- derive_structural_groups(rel_path)

  expect_s3_class(res, "data.frame")

  expect_setequal(names(res), c("structural_group", "component"))

  expect_equal(nrow(res), length(rel_path))
})


# Standard paths -------------------------------------------------------

test_that("derive_structural_groups  parses standard paths correctly", {
  rel_path <- c(
    "_packages/eviota/R/file.R",
    "_markdown/report/analysis.qmd"
  )

  res <- derive_structural_groups(rel_path)

  expect_equal(res$structural_group[1], "_packages/eviota")
  expect_equal(res$component[1], "R")

  expect_equal(res$structural_group[2], "_markdown/report")
  expect_equal(res$component[2], "analysis.qmd")
})


# Edge cases -----------------------------------------------------------

test_that("derive_structural_groups  handles short paths", {
  rel_path <- c(
    "file.R",
    "folder/file.R"
  )

  res <- derive_structural_groups(rel_path)

  # length 1 → no component
  expect_equal(res$structural_group[1], "file.R")
  expect_true(is.na(res$component[1]))

  # length 2 → no component
  expect_equal(res$structural_group[2], "folder/file.R")
  expect_true(is.na(res$component[2]))
})


test_that("derive_structural_groups  handles empty or malformed input", {
  rel_path <- c("", NA)

  res <- derive_structural_groups(rel_path)

  expect_equal(nrow(res), 2)
})


# Determinism ----------------------------------------------------------

test_that("derive_structural_groups  is deterministic", {
  rel_path <- c(
    "a/b/c.txt",
    "x/y/z.txt"
  )

  res1 <- derive_structural_groups(rel_path)
  res2 <- derive_structural_groups(rel_path)

  expect_equal(res1, res2)
})


# Integration with snapshot --------------------------------------------

test_that("derive_structural_groups  works on fscontextdemo_snapshot_02", {
  data(fscontextdemo_snapshot_02, package = "fscontext")

  res <- derive_structural_groups(fscontextdemo_snapshot_02$rel_path)

  expect_equal(nrow(res), nrow(fscontextdemo_snapshot_02))

  expect_true(all(!is.na(res$structural_group)))
})


# Conceptual consistency -----------------------------------------------

test_that("structural_group is consistent with rel_path prefix", {
  rel_path <- c(
    "a/b/c/d.txt",
    "x/y/z.txt"
  )

  res <- derive_structural_groups(rel_path)

  expect_true(all(startsWith(rel_path, res$structural_group)))
})


test_that("derive_group_path handles _packages and packages consistently", {
  x <- c(
    "_packages/iotables/R/a.R",
    "packages/iotables/R/a.R"
  )

  res <- derive_group_path(x)

  expect_equal(
    res,
    c("_packages/iotables/R", "packages/iotables/R")
  )
})

## WACZ ---------------------------------------------------------------------
test_that("derive_structural_groups handles WACZ package members", {
  rel_path <- c(
    "archive/data.warc.gz",
    "indexes/index.cdx",
    "pages/pages.jsonl",
    "datapackage.json"
  )

  res <- derive_structural_groups(rel_path)

  expect_equal(
    res$structural_group,
    c(
      "archive/data.warc.gz",
      "indexes/index.cdx",
      "pages/pages.jsonl",
      "datapackage.json"
    )
  )

  expect_true(all(is.na(res$component)))
})

test_that("derive_structural_groups works on WACZ observations", {
  wacz <- scan_storage(
    system.file(
      "testdata/fscontext_020.wacz",
      package = "fscontext"
    )
  )

  res <- derive_structural_groups(
    wacz$rel_path
  )

  expect_equal(
    nrow(res),
    nrow(wacz)
  )

  expect_true(
    all(!is.na(res$structural_group))
  )
})

# Profiles -------------------------------------------------------------

test_that("folder-depth-1 groups by first folder level", {
  rel_path <- c(
    "_packages/eviota/R/file.R",
    "_packages/iotables/R/file.R"
  )

  res <- derive_structural_groups(
    rel_path,
    profile = "folder-depth-1"
  )

  expect_equal(
    res$structural_group,
    c("_packages", "_packages")
  )

  expect_equal(
    res$component,
    c("eviota", "iotables")
  )
})

test_that("folder-depth-3 groups by first three folder levels", {
  rel_path <- "_packages/eviota/R/file.R"

  res <- derive_structural_groups(
    rel_path,
    profile = "folder-depth-3"
  )

  expect_equal(
    res$structural_group,
    "_packages/eviota/R"
  )

  expect_equal(
    res$component,
    "file.R"
  )
})

test_that("folder-depth-4 falls back gracefully on short paths", {
  rel_path <- c(
    "archive/data.warc.gz",
    "datapackage.json"
  )

  res <- derive_structural_groups(
    rel_path,
    profile = "folder-depth-4"
  )

  expect_equal(
    res$structural_group,
    rel_path
  )

  expect_true(
    all(is.na(res$component))
  )
})

test_that("unknown profile throws an error", {
  expect_error(
    derive_structural_groups(
      "a/b/c.txt",
      profile = "banana"
    )
  )
})

## WACZ ---------------------------------------------------------------------
## WACZ ----------------------------------------------------------------

test_that("wacz profile derives top-level WACZ aggregations", {
  rel_path <- c(
    "archive/data.warc.gz",
    "indexes/index.cdx",
    "pages/pages.jsonl",
    "datapackage.json"
  )

  res <- derive_structural_groups(
    rel_path,
    profile = "wacz"
  )

  expect_equal(
    res$structural_group,
    c(
      "archive",
      "indexes",
      "pages",
      "datapackage.json"
    )
  )

  expect_equal(
    res$component,
    c(
      "data.warc.gz",
      "index.cdx",
      "pages.jsonl",
      NA_character_
    )
  )
})

test_that("wacz profile works on WACZ observations", {
  wacz <- scan_storage(
    system.file(
      "testdata/fscontext_020.wacz",
      package = "fscontext"
    )
  )

  res <- derive_structural_groups(
    wacz$rel_path,
    profile = "wacz"
  )

  expect_equal(nrow(res), nrow(wacz))

  expect_true(
    all(!is.na(res$structural_group))
  )

  expect_setequal(
    unique(res$structural_group),
    c(
      "archive", "indexes", "pages",
      "datapackage-digest.json", "datapackage.json"
    )
  )
})


test_that("folder-depth-2 and wacz produce different aggregations", {
  rel_path <- "archive/data.warc.gz"

  depth2 <- derive_structural_groups(
    rel_path,
    profile = "folder-depth-2"
  )

  wacz <- derive_structural_groups(
    rel_path,
    profile = "wacz"
  )

  expect_equal(
    depth2$structural_group,
    "archive/data.warc.gz"
  )

  expect_equal(
    wacz$structural_group,
    "archive"
  )
})
