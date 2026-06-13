test_that(
  "subset_snapshot works on fscontextdemo_snapshot_02",
  {
    data(fscontextdemo_snapshot_02, package = "fscontext")

    tmp <- tempfile(fileext = ".rds")

    saveRDS(
      fscontextdemo_snapshot_02,
      tmp
    )

    res <- subset_snapshot(
      snapshot_path = tmp,
      folder_path =
        "D:/_packages/fscontextdemo"
    )

    expect_s3_class(
      res,
      "data.frame"
    )

    expect_true(
      nrow(res) > 0
    )

    expect_true(
      "rel_root_path" %in%
        names(res)
    )
  }
)

# Extension filtering ------------------------------

test_that(
  "subset_snapshot filters by extension",
  {
    data(
      fscontextdemo_snapshot_02,
      package = "fscontext"
    )

    tmp <- tempfile(fileext = ".rds")

    saveRDS(
      fscontextdemo_snapshot_02,
      tmp
    )

    res <- subset_snapshot(
      snapshot_path = tmp,
      folder_path =
        "D:/_packages/fscontextdemo",
      extensions = "html"
    )

    expect_true(
      all(
        tolower(res$extension) ==
          "html"
      )
    )
  }
)
# Select multiple folders -----------------------------

test_that(
  "subset_snapshot handles multiple folders",
  {
    data(
      fscontextdemo_snapshot_02,
      package = "fscontext"
    )

    tmp <- tempfile(fileext = ".rds")

    saveRDS(
      fscontextdemo_snapshot_02,
      tmp
    )

    res <- subset_snapshot(
      snapshot_path = tmp,
      folder_path = c(
        "D:/_packages/fscontextdemo",
        "D:/research/project-beta"
      )
    )

    expect_true(
      nrow(res) > 0
    )
  }
)

# Relative path --------------------------------

test_that(
  "rel_root_path is relative",
  {
    data(
      fscontextdemo_snapshot_02,
      package = "fscontext"
    )

    tmp <- tempfile(fileext = ".rds")

    saveRDS(
      fscontextdemo_snapshot_02,
      tmp
    )

    res <- subset_snapshot(
      snapshot_path = tmp,
      folder_path =
        "D:/_packages/fscontextdemo"
    )

    expect_false(
      any(
        startsWith(
          res$rel_root_path,
          "D:/"
        )
      )
    )
  }
)

# Exclusion works ------------------------------


test_that("subset_snapshot excludes patterns", {
  df <- data.frame(
    full_path = c(
      "D:/ROOT/_packages/a.R",
      "D:/ROOT/_packages/x.Rcheck/file.R"
    ),
    extension = c("r", "r"),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".rds")
  saveRDS(df, tmp)

  res <- subset_snapshot(
    snapshot_path = tmp,
    folder_path = "D:/ROOT/_packages",
    exclude_patterns = "\\.Rcheck"
  )

  expect_equal(nrow(res), 1)
})


# Empty result is safe ----------------------------

test_that("subset_snapshot handles empty result", {
  data(fscontextdemo_snapshot_02, package = "fscontext")

  tmp <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, tmp)

  res <- subset_snapshot(
    snapshot_path = tmp,
    folder_path = "D:/ROOT/does_not_exist"
  )

  expect_equal(nrow(res), 0)
})


# Attribute test ---------------------------------------------------

test_that(
  "subset_snapshot preserves provenance attributes",
  {
    data(
      fscontextdemo_snapshot_02,
      package = "fscontext"
    )

    tmp <- tempfile(fileext = ".rds")

    saveRDS(
      fscontextdemo_snapshot_02,
      tmp
    )

    res <- subset_snapshot(
      snapshot_path = tmp,
      folder_path =
        "D:/_packages/fscontextdemo"
    )

    expect_identical(
      attr(res, "created_by"),
      attr(
        fscontextdemo_snapshot_02,
        "created_by"
      )
    )

    expect_identical(
      attr(res, "created_at"),
      attr(
        fscontextdemo_snapshot_02,
        "created_at"
      )
    )

    expect_equal(
      attr(res, "derived_by"),
      "subset_snapshot"
    )

    expect_true(
      inherits(
        attr(res, "derived_at"),
        "POSIXct"
      )
    )
  }
)
