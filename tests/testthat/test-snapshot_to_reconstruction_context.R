test_that("snapshot_to_reconstruction_context reconstructs expected columns", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  snapshot_files <- file.path(
    tempdir(),
    c("fscontextdemo_snapshot_01.rds", "fscontextdemo_snapshot_02.rds")
  )

  saveRDS(fscontextdemo_snapshot_01, snapshot_files[1])
  saveRDS(fscontextdemo_snapshot_02, snapshot_files[2])

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1:2]

  out <- snapshot_to_reconstruction_context(
    snapshot_files = snapshot_files,
    roots = roots
  )

  expect_s3_class(out, "data.frame")

  expect_true(all(c(
    "storage_id",
    "person_id",
    "full_path",
    "rel_path",
    "filename",
    "storage_path_id",
    "observation_id",
    "structural_group",
    "component",
    "record_set_id",
    "resource_id",
    "locator_path"
  ) %in% names(out)))

  expect_gt(nrow(out), 0)
})

test_that("record_set_id is never missing", {
  data("fscontextdemo_snapshot_02")

  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, f)

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1:3]

  out <- snapshot_to_reconstruction_context(
    snapshot_files = f,
    roots = roots
  )

  expect_false(any(is.na(out$record_set_id)))
  expect_false(any(out$record_set_id == ""))
})


test_that("resource_id is never missing", {
  data("fscontextdemo_snapshot_02")

  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, f)

  roots <- c(
    "D:/_packages/fscontextdemo/R",
    "D:/_packages/fscontextdemo/man"
  )

  out <- snapshot_to_reconstruction_context(
    snapshot_files = f,
    roots = roots
  )

  expect_false(any(is.na(out$resource_id)))
  expect_false(any(out$resource_id == ""))
})


test_that("locator_path equals rel_path", {
  data("fscontextdemo_snapshot_02")

  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, f)

  roots <- c(
    "D:/_packages/fscontextdemo/R",
    "D:/_packages/fscontextdemo/man"
  )

  out <- snapshot_to_reconstruction_context(
    snapshot_files = f,
    roots = roots
  )

  expect_equal(
    out$locator_path,
    out$rel_root_path
  )
})


test_that("observation_id is unique", {
  data("fscontextdemo_snapshot_02")

  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, f)

  roots <- c(
    "D:/_packages/fscontextdemo/R",
    "D:/_packages/fscontextdemo/man"
  )

  out <- snapshot_to_reconstruction_context(
    snapshot_files = f,
    roots = roots
  )

  expect_equal(
    length(unique(out$observation_id)),
    nrow(out)
  )
})

test_that("roots filter observations", {
  data("fscontextdemo_snapshot_02")
  
  f <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, f)
  
  roots <- c(
    "D:/_packages/fscontextdemo/R",
    "D:/_packages/fscontextdemo/man"
  )
  
  out <- snapshot_to_reconstruction_context(
    snapshot_files = f,
    roots = roots
  )
  
  matches_root <- vapply(out$full_path, 
                         \(x) any(startsWith(x, roots)), logical(1))
  
  expect_true(all(matches_root))
  
})

test_that(
  "empty root selection errors clearly",
  {
    data(
      fscontextdemo_snapshot_02,
      package = "fscontext"
    )

    f <- tempfile(fileext = ".rds")

    saveRDS(
      fscontextdemo_snapshot_02,
      f
    )

    expect_error(
      snapshot_to_reconstruction_context(
        snapshot_files = f,
        roots =
          "D:/research/does_not_exist"
      ),
      "No filesystem observations matched the supplied contextual roots"
    )
  }
)

