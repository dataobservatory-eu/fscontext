make_minimal_fs <- function() {
  root <- tempfile()
  dir.create(root)

  dir.create(file.path(root, "R"))
  dir.create(file.path(root, "data"))

  file.create(file.path(root, "R", "a.R"))
  file.create(file.path(root, "R", "b.R"))
  file.create(file.path(root, "data", "c.csv"))

  root
}

test_that("snapshot_storage parameter validation", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  expect_error(
    snapshot_storage(
      root = root,
      storage_id = "test-storage",
      path = 1,
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )

  expect_error(
    snapshot_storage(
      root = root,
      storage_id = "test-storage",
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )

  expect_error(
    snapshot_storage(
      root = root,
      storage_id = "test-storage",
      path = NULL,
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )

  expect_error(
    snapshot_storage(
      root = root,
      person_id = 1,
      storage_id = "test-storage",
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )

  expect_error(
    snapshot_storage(
      root = root,
      person_id = 1,
      path = root,
      storage_id = "test-storage",
      label = "tmp"
    ),
    "'person_id' must be a character vector of length 1"
  )

  expect_error(
    snapshot_storage(
      root = root,
      person_id = "testuser",
      path = root,
      storage_id = NULL,
      label = "tmp"
    ),
    "'storage_id' must be a character vector of length 1"
  )
})

test_that("snapshot_storage creates a file", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  out <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    path = tmp
  )

  expect_true(file.exists(out))
})


test_that("snapshot_storage returns full path", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  out <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    path = tmp
  )

  expect_true(is.character(out))
  expect_length(out, 1)
  expect_true(startsWith(out, tmp))
})


test_that("snapshot_storage uses label in filename", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  out <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    label = "My Test Scope",
    path = tmp
  )

  fname <- basename(out)

  expect_true(grepl("my_test_scope", fname))
})


test_that("snapshot_storage saves a valid scan object", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  out <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    path = tmp
  )

  obj <- readRDS(out)

  expect_s3_class(obj, "data.frame")
  expect_true(nrow(obj) > 0)

  expect_true(all(c("full_path", "rel_path", "storage_path_id") %in% names(obj)))
})


test_that("snapshot_storage produces consistent structure", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  out1 <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    path = tmp
  )

  out2 <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    path = tmp
  )

  obj1 <- readRDS(out1)
  obj2 <- readRDS(out2)

  expect_setequal(names(obj1), names(obj2))
  expect_equal(nrow(obj1), nrow(obj2))
})


test_that("snapshot_storage can disable signatures", {
  tmp <- fs::dir_create(fs::file_temp())
  root <- make_minimal_fs()

  out <- snapshot_storage(
    root = root,
    storage_id = "test-storage",
    path = tmp,
    compute_signature = FALSE
  )

  obj <- readRDS(out)

  expect_true(all(is.na(obj$quick_sig)))
})
