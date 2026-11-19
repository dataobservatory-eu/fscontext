test_that("save_scan requires a path", {
  df <- data.frame(x = 1)
  attr(df, "created_at") <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  expect_error(
    save_scan(
      df = df,
      storage_id = "test-storage",
      path = 1,
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )

  expect_error(
    save_scan(
      df = df,
      storage_id = "test-storage",
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )

  expect_error(
    save_scan(
      df = df,
      storage_id = "test-storage",
      path = NULL,
      label = "tmp"
    ),
    "'path' must be supplied explicitly"
  )
})

test_that("save_scan writes file and returns path", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  df <- data.frame(x = 1)
  attr(df, "created_at") <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  path <- save_scan(df = df, storage_id = "test-storage", path = tmp)

  expect_true(file.exists(path))

  loaded <- readRDS(path)
  expect_equal(loaded$x, 1)
})


test_that("save_scan fails without created_at", {
  df <- data.frame(x = 1)

  expect_error(
    save_scan(df, "test-storage", path = tempdir()),
    "created_at"
  )
})


test_that("save_scan uses label in filename", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  df <- data.frame(x = 1)
  attr(df, "created_at") <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  out <- save_scan(
    df,
    storage_id = "test-storage",
    path = tmp,
    label = "My Test Scope"
  )

  fname <- basename(out)

  expect_true(grepl("my_test_scope", fname))
  expect_true(file.exists(out))
})


test_that("save_scan filename depends on label", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  df <- data.frame(x = 1)
  attr(df, "created_at") <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  f1 <- save_scan(df, "test-storage", tmp, label = "a")
  f2 <- save_scan(df, "test-storage", tmp, label = "b")

  expect_false(basename(f1) == basename(f2))
})


test_that("save_scan treats NULL and empty label equally", {
  tmp1 <- tempfile("scan_null_")
  tmp2 <- tempfile("scan_empty_")

  dir.create(tmp1)
  dir.create(tmp2)

  on.exit(unlink(c(tmp1, tmp2), recursive = TRUE), add = TRUE)

  df <- data.frame(
    rel_path = "example.txt",
    size = 100
  )

  attr(df, "created_at") <- as.POSIXct(
    "2026-04-30 16:00:59",
    tz = "UTC"
  )

  p1 <- save_scan(df, "test-storage", tmp1, label = NULL)
  p2 <- save_scan(df, "test-storage", tmp2, label = "")

  # Same filename, despite different output directories.
  expect_identical(basename(p1), basename(p2))

  # Both snapshots were saved successfully.
  expect_true(file.exists(p1))
  expect_true(file.exists(p2))

  # Both preserve their contents.
  expect_identical(readRDS(p1), df)
  expect_identical(readRDS(p2), df)
})


test_that("save_scan sanitises label in filename", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  df <- data.frame(x = 1)
  attr(df, "created_at") <- as.POSIXct("2026-04-30 16:00:59", tz = "UTC")

  out <- save_scan(
    df,
    "test-storage",
    tmp,
    label = "D:/My Project!"
  )

  fname <- basename(out)

  expect_true(grepl("d_my_project", fname))
  expect_false(grepl("[/:! ]", fname))
})

test_that("save_scan rejects invalid label", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  df <- data.frame(x = 1)
  attr(df, "created_at") <- Sys.time()

  expect_error(
    save_scan(df, "test-storage", tmp, label = 123),
    "label"
  )
})
