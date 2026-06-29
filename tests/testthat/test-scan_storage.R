# Structure ------------------------------------------------------------

test_that("scan_storage returns expected structure", {
  root <- system.file("testdata/minimal_R_folder",
    package = "fscontext"
  )
  res <- scan_storage(root)

  expect_s3_class(res, "data.frame")

  expect_setequal(names(res), c(
    "storage_id", "person_id", "full_path", "rel_path",
    "filename", "stem", "extension", "type", "size",
    "mtime", "ctime", "atime", "birth_time",
    "depth", "links", "permissions",
    "quick_sig", "scan_time", "storage_path_id",
    "repo_root", "repo_rel_path", "git_tracked"
  ))

  expect_gt(nrow(res), 10)
})

# Content --------------------------------------------------------------

test_that("scan_storage detects key files", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")
  res <- scan_storage(root)

  expect_true(any(res$rel_path == "DESCRIPTION"))
  expect_true(any(res$rel_path == "NAMESPACE"))
  expect_true(any(res$rel_path == "R/hello_world.R"))
  expect_true(any(res$rel_path == "vignettes/demo.Rmd"))
})


test_that("scan_storage is recursive", {
  root <- system.file("testdata/minimal_R_folder",
    package = "fscontext"
  )
  stopifnot(nzchar(root))

  res <- scan_storage(root)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)

  # must include files from nested directories
  expect_true(any(grepl("^R/", res$rel_path)))
  expect_true(any(grepl("^vignettes/", res$rel_path)))
})

# Extensions -----------------------------------------------------------

test_that("extensions are lowercase when present", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")
  res <- scan_storage(root)

  non_na <- !is.na(res$extension)

  expect_true(all(res$extension[non_na] == tolower(res$extension[non_na])))
})


# storage_path_id --------------------------------------------------------------

test_that("storage_path_id is correctly constructed and unique", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")
  res <- scan_storage(root)

  expected <- paste(res$storage_id, res$rel_path, sep = "::")

  expect_equal(res$storage_path_id, expected)
  expect_equal(length(unique(res$storage_path_id)), nrow(res))
})

# Repository -----------------------------------------------------------

test_that("repository metadata is detected and structured correctly", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")
  res <- scan_storage(root)

  repos <- attr(res, "repos")

  expect_s3_class(repos, "data.frame")

  expect_setequal(names(repos), c(
    "repo_root", "git_remote", "git_branch"
  ))

  if (nrow(repos) > 0) {
    expect_true(all(fs::dir_exists(repos$repo_root)))
  }

  expect_true("repo_root" %in% names(res))
})

# Attributes -----------------------------------------------------------

test_that("scan metadata attributes are correct", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")
  res <- scan_storage(root)

  expect_equal(attr(res, "created_by"), "scan_storage")
  expect_equal(attr(res, "package"), "fscontext")

  expect_true(is.character(attr(res, "package_version")) ||
    is.na(attr(res, "package_version")))

  expect_true(inherits(attr(res, "created_at"), "POSIXct"))
})


# scan_time consistency -----------------------------------------------

test_that("scan_time column matches created_at attribute", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")
  res <- scan_storage(root)

  expect_true(all(res$scan_time == attr(res, "created_at")))
})


# Tempdir / mtime ------------------------------------------------------

test_that("scan_storage captures recent file mtimes (minute precision)", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  t0 <- as.POSIXct(Sys.time(), tz = "UTC")

  f1 <- fs::path(tmp, "a.R")
  f2 <- fs::path(tmp, "b.txt")

  writeLines("x <- 1", f1)
  writeLines("hello", f2)

  res <- scan_storage(
    root = tmp,
    storage_id = "test-storage",
    person_id = "tester"
  )

  round_min <- function(x) {
    as.POSIXct(format(x, "%Y-%m-%d %H:%M"), tz = "UTC")
  }

  t0_min <- round_min(t0)
  mtime_min <- round_min(res$mtime)

  expect_equal(nrow(res), 2)
  expect_true(all(abs(as.numeric(difftime(res$mtime, t0, units = "secs"))) < 60))
  expect_setequal(res$rel_path, c("a.R", "b.txt"))
})

## mtime ----------------------------------------------------

test_that("scan_storage captures recent file mtimes", {
  tmp <- fs::dir_create(fs::file_temp("scan_test_"))

  t0 <- Sys.time()

  f1 <- fs::path(tmp, "a.R")
  f2 <- fs::path(tmp, "b.txt")

  writeLines("x <- 1", f1)
  writeLines("hello", f2)

  res <- scan_storage(tmp)

  expect_equal(nrow(res), 2)
  expect_setequal(res$rel_path, c("a.R", "b.txt"))

  expect_true(all(abs(as.numeric(difftime(res$mtime, t0, units = "secs"))) < 60))
})


## Depth correctness ---------------------------------------


test_that("depth is computed correctly", {
  tmp <- fs::dir_create(fs::file_temp("scan_depth_"))

  fs::dir_create(fs::path(tmp, "a/b"))
  writeLines("x", fs::path(tmp, "a/b/file.txt"))

  res <- scan_storage(tmp)

  d <- res$depth[res$rel_path == "a/b/file.txt"]

  expect_equal(d, 3)
})


## Signature ----------------------------------------------


test_that("quick_sig is computed conditionally", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")

  res <- scan_storage(root, compute_signature = TRUE)

  expect_true(any(!is.na(res$quick_sig)))
})


test_that("quick_sig can be disabled", {
  root <- system.file("testdata/minimal_R_folder", package = "fscontext")

  res <- scan_storage(root, compute_signature = FALSE)

  expect_true(all(is.na(res$quick_sig)))
})

test_that("quick_sig uses local filesystem paths", {
  tmp <- fs::dir_create(fs::file_temp())

  f <- fs::path(tmp, "a.R")
  writeLines("x <- 1", f)

  res <- scan_storage(tmp)

  expect_false(is.na(res$quick_sig[1]))
})

## Repos  --------------------------------------------------

test_that("scan_storage detects repo roots correctly", {
  tmp <- fs::dir_create(fs::file_temp())

  fs::dir_create(fs::path(tmp, "repo/.git"))
  writeLines("x", fs::path(tmp, "repo/file.R"))

  res <- scan_storage(tmp)

  expect_true(any(!is.na(res$repo_root)))
})


test_that("scan_storage assigns nearest repo root", {
  tmp <- fs::dir_create(fs::file_temp())

  # create nested repos
  fs::dir_create(fs::path(tmp, "repo/.git"))
  fs::dir_create(fs::path(tmp, "repo/sub/.git"))

  f1 <- fs::path(tmp, "repo/file1.R")
  f2 <- fs::path(tmp, "repo/sub/file2.R")

  writeLines("x", f1)
  writeLines("y", f2)

  res <- scan_storage(tmp)

  r1 <- res$repo_root[res$rel_path == "repo/file1.R"]
  r2 <- res$repo_root[res$rel_path == "repo/sub/file2.R"]

  expect_true(grepl("repo$", r1))
  expect_true(grepl("repo/sub$", r2))
})

test_that("repo detection works with contextualized full_path", {
  tmp <- fs::dir_create(fs::file_temp())

  fs::dir_create(fs::path(tmp, "repo/.git"))
  writeLines("x", fs::path(tmp, "repo/test.R"))

  res <- scan_storage(tmp)

  expect_true(any(!is.na(res$repo_root)))
  expect_true(any(res$rel_path == "repo/test.R"))
})

## Scan provenance --------------------------------------

test_that("scan provenance attributes are present", {
  root <- system.file(
    "testdata/minimal_R_folder",
    package = "fscontext"
  )

  res <- scan_storage(root)

  expect_true(!is.null(attr(res, "scan_root")))
  expect_true(!is.null(attr(res, "scan_call")))

  expect_true(
    is.logical(attr(res, "signature_enabled"))
  )

  expect_true(
    is.numeric(attr(res, "max_signature_size"))
  )
})


test_that("full_path stores filesystem paths", {
  tmp <- fs::dir_create(fs::file_temp())

  f <- fs::path(tmp, "a.R")
  writeLines("x <- 1", f)

  res <- scan_storage(tmp)

  expect_true(all(fs::file_exists(res$full_path)))
})

## Working with ZIP files --------------------------------------------------
test_that(
  "zip storage reproduces directory observations",
  {
    folder_root <- system.file("testdata/minimal_R_folder",
      package = "fscontext"
    )

    zip_root <- system.file(
      "testdata/minimal_R_folder.zip",
      package = "fscontext"
    )

    folder <- scan_storage(folder_root)

    zip <- scan_storage(zip_root)

    folder <- folder[!grepl("(^|/)\\.", folder$rel_path), ]
    zip <- zip[!grepl("(^|/)\\.", zip$rel_path), ]

    folder <- folder[order(folder$rel_path), ]

    zip <- zip[order(zip$rel_path), ]

    expect_equal(folder$rel_path, zip$rel_path)

    expect_equal(folder$filename, zip$filename)

    expect_equal(folder$extension, zip$extension)

    expect_equal(folder$size, zip$size)

    expect_equal(folder$quick_sig, zip$quick_sig)
  }
)

test_that("zip files can be observed", {
  zip_root <- system.file(
    "testdata/minimal_R_folder.zip",
    package = "fscontext"
  )

  res <- scan_storage(zip_root)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 10)
})


test_that("zip observations can be contextualised", {
  zip_root <- system.file(
    "testdata/minimal_R_folder.zip",
    package = "fscontext"
  )

  res <- scan_storage(zip_root)

  ctx <- add_snapshot_context(res)

  expect_true(
    "observation_id" %in% names(ctx)
  )
})


test_that(
  "zip snapshots can be contextualised",
  {
    zip_root <- system.file(
      "testdata/minimal_R_folder.zip",
      package = "fscontext"
    )

    zip <- scan_storage(zip_root)

    zip <- add_snapshot_context(zip)

    expect_true(
      "observation_id" %in% names(zip)
    )

    expect_true(
      "storage_full_path" %in% names(zip)
    )
  }
)
