# Tests-only diagnostic suite for path representation and storage identity.
# No production functions are modified.

local_fixture <- function() {
  root <- fs::file_temp(pattern = "fscontext_path_identity_")
  fs::dir_create(root)
  root
}

scan_fixture <- function(root, storage_id = "path-test", scan_time = as.POSIXct("2026-11-19 12:00:00", tz = "UTC")) {
  fscontext::scan_storage(root,
    storage_id = storage_id,
    scan_time = scan_time, compute_signature = FALSE
  )
}

test_that("equivalent root spelling yields identical storage path identity", {
  root <- local_fixture()
  withr::defer(fs::dir_delete(root))
  fs::file_create(fs::path(root, "item.txt"))
  a <- scan_fixture(root)
  b <- scan_fixture(fs::path(root, "."))
  expect_equal(a$rel_path, b$rel_path)
  expect_equal(a$storage_path_id, b$storage_path_id)
})

test_that("nested dot and dot-dot root segments preserve relative identities", {
  root <- local_fixture()
  withr::defer(fs::dir_delete(root))
  fs::dir_create(fs::path(root, "child"))
  fs::file_create(fs::path(root, "child", "item.txt"))
  canonical <- scan_fixture(root)
  alternate <- scan_fixture(fs::path(root, "child", ".."))
  expect_setequal(canonical$storage_path_id, alternate$storage_path_id)
})

test_that("nested path components have stable relative separators", {
  root <- local_fixture()
  withr::defer(fs::dir_delete(root))
  fs::dir_create(fs::path(root, "sub", "deep"), recurse = TRUE)
  fs::file_create(fs::path(root, "sub", "deep", "file.txt"))
  x <- scan_fixture(root)
  expect_length(x$rel_path, 1L)
  expect_equal(gsub("\\\\", "/", x$rel_path), "sub/deep/file.txt")
  expect_equal(x$storage_path_id, paste0("path-test::", x$rel_path))
})

test_that("repeated scans preserve case spelling of filenames", {
  root <- local_fixture()
  withr::defer(fs::dir_delete(root))
  fs::file_create(fs::path(root, "MixedCase.TXT"))
  a <- scan_fixture(root)
  b <- scan_fixture(root)
  expect_equal(a$rel_path, b$rel_path)
  expect_equal(a$storage_path_id, b$storage_path_id)
  expect_equal(a$filename, "MixedCase.TXT")
})

test_that("case-only filename variants follow filesystem capabilities", {
  root <- local_fixture()
  withr::defer(fs::dir_delete(root))
  fs::file_create(fs::path(root, "alpha.txt"))
  fs::file_create(fs::path(root, "ALPHA.txt"))
  x <- scan_fixture(root)
  if (nrow(x) == 2L) {
    expect_length(unique(x$storage_path_id), 2L)
  } else {
    expect_equal(nrow(x), 1L)
  }
})

test_that("context enrichment preserves distinct literal relative path spellings", {
  # Characterization test: does not assert that a canonicalization policy exists.
  x <- data.frame(
    storage_id = c("s", "s", "s"),
    rel_path = c("sub/file.txt", "sub\\\\file.txt", "sub/./file.txt"),
    full_path = c("X:/sub/file.txt", "X:\\\\sub\\\\file.txt", "X:/sub/./file.txt"),
    scan_time = rep(as.POSIXct("2026-11-19 12:00:00", tz = "UTC"), 3),
    stringsAsFactors = FALSE
  )
  y <- fscontext::add_snapshot_context(x)
  expect_equal(y$storage_path_id, paste0("s::", x$rel_path))
  expect_equal(length(unique(y$storage_path_id)), 3L)
})

test_that("drive-letter casing does not affect relative identity for same scan root", {
  skip_if(.Platform$file.sep != "\\", "Windows drive-letter test")
  root <- local_fixture()
  withr::defer(fs::dir_delete(root))
  fs::file_create(fs::path(root, "item.txt"))
  root_text <- as.character(fs::path_abs(root))
  skip_if(!grepl("^[A-Za-z]:", root_text), "Not on a drive-letter path")
  alt <- paste0(
    if (substr(root_text, 1, 1) == toupper(substr(root_text, 1, 1))) {
      tolower(substr(root_text, 1, 1))
    } else {
      toupper(substr(root_text, 1, 1))
    },
    substring(root_text, 2)
  )
  a <- scan_fixture(root_text)
  b <- scan_fixture(alt)
  expect_setequal(a$storage_path_id, b$storage_path_id)
})

test_that("UNC path identity is not inferred from a fabricated network location", {
  # A live UNC share is required to test scan_storage on network paths.
  # Here we verify only the documented literal storage-scoped construction.
  x <- data.frame(
    storage_id = "network",
    rel_path = "folder/file.txt",
    full_path = "\\\\server\\share\\folder\\file.txt",
    scan_time = as.POSIXct("2026-11-19 12:00:00", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  y <- fscontext::add_snapshot_context(x)
  expect_equal(y$storage_path_id, "network::folder/file.txt")
  expect_equal(y$storage_full_path, paste0("network::", x$full_path))
})
