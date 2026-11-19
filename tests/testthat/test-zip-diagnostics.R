# ZIP-backed storage diagnostics for fscontext.
# Tests only: no production behavior is modified.

zip_fixture <- function(source_dir, archive, entries = NULL) {
  if (!nzchar(Sys.which("zip"))) {
    testthat::skip("The external zip utility is not available")
  }
  if (is.null(entries)) {
    entries <- as.character(fs::dir_ls(source_dir,
      recurse = TRUE,
      type = "file", all = TRUE
    ))
    entries <- as.character(fs::path_rel(entries, start = source_dir))
  }
  old <- setwd(source_dir)
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(utils::zip(
    zipfile = as.character(fs::path_abs(archive)),
    files = entries, flags = "-r9X"
  ))
  testthat::expect_true(file.exists(archive))
  archive
}

test_that("ZIP scan agrees with directory scan on observational fields", {
  root <- fs::file_temp("fscontext_zip_equivalence_")
  withr::defer(fs::dir_delete(root))
  fs::dir_create(fs::path(root, "src", "nested"), recurse = TRUE)
  writeLines("one", fs::path(root, "src", "a.txt"))
  writeLines("two", fs::path(root, "src", "nested", "b.csv"))
  archive <- fs::path(root, "equivalence.zip")
  zip_fixture(fs::path(root, "src"), archive)

  stamp <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  directory <- scan_storage(fs::path(root, "src"),
    storage_id = "fixture",
    scan_time = stamp, compute_signature = TRUE
  )
  zipped <- scan_storage(archive,
    storage_id = "fixture",
    scan_time = stamp, compute_signature = TRUE
  )
  fields <- c(
    "rel_path", "filename", "extension", "size",
    "depth", "storage_path_id", "quick_sig"
  )
  directory <- directory[order(directory$rel_path), fields, drop = FALSE]
  zipped <- zipped[order(zipped$rel_path), fields, drop = FALSE]
  rownames(directory) <- NULL
  rownames(zipped) <- NULL
  expect_equal(zipped, directory)
  expect_true(all(zipped$storage_path_id == paste0("fixture::", zipped$rel_path)))
  expect_true(all(attr(
    scan_storage(archive, compute_signature = FALSE),
    "archive_root"
  ) == as.character(fs::path_abs(archive))))
})

test_that("ZIP with one top-level directory removes only that wrapper", {
  root <- fs::file_temp("fscontext_zip_wrapper_")
  withr::defer(fs::dir_delete(root))
  fs::dir_create(fs::path(root, "src", "package", "nested"), recurse = TRUE)
  writeLines("description", fs::path(root, "src", "package", "DESCRIPTION"))
  writeLines("nested", fs::path(root, "src", "package", "nested", "x.txt"))
  archive <- fs::path(root, "wrapper.zip")
  zip_fixture(fs::path(root, "src"), archive)
  result <- scan_storage(archive, compute_signature = FALSE)
  expect_setequal(result$rel_path, c("DESCRIPTION", "nested/x.txt"))
  expect_equal(unique(result$container_file), "wrapper.zip")
  expect_equal(unique(result$container_type), "zip")
})

test_that("ZIP with multiple top-level entries retains its hierarchy", {
  root <- fs::file_temp("fscontext_zip_multiroot_")
  withr::defer(fs::dir_delete(root))
  fs::dir_create(fs::path(root, "src", "first"), recurse = TRUE)
  fs::dir_create(fs::path(root, "src", "second"), recurse = TRUE)
  writeLines("a", fs::path(root, "src", "first", "a.txt"))
  writeLines("b", fs::path(root, "src", "second", "b.txt"))
  archive <- fs::path(root, "multi.zip")
  zip_fixture(fs::path(root, "src"), archive)
  result <- scan_storage(archive, compute_signature = FALSE)
  expect_setequal(result$rel_path, c("first/a.txt", "second/b.txt"))
})

test_that("ZIP preserves Unicode paths when the ZIP utility supports them", {
  root <- fs::file_temp("fscontext_zip_unicode_")
  withr::defer(fs::dir_delete(root))
  folder <- "Árvíztűrő"
  name <- "próba_őű.txt"
  fs::dir_create(fs::path(root, "src", folder), recurse = TRUE)
  writeLines("unicode", fs::path(root, "src", folder, name), useBytes = TRUE)
  archive <- fs::path(root, "unicode.zip")
  zip_fixture(fs::path(root, "src"), archive)
  manifest <- utils::unzip(archive, list = TRUE)$Name
  if (!any(grepl(name, manifest, fixed = TRUE))) {
    skip("ZIP utility did not encode the Unicode entry name faithfully")
  }
  result <- scan_storage(archive, compute_signature = FALSE)
  expect_equal(result$rel_path, name)
  expect_true(grepl(folder, attr(result, "archive_scan_root"), fixed = TRUE))
  expect_equal(result$filename, name)
})

test_that("same-basename archives do not invalidate earlier extracted observations", {
  # Regression candidate: scan_zip_storage() uses tempdir()/archive basename,
  # deleting an existing extraction directory before the next scan.
  root <- fs::file_temp("fscontext_zip_collision_")
  withr::defer(fs::dir_delete(root))
  fs::dir_create(fs::path(root, "first", "src"), recurse = TRUE)
  fs::dir_create(fs::path(root, "second", "src"), recurse = TRUE)
  writeLines("first archive", fs::path(root, "first", "src", "item.txt"))
  writeLines("second archive", fs::path(root, "second", "src", "item.txt"))
  # Deliberately use a basename DIFFERENT from the fixture root.
  # Otherwise the scanner's tempdir()/archive_id cleanup deletes the
  # test fixture itself, preventing a valid collision diagnosis.
  shared_name <- "shared_archive_collision_probe.zip"
  extraction <- fs::path(tempdir(), "shared_archive_collision_probe")
  # Do not delete an extraction path belonging to an unrelated process.
  if (fs::dir_exists(extraction)) {
    skip("Collision extraction directory already exists; avoiding interference")
  }
  withr::defer({
    if (fs::dir_exists(extraction)) fs::dir_delete(extraction)
  })
  a <- fs::path(root, "first", shared_name)
  b <- fs::path(root, "second", shared_name)
  zip_fixture(fs::path(root, "first", "src"), a)
  zip_fixture(fs::path(root, "second", "src"), b)
  first <- scan_storage(a, compute_signature = TRUE)
  expect_equal(readLines(first$full_path[1]), "first archive")
  second <- scan_storage(b, compute_signature = TRUE)
  expect_equal(readLines(second$full_path[1]), "second archive")
  # Expected to fail with current implementation: extraction path is reused.
  expect_true(file.exists(first$full_path[1]))
  expect_equal(readLines(first$full_path[1]), "first archive")
  expect_false(identical(
    attr(first, "extracted_root"),
    attr(second, "extracted_root")
  ))
})

test_that("repeated ZIP scans preserve earlier extracted files", {
  root <- fs::file_temp("fscontext_zip_repeat_")
  withr::defer(fs::dir_delete(root))

  src <- fs::path(root, "src")
  fs::dir_create(src, recurse = TRUE)
  writeLines("original", fs::path(src, "item.txt"))

  archive <- fs::path(root, "repeat.zip")
  zip_fixture(src, archive)

  first <- scan_storage(archive, compute_signature = FALSE)
  second <- scan_storage(archive, compute_signature = FALSE)

  expect_false(identical(
    attr(first, "extracted_root"),
    attr(second, "extracted_root")
  ))

  expect_true(file.exists(first$full_path[1]))
  expect_true(file.exists(second$full_path[1]))

  expect_equal(readLines(first$full_path[1]), "original")
  expect_equal(readLines(second$full_path[1]), "original")

  expect_equal(first$rel_path, second$rel_path)
  expect_equal(first$storage_path_id, second$storage_path_id)
})
