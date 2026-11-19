# Tests-only diagnostics for filesystem aliases and links.
# No production changes. Run: devtools::test(filter = '^filesystem-links-diagnostics$')

link_fixture <- function() {
  root <- fs::file_temp(pattern = "fscontext_links_")
  fs::dir_create(root)
  withr::defer(fs::dir_delete(root), envir = parent.frame())
  root
}

scan_links <- function(root) {
  fscontext::scan_storage(root,
    storage_id = "link-storage",
    compute_signature = TRUE,
    scan_time = as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  )
}

test_that("ordinary files retain distinct storage-scoped path identities", {
  root <- link_fixture()
  writeLines("same content", fs::path(root, "one.txt"))
  writeLines("same content", fs::path(root, "two.txt"))
  x <- scan_links(root)
  expect_setequal(x$rel_path, c("one.txt", "two.txt"))
  expect_equal(length(unique(x$storage_path_id)), 2L)
  expect_equal(length(unique(x$quick_sig)), 1L)
})

test_that("hard links preserve distinct paths and report equivalent content", {
  root <- link_fixture()
  original <- fs::path(root, "original.txt")
  alias <- fs::path(root, "hardlink.txt")
  writeLines("hard-linked bytes", original)
  made <- tryCatch(isTRUE(file.link(original, alias)), error = function(e) FALSE)
  if (!made || !file.exists(alias)) skip("Hard links not supported in this environment")
  x <- scan_links(root)
  expect_setequal(x$rel_path, c("original.txt", "hardlink.txt"))
  expect_equal(length(unique(x$storage_path_id)), 2L)
  expect_equal(length(unique(x$quick_sig)), 1L)
  expect_true(all(!is.na(x$quick_sig)))
})

test_that("file symbolic link scanning is characterized without assuming dereference policy", {
  root <- link_fixture()
  original <- fs::path(root, "original.txt")
  alias <- fs::path(root, "alias.txt")
  writeLines("linked bytes", original)
  made <- tryCatch(isTRUE(suppressWarnings(file.symlink(original, alias))), error = function(e) FALSE)
  if (!made || !file.exists(alias)) skip("File symlinks unavailable or require privileges")
  x <- scan_links(root)
  expect_true("original.txt" %in% x$rel_path)
  expect_true(all(!duplicated(x$storage_path_id)))
  if ("alias.txt" %in% x$rel_path) {
    expect_false(identical(
      x$storage_path_id[x$rel_path == "alias.txt"],
      x$storage_path_id[x$rel_path == "original.txt"]
    ))
  }
})

test_that("symlink to file outside root does not silently change observed path identity", {
  root <- link_fixture()
  outside <- fs::file_temp(pattern = "fscontext_external_", ext = ".txt")
  withr::defer(unlink(outside), envir = environment())
  writeLines("external bytes", outside)
  alias <- fs::path(root, "external-link.txt")
  made <- tryCatch(isTRUE(suppressWarnings(file.symlink(outside, alias))), error = function(e) FALSE)
  if (!made || !file.exists(alias)) skip("File symlinks unavailable or require privileges")
  x <- scan_links(root)
  expect_true(all(x$rel_path %in% "external-link.txt"))
  if (nrow(x)) {
    expect_true(all(x$storage_path_id == paste0("link-storage::", x$rel_path)))
  }
})

test_that("directory symlink does not cause unbounded traversal", {
  root <- link_fixture()
  actual <- fs::path(root, "actual")
  fs::dir_create(actual)
  writeLines("payload", fs::path(actual, "item.txt"))
  alias <- fs::path(root, "alias-dir")
  made <- tryCatch(isTRUE(suppressWarnings(file.symlink(actual, alias))), error = function(e) FALSE)
  if (!made || !dir.exists(alias)) skip("Directory symlinks unavailable or require privileges")
  x <- scan_links(root)
  expect_true(any(grepl("item\\.txt$", x$rel_path)))
  expect_lte(nrow(x), 2L)
  expect_true(all(!duplicated(x$storage_path_id)))
})

test_that("self-referential directory symlink does not cause unbounded traversal", {
  root <- link_fixture()
  writeLines("payload", fs::path(root, "item.txt"))
  loop <- fs::path(root, "loop")
  made <- tryCatch(isTRUE(suppressWarnings(file.symlink(root, loop))), error = function(e) FALSE)
  if (!made || !dir.exists(loop)) skip("Directory symlinks unavailable or require privileges")
  x <- scan_links(root)
  expect_true("item.txt" %in% x$rel_path)
  expect_lte(nrow(x), 2L)
})
