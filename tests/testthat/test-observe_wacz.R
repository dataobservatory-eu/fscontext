test_that("observe_wacz() reads a WACZ archive", {
  
  wacz <- system.file(
    "testdata",
    "fscontext_020.wacz",
    package = "fscontext"
  )
  
  obs <- observe_wacz(wacz)
  
  expect_true(
    all(!is.na(obs$quick_sig_text))
  )
  
  expect_true(
    any(!is.na(obs$digest))
  )
  
  expect_s3_class(obs, "data.frame")
  
  expect_gt(nrow(obs), 0)
  
  expect_true("resource_locator" %in% names(obs))
  
  expect_true("page_id" %in% names(obs))
  
  expect_true("archive" %in% names(obs))
  
  expect_equal(basename(attr(obs, "wacz")), "fscontext_020.wacz")
  
  dp <- attr(obs, "datapackage")
  
  expect_type(dp, "list")
  
  expect_true(
    "title" %in% names(dp)
  )
})

  
test_that("read_pages_jsonl() reads page metadata", {

  wacz <- system.file(
    "testdata",
    "fscontext_020.wacz",
    package = "fscontext"
  )

  tmp <- tempfile("wacz")

  extract_storage(
    archive = wacz,
    exdir = tmp
  )

  pages <- read_pages_jsonl(tmp)

  expect_s3_class(pages, "tbl_df")

  expect_gt(nrow(pages), 0)

  expect_true(
    all(
      c(
        "page_id",
        "title",
        "resource_locator",
        "timestamp",
        "favicon",
        "text",
        "text_length",
        "quick_sig_text"
      ) %in% names(pages)
    )
  )

  expect_true(all(!is.na(pages$resource_locator)))

  expect_true(all(pages$text_length >= 0))

  expect_true(all(!is.na(pages$quick_sig_text)))

})

test_that("collapse_cdx_versions() collapses repeated resources", {
  
  wacz <- system.file(
    "testdata",
    "fscontext_020.wacz",
    package = "fscontext"
  )
  
  tmp <- tempfile("wacz")
  
  extract_storage(
    archive = wacz,
    exdir = tmp
  )
  
  cdx <- read_cdx(tmp)
  
  collapsed <- collapse_cdx_versions(cdx)
  
  expect_s3_class(collapsed, "tbl_df")
  
  expect_true(all(collapsed$mime == "text/html"))
  
  expect_true(all(collapsed$n_versions >= 1))
  
  expect_equal(anyDuplicated(collapsed$resource_locator), 0L)
})

test_that("match_pages_to_cdx() joins page observations and archive metadata", {
  
  wacz <- system.file(
    "testdata",
    "fscontext_020.wacz",
    package = "fscontext"
  )
  
  tmp <- tempfile("wacz")
  
  extract_storage(
    archive = wacz,
    exdir = tmp
  )
  
  pages <- read_pages_jsonl(tmp)
  
  cdx <- read_cdx(tmp) |>
    collapse_cdx_versions()
  
  matched <- match_pages_to_cdx(
    pages,
    cdx
  )
  
  expect_s3_class(matched, "tbl_df")
  
  expect_equal(nrow(matched), nrow(pages))
  
  expect_true(
    any(!is.na(matched$digest))
  )
  
  expect_true(
    any(!is.na(matched$offset))
  )
})


test_that("read_datapackage() reads WACZ metadata", {
  
  wacz <- system.file(
    "testdata",
    "fscontext_020.wacz",
    package = "fscontext"
  )
  
  tmp <- tempfile("wacz")
  
  extract_storage(
    archive = wacz,
    exdir = tmp
  )
  
  dp <- read_datapackage(tmp)
  
  expect_true(
    all(
      c("profile", "title", "created", "resources") %in% names(dp)
      )
  )
  
  expect_type(dp, "list")
  
})
