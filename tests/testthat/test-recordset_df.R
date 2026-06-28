test_that("recordset_df creates a dataset_df intherited record set df", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/2"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  rs <- recordset_df(x,
    title = "Test"
  )

  expect_identical(
    dataset::subject(rs)$term,
    "Record Set"
  )

  expect_s3_class(rs, "recordset_df")
  expect_s3_class(rs, "dataset_df")
  expect_true(is.data.frame(rs))
  expect_equal(attr(rs, "dataset_bibentry")$title, "Test")

  expect_identical(dataset::dataset_title(rs), "Test")
})

test_that("record_set_identifier is set properly", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/2"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  rs <- recordset_df(
    x,
    record_set_identifier = "rs001"
  )


  expect_identical(dataset::identifier(rs), "rs001")
})

test_that("record_identifier is declared as a RiC identifier", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/2"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  rs <- recordset_df(
    x,
    record_identifier = "resource_locator"
  )

  expect_s3_class(rs$resource_locator, "haven_labelled_defined")
  expect_identical(attr(rs$resource_locator, "concept"), "rico:Identifier")
})

test_that("missing record identifier column throws an error", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/2"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  expect_error(
    recordset_df(x, record_identifier = "missing"),
    "Column not found"
  )
})

test_that("duplicate record identifiers produce a warning", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/1"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    recordset_df(x, record_identifier = "resource_locator"),
    "not unique"
  )
})

test_that("record_part_identifier is declared as a RiC identifier", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/2"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  rs <- recordset_df(
    x,
    record_identifier = "resource_locator",
    record_part_identifier = "filename"
  )

  expect_s3_class(rs$filename, "haven_labelled_defined")

  expect_identical(attr(rs$filename, "concept"), "rico:Identifier")

  expect_equal(as.character(unclass(rs$filename)), c("a.html", "b.html"))

  expect_error(
    recordset_df(
      x,
      record_part_identifier = "missing"
    ),
    "Column not found"
  )

  x <- data.frame(
    id = c("r1", "r2"),
    filename = c("a.html", "a.html")
  )

  expect_warning(
    recordset_df(x, record_part_identifier = "filename"),
    "not unique"
  )
})

test_that("dataset-level metadata is preserved", {
  x <- data.frame(
    resource_locator = c("https://example.org/1", "https://example.org/2"),
    filename = c("a.html", "b.html"),
    stringsAsFactors = FALSE
  )

  rs <- recordset_df(
    x,
    record_identifier = "resource_locator",
    record_part_identifier = "filename",
    title = "Demo Record Set",
    creator = person("Joe", "Doe", role = "aut")
  )

  # Test on core elements of the dataset_bibentry without class-specific
  # implementation details.

  expect_identical(
    attr(rs, "dataset_bibentry")$title,
    "Demo Record Set"
  )

  expect_identical(
    attr(rs, "dataset_bibentry")$author,
    person("Joe", "Doe", role = "aut")
  )

  rpt <- "<https://fscontext.dataobservatory.eu/software/fscontext> "

  expect_true(
    any(grepl(pattern = rpt, x = attr(rs, "prov")))
  )
})
