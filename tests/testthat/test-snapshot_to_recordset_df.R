test_that("snapshot_to_recordset_df returns a semantic recordset_df", {
  data("fscontextdemo_snapshot_02")

  snapshot_file <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, snapshot_file)

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1]

  rs <- snapshot_to_recordset_df(
    snapshot_files = snapshot_file,
    roots = roots,
    record_set_id = "test-record-set",
    person = utils::person("Jane", "Doe")
  )

  expect_s3_class(rs, "recordset_df")
  expect_s3_class(rs, "dataset_df")
  expect_s3_class(rs, "data.frame")

  expect_gt(nrow(rs), 0)

  expect_true("record_set_id" %in% names(rs))
  expect_equal(unique(rs$record_set_id), "test-record-set")
})

test_that("snapshot_to_recordset_df applies asserted metadata", {
  data("fscontextdemo_snapshot_02")

  snapshot_file <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, snapshot_file)

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1]

  rs <- snapshot_to_recordset_df(
    snapshot_files = snapshot_file,
    roots = roots,
    record_set_id = "test-record-set",
    record_set_title = "The test-record-set filesystem record set",
    person = utils::person("Jane", "Doe")
  )

  expect_equal(
    unique(rs$record_set_id),
    "test-record-set"
  )

  expect_equal(
    dataset::dataset_title(rs),
    "The test-record-set filesystem record set"
  )

  bib <- attr(rs, "dataset_bibentry")

  expect_true(any(grepl("Jane", capture.output(print(bib)))))
  expect_true(any(grepl("Doe", capture.output(print(bib)))))
})

test_that("snapshot_to_recordset_df preserves reconstructed observations", {
  data("fscontextdemo_snapshot_02")

  snapshot_file <- tempfile(fileext = ".rds")

  saveRDS(fscontextdemo_snapshot_02, snapshot_file)

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1]

  rs <- snapshot_to_recordset_df(
    snapshot_files = snapshot_file,
    person = utils::person("Jane", "Doe"),
    roots = roots,
    record_set_id = "test-record-set"
  )

  # ----------------------------------------------------------
  # Core observational variables preserved
  # ----------------------------------------------------------

  required_cols <- c(
    "storage_id",
    "person_id",
    "full_path",
    "rel_path",
    "filename",
    "mtime",
    "scan_time",
    "record_set_id"
  )

  expect_true(
    all(required_cols %in% names(rs))
  )

  # ----------------------------------------------------------
  # Original filesystem observations retained
  # ----------------------------------------------------------

  original_paths <- unique(fscontextdemo_snapshot_02$full_path)

  reconstructed_paths <- unique(rs$full_path)

  expect_true(
    any(reconstructed_paths %in% original_paths)
  )
})

test_that("snapshot_to_recordset_df preserves core observational columns", {
  data("fscontextdemo_snapshot_02")

  snapshot_file <- tempfile(fileext = ".rds")
  saveRDS(fscontextdemo_snapshot_02, snapshot_file)

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1]

  rs <- snapshot_to_recordset_df(
    snapshot_files = snapshot_file,
    roots = roots,
    record_set_id = "test-record-set",
    person = utils::person("Jane", "Doe")
  )

  required_cols <- c(
    "storage_id",
    "person_id",
    "full_path",
    "rel_path",
    "filename",
    "mtime",
    "scan_time",
    "record_set_id",
    "resource_id",
    "locator_path",
    "observation_id"
  )

  expect_true(all(required_cols %in% names(rs)))
})

test_that("snapshot_to_recordset_df attaches provenance metadata", {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")

  snapshot_files <- file.path(
    tempdir(),
    c(
      "fscontextdemo_snapshot_01.rds",
      "fscontextdemo_snapshot_01.rds",
      "fscontextdemo_snapshot_02.rds"
    )
  )

  saveRDS(fscontextdemo_snapshot_01, snapshot_files[1])
  saveRDS(fscontextdemo_snapshot_01, snapshot_files[2])
  saveRDS(fscontextdemo_snapshot_02, snapshot_files[3])

  roots <- unique(dirname(fscontextdemo_snapshot_02$full_path))[1]

  rs <- snapshot_to_recordset_df(
    snapshot_files = snapshot_files,
    person = utils::person("Jane", "Doe"),
    roots = roots,
    record_set_id = "test-record-set"
  )

  prov <- dataset::provenance(rs)

  # ----------------------------------------------------------
  # Provenance graph exists
  # ----------------------------------------------------------

  expect_true(is.character(prov))
  expect_gt(length(prov), 0)

  # ----------------------------------------------------------
  # PROV relations present
  # ----------------------------------------------------------

  expect_true(any(grepl(
    "prov#wasGeneratedBy",
    prov,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "prov#used",
    prov,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "prov#wasDerivedFrom",
    prov,
    fixed = TRUE
  )))

  # ----------------------------------------------------------
  # Human and software agents recorded
  # ----------------------------------------------------------

  expect_true(any(grepl(
    "foaf/0.1/Person",
    prov,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "prov#SoftwareAgent",
    prov,
    fixed = TRUE
  )))
})
