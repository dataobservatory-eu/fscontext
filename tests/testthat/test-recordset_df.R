test_that("recordset_df creates a valid recordset_df object", {
  toy_recordset <- recordset_df(
    record_set_id = c(
      "eviota",
      "eviota",
      "eviota"
    ),
    member_id = c(
      "inst_001",
      "inst_002",
      "inst_003"
    ),
    member_path = c(
      "filmledgerimport/R/import.R",
      "eviota/data-raw/build.R",
      "eviota/reports/report.qmd"
    ),
    member_type = c(
      "file",
      "file",
      "file"
    ),
    source_type = c(
      "filesystem",
      "filesystem",
      "filesystem"
    ),
    identifier = c(
      member =
        "https://example.org/recordset/eviota#member"
    ),
    var_labels = list(
      record_set_id = "Record set identifier",
      member_id = "Member identifier",
      member_path = "Member path"
    ),
    concepts = list(
      record_set_id =
        "https://www.ica.org/standards/RiC/ontology#RecordSet",
      member_id =
        "https://www.ica.org/standards/RiC/ontology#Instantiation"
    ),
    dataset_bibentry = dataset::dublincore(
      title = "Toy Eviota Record Set",
      creator = person("Daniel", "Antal"),
      publisher = "fscontext"
    )
  )

  expect_s3_class(
    toy_recordset,
    "recordset_df"
  )

  expect_s3_class(
    toy_recordset,
    "dataset_df"
  )

  expect_true(
    is.data.frame(toy_recordset)
  )

  expect_true(
    all(
      c("record_set_id", "member_id") %in%
        names(toy_recordset)
    )
  )
})

test_that("recordset_df requires record_set_id", {
  expect_error(
    recordset_df(
      member_id = c("inst_001"),
      member_path = c("file.R")
    ),
    "Missing required columns: record_set_id"
  )
})

test_that("recordset_df requires member_id", {
  expect_error(
    recordset_df(
      record_set_id = c("eviota"),
      member_path = c("file.R")
    ),
    "Missing required columns: member_id"
  )
})
