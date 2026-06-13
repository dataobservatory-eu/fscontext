test_that("create_record_set creates contextual record set projection", {
  toy_resources <- tibble::tibble(
    structural_group = c(
      "_packages/eviota",
      "_packages/eviota",
      "_packages/iotables"
    ),
    path_id = c(
      "l480::R/import.R",
      "l480::data-raw/build.R",
      "l480::R/cube.R"
    ),
    rel_root_path = c(
      "R/import.R",
      "data-raw/build.R",
      "R/cube.R"
    )
  )

  rs <- create_record_set(
    toy_resources,
    record_set_id = "structural_group",
    resource_id = "path_id",
    locator_path = "rel_root_path",
    construction_rule =
      "filtered_project_roots|structural_group",
    resource_type = "file"
  )

  expect_s3_class(rs, "tbl_df")

  expect_true(
    all(
      c(
        "record_set_id",
        "resource_id",
        "locator_path",
        "resource_type"
      ) %in% names(rs)
    )
  )

  expect_equal(
    rs$record_set_id,
    toy_resources$structural_group
  )

  expect_equal(
    rs$resource_id,
    toy_resources$path_id
  )

  expect_equal(
    rs$locator_path,
    toy_resources$rel_root_path
  )

  expect_equal(
    unique(rs$resource_type),
    "file"
  )

  expect_equal(
    attr(rs, "construction_rule"),
    "filtered_project_roots|structural_group"
  )

  expect_equal(
    attr(rs, "created_by"),
    "create_record_set"
  )

  expect_true(
    inherits(
      attr(rs, "record_set_created_at"),
      "POSIXct"
    )
  )
})


test_that("create_record_set errors without required identifiers", {
  toy_resources <- tibble::tibble(
    rel_path = c("a.txt", "b.txt")
  )

  expect_error(
    create_record_set(
      toy_resources,
      record_set_id = NULL,
      resource_id = NULL,
      construction_rule = "test"
    ),
    "Missing required values"
  )
})


test_that("create_record_set accepts scalar values", {
  toy_resources <- tibble::tibble(
    path_id = c(
      "id1",
      "id2"
    )
  )

  rs <- create_record_set(
    toy_resources,
    record_set_id = "toy_record_set",
    resource_id = "path_id",
    construction_rule = "manual",
    resource_type = "file"
  )

  expect_equal(
    unique(rs$record_set_id),
    "toy_record_set"
  )

  expect_equal(
    unique(rs$resource_type),
    "file"
  )
})
