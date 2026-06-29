test_that("record_set_projection creates contextual record set projection", {
  toy_resources <- tibble::tibble(
    structural_group = c(
      "_packages/pkg-a",
      "_packages/pkg-a",
      "_packages/pkg-b"
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

  rs <- record_set_projection(
    toy_resources,
    record_set_identifier = "structural_group",
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
        "record_set_identifier",
        "resource_id",
        "locator_path",
        "resource_type"
      ) %in% names(rs)
    )
  )

  expect_equal(rs$record_set_identifier, toy_resources$structural_group)

  expect_equal(rs$resource_id, toy_resources$path_id)

  expect_equal(rs$locator_path, toy_resources$rel_root_path)

  expect_equal(unique(rs$resource_type), "file")

  expect_equal(
    attr(rs, "construction_rule"),
    "filtered_project_roots|structural_group"
  )
})


test_that("record_set_projection errors without required identifiers", {
  toy_resources <- tibble::tibble(
    rel_path = c("a.txt", "b.txt")
  )

  expect_error(
    record_set_projection(
      toy_resources,
      record_set_identifier = NULL,
      resource_id = NULL,
      construction_rule = "test"
    ),
    "Missing required values"
  )
})


test_that("record_set_projection accepts scalar values", {
  toy_resources <- tibble::tibble(
    path_id = c("id1", "id2")
  )

  rs <- record_set_projection(
    toy_resources,
    record_set_identifier = "toy_record_set",
    resource_id = "path_id",
    construction_rule = "manual",
    resource_type = "file"
  )

  expect_equal(unique(rs$record_set_identifier), "toy_record_set")

  expect_equal(unique(rs$resource_type), "file")
})
