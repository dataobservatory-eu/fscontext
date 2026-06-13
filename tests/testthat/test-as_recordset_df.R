test_that(
  "as_recordset_df creates semantically enriched recordset_df",
  {
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

    rs <- toy_resources |>
      create_record_set(
        record_set_id = "structural_group",
        resource_id = "path_id",
        locator_path = "rel_root_path",
        construction_rule =
          "filtered_project_roots|structural_group",
        resource_type = "file"
      ) |>
      as_recordset_df(
        title =
          "Toy reconstruction workspace",
        creator =
          utils::person(
            given = "Daniel",
            family = "Antal"
          ),
        description =
          "Contextual reconstruction record set"
      )

    expect_s3_class(
      rs,
      "recordset_df"
    )

    expect_s3_class(
      rs,
      "dataset_df"
    )

    expect_s3_class(
      rs,
      "tbl_df"
    )

    expect_true(
      all(
        c(
          "record_set_id",
          "member_id"
        ) %in% names(rs)
      )
    )

    expect_true(
      all(
        c(
          "member_path",
          "member_type"
        ) %in% names(rs)
      )
    )

    expect_equal(
      rs$member_id,
      toy_resources$path_id
    )

    expect_equal(
      rs$member_path,
      toy_resources$rel_root_path
    )

    expect_equal(
      unique(rs$member_type),
      "file"
    )

    expect_equal(
      dataset::dataset_title(rs),
      "Toy reconstruction workspace"
    )
  }
)


test_that(
  "as_recordset_df allows custom semantic mappings",
  {
    toy_resources <- tibble::tibble(
      record_identifier = c(
        "id_001",
        "id_002"
      ),
      record_locator = c(
        "a/file.txt",
        "b/file.txt"
      ),
      record_kind = c(
        "file",
        "file"
      ),
      grouping = c(
        "set_a",
        "set_a"
      )
    )

    rs <- toy_resources |>
      create_record_set(
        record_set_id = "grouping",
        resource_id = "record_identifier",
        locator_path = "record_locator",
        resource_type = "record_kind",
        construction_rule = "manual"
      ) |>
      as_recordset_df(
        title = "Custom mapped record set",
        creator =
          utils::person(
            given = "Daniel",
            family = "Antal"
          ),
        member_id = "resource_id",
        member_path = "locator_path",
        member_type = "resource_type"
      )

    expect_equal(
      rs$member_id,
      toy_resources$record_identifier
    )

    expect_equal(
      rs$member_path,
      toy_resources$record_locator
    )

    expect_equal(
      rs$member_type,
      toy_resources$record_kind
    )
  }
)


test_that(
  "as_recordset_df errors when semantic source column is missing",
  {
    toy_resources <- tibble::tibble(x = 1:3)

    expect_error(
      as_recordset_df(
        toy_resources,
        title = "Broken mapping",
        creator =
          utils::person(
            given = "Daniel",
            family = "Antal"
          ),
        member_id = "does_not_exist"
      ),
      regexp = "Column not found"
    )
  }
)


test_that(
  "as_recordset_df incorporates construction rule into description",
  {
    toy_resources <- tibble::tibble(
      structural_group = c(
        "_packages/eviota",
        "_packages/eviota"
      ),
      path_id = c(
        "l480::R/import.R",
        "l480::data-raw/build.R"
      ),
      rel_root_path = c(
        "R/import.R",
        "data-raw/build.R"
      )
    )

    rs <- toy_resources |>
      create_record_set(
        record_set_id = "structural_group",
        resource_id = "path_id",
        locator_path = "rel_root_path",
        construction_rule =
          "filtered_project_roots|structural_group",
        resource_type = "file"
      ) |>
      as_recordset_df(
        title =
          "Toy reconstruction workspace",
        creator =
          utils::person(
            given = "Daniel",
            family = "Antal"
          ),
        description =
          "Contextual reconstruction record set"
      )

    description_text <-
      attr(
        rs,
        "dataset_bibentry"
      )$description

    expect_true(
      grepl(
        "Contextual reconstruction record set",
        description_text
      )
    )

    expect_true(
      grepl(
        "Construction rule:",
        description_text
      )
    )

    expect_true(
      grepl(
        "filtered_project_roots\\|structural_group",
        description_text
      )
    )
  }
)
