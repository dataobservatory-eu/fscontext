## Basic validation and dimensions ----------------------------

test_that(
  "refine_by_rulebook returns expected structure",
  {
    semantic_test_matrix <-
      data.frame(
        row_id = c(
          1, 2, 3, 4
        ),
        explored_path = c(
          "data-raw/figs",
          "data-raw/tables",
          "tests/testthat",
          "docs/deps"
        ),
        extension = c(
          "png",
          "csv",
          "r",
          "js"
        ),
        workflow_context_label = c(
          "etl_and_modelling",
          "etl_and_modelling",
          "package_functionality",
          "rendered_reporting"
        ),
        stringsAsFactors = FALSE
      )

    ## Semantic refinement rules ----------------------------------

    semantic_refinement_rules <-
      data.frame(
        refine_id = c(
          "refine_1",
          "refine_2",
          "refine_3",
          "refine_3",
          "refine_4"
        ),
        variable = c(
          "explored_path",
          "explored_path",
          "explored_path",
          "extension",
          "extension"
        ),
        match = c(
          "starts_with",
          "starts_with",
          "starts_with",
          "exact",
          "exact"
        ),
        pattern = c(
          "docs/deps",
          "tests/testthat",
          "data-raw",
          "png",
          "js"
        ),
        refined_assertion = c(
          "generated_website_support",
          "validation",
          "use_case_visualisation",
          "use_case_visualisation",
          "website_support"
        ),
        stringsAsFactors = FALSE
      )

    semantic_refinement_rules

    compiled_rulebook <-
      compile_rulebook(
        semantic_refinement_rules
      )


    semantic_test_result <-
      semantic_test_matrix |>
      refine_by_rulebook(
        target =
          workflow_context_label,
        rulebook =
          compiled_rulebook,
        prefix =
          "workflow_context"
      )

    result <-
      semantic_test_matrix |>
      refine_by_rulebook(
        target =
          workflow_context_label,
        rulebook =
          compiled_rulebook,
        prefix =
          "workflow_context"
      )

    ## Row count preserved --------------------------------

    expect_equal(
      nrow(result),
      nrow(semantic_test_matrix)
    )

    ## Expected refinement columns created ---------------

    expected_ref_cols <- c(
      "workflow_context_1_ref",
      "workflow_context_2_ref",
      "workflow_context_3_ref",
      "workflow_context_4_ref",
      "workflow_context_final"
    )

    expect_true(
      all(
        expected_ref_cols %in%
          names(result)
      )
    )

    ## Final column matches last refinement stage --------

    expect_equal(
      result$workflow_context_final,
      result$workflow_context_4_ref
    )
  }
)


## Semantic refinement results -------------------------------

test_that(
  "refine_by_rulebook produces expected semantic progression",
  {
    semantic_test_matrix <-
      data.frame(
        row_id = c(
          1, 2, 3, 4
        ),
        explored_path = c(
          "data-raw/figs",
          "data-raw/tables",
          "tests/testthat",
          "docs/deps"
        ),
        extension = c(
          "png",
          "csv",
          "r",
          "js"
        ),
        workflow_context_label = c(
          "etl_and_modelling",
          "etl_and_modelling",
          "package_functionality",
          "rendered_reporting"
        ),
        stringsAsFactors = FALSE
      )

    ## Semantic refinement rules ----------------------------------

    semantic_refinement_rules <-
      data.frame(
        refine_id = c(
          "refine_1",
          "refine_2",
          "refine_3",
          "refine_3",
          "refine_4"
        ),
        variable = c(
          "explored_path",
          "explored_path",
          "explored_path",
          "extension",
          "extension"
        ),
        match = c(
          "starts_with",
          "starts_with",
          "starts_with",
          "exact",
          "exact"
        ),
        pattern = c(
          "docs/deps",
          "tests/testthat",
          "data-raw",
          "png",
          "js"
        ),
        refined_assertion = c(
          "generated_website_support",
          "validation",
          "use_case_visualisation",
          "use_case_visualisation",
          "website_support"
        ),
        stringsAsFactors = FALSE
      )

    compiled_rulebook <-
      compile_rulebook(semantic_refinement_rules)

    result <-
      semantic_test_matrix |>
      refine_by_rulebook(
        target =
          workflow_context_label,
        rulebook =
          compiled_rulebook,
        prefix =
          "workflow_context"
      )

    ## Stage 1 -------------------------------------------

    expect_equal(
      result$workflow_context_1_ref,
      c(
        "etl_and_modelling",
        "etl_and_modelling",
        "package_functionality",
        "generated_website_support"
      )
    )

    ## Stage 2 -------------------------------------------

    expect_equal(
      result$workflow_context_2_ref,
      c(
        "etl_and_modelling",
        "etl_and_modelling",
        "validation",
        "generated_website_support"
      )
    )

    ## Stage 3 -------------------------------------------

    expect_equal(
      result$workflow_context_3_ref,
      c(
        "use_case_visualisation",
        "etl_and_modelling",
        "validation",
        "generated_website_support"
      )
    )

    ## Stage 4 -------------------------------------------

    expect_equal(
      result$workflow_context_4_ref,
      c(
        "use_case_visualisation",
        "etl_and_modelling",
        "validation",
        "website_support"
      )
    )

    ## Final stabilized state ----------------------------

    expect_equal(
      result$workflow_context_final,
      c(
        "use_case_visualisation",
        "etl_and_modelling",
        "validation",
        "website_support"
      )
    )
  }
)
