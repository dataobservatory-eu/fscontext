create_demo_snapshot_dir <- function() {
  data("fscontextdemo_snapshot_01")
  data("fscontextdemo_snapshot_02")
  
  tmp <- tempfile("observe-universe-")
  dir.create(tmp)
  
  saveRDS(fscontextdemo_snapshot_01, file.path(tmp, "snapshot_01.rds"))
  saveRDS(fscontextdemo_snapshot_02, file.path(tmp, "snapshot_02.rds"))
  tmp
}

# ------------------------------------------------------------------
# observe_universe()
# ------------------------------------------------------------------

test_that(
  "observe_universe counts repeated snapshot observations",
  {
    tmp <- tempfile("observe-repeat-")
    dir.create(tmp)
    
    saveRDS(fscontextdemo_snapshot_01, file.path(tmp, "a.rds"))
    
    saveRDS(fscontextdemo_snapshot_01, file.path(tmp, "b.rds"))
    
    res <- observe_universe(snapshot_dir = tmp, max_aggregation_depth = 2)
    
    expect_true(
      any(res$n_observations == 2)
    )
  }
)

test_that(
  "observe_universe creates longitudinal observational summaries",
  {
    data("fscontextdemo_snapshot_01")
    tmp <- create_demo_snapshot_dir()

    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    saveRDS(
      fscontextdemo_snapshot_01,
      file.path(
        tmp,
        "snapshot_009.rds"
      )
    )

    saveRDS(
      fscontextdemo_snapshot_01,
      file.path(
        tmp,
        "snapshot_010.rds"
      )
    )

    saveRDS(
      fscontextdemo_snapshot_02,
      file.path(
        tmp,
        "snapshot_012.rds"
      )
    )

    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2
    )

    expect_s3_class(
      res,
      "tbl_df"
    )

    expect_true(
      all(
        c(
          "observed_unit",
          "aggregation_depth",
          "max_aggregation_depth",
          "n_observations",
          "avg_files_unit",
          "avg_size_unit",
          "avg_size_mb_unit",
          "avg_size_gb_unit",
          "total_files_unit",
          "total_size_unit"
        ) %in% names(res)
      )
    )

    expect_true(all(res$n_observations >= 1))

    expect_true(all(res$total_files_unit >= res$avg_files_unit))

    expect_true(all(res$total_size_unit >= res$avg_size_unit))
  }
)


test_that(
  "observe_universe preserves storage boundaries",
  {
    tmp <- create_demo_snapshot_dir()

    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2,
      by_storage = TRUE
    )

    expect_true(
      "storage_id" %in% names(res)
    )
  }
)


test_that(
  "observe_universe optionally preserves person boundaries",
  {
    tmp <- create_demo_snapshot_dir()

    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2,
      by_person = TRUE
    )

    expect_true("person_id" %in% names(res))
  }
)


test_that(
  "observe_universe returns positive size summaries",
  {
    tmp <- create_demo_snapshot_dir()
    
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2
    )

    expect_true(
      all(res$avg_size_unit >= 0)
    )

    expect_true(
      all(res$avg_size_mb_unit >= 0)
    )

    expect_true(
      all(res$avg_size_gb_unit >= 0)
    )
  }
)


# ------------------------------------------------------------------
# aggregation depth behaviour
# ------------------------------------------------------------------

test_that(
  "observe_universe records operational aggregation depth",
  {
    tmp <- create_demo_snapshot_dir()
    
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    test_depth_snapshot <- tibble::tibble(
      full_path = c(
        "D:/hello.R",
        "D:/packages/hello.R",
        "D:/packages/R/hello.R",
        "D:/work/packages/R/hello.R"
      ),
      size = c(1, 1, 1, 1),
      storage_id = "test-storage",
      person_id = "test-person"
    )

    saveRDS(
      test_depth_snapshot,
      file.path(tmp, "depth_test.rds")
    )

    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 3
    )

    expect_equal(
      sort(unique(res$aggregation_depth)),
      c(0, 1, 2, 3)
    )

    expect_true(
      all(
        res$aggregation_depth <=
          res$max_aggregation_depth
      )
    )

    expect_equal(
      aggregation_depth(
        res$observed_unit
      ),
      res$aggregation_depth
    )
  }
)


test_that(
  "observe_universe derives different aggregation units at different depths",
  {
    tmp <- tempfile("observe-depth-")
    dir.create(tmp)
    
    saveRDS(
      tibble::tibble(
        full_path = "D:/packages/R/hello.R",
        size = 1,
        storage_id = "test-storage",
        person_id = "test-person"
      ),
      file.path(tmp, "depth_test.rds")
    )
    
    res_depth_1 <- observe_universe(tmp, 1)
    res_depth_2 <- observe_universe(tmp, 2)
    res_depth_3 <- observe_universe(tmp, 3)
    
    expect_equal(res_depth_1$observed_unit, "D:/packages")
    expect_equal(res_depth_2$observed_unit, "D:/packages/R")
    expect_equal(res_depth_3$observed_unit, "D:/packages/R")
    
    expect_equal(res_depth_1$aggregation_depth, 1)
    expect_equal(res_depth_2$aggregation_depth, 2)
    expect_equal(res_depth_3$aggregation_depth, 2)
  }
)

test_that(
  "observe_universe excludes operational artefacts by default",
  {
    tmp <- create_demo_snapshot_dir()
    
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    test_snapshot <- tibble::tibble(
      full_path = c(
        "D:/packages/pkg/.gitignore",
        "D:/packages/pkg/.quarto/config.yml",
        "D:/packages/pkg/R/hello.R"
      ),
      size = c(1, 1, 1),
      storage_id = "test-storage",
      person_id = "test-person"
    )

    saveRDS(test_snapshot, file.path(tmp, "exclude_test.rds"))

    res <- observe_universe(snapshot_dir = tmp, max_aggregation_depth = 3)

    expect_false(
      any(grepl("\\.gitignore|\\.quarto", res$observed_unit))
    )
  }
)

test_that(
  "observe_universe supports custom exclusion patterns",
  {
    tmp <- create_demo_snapshot_dir()
    
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    test_snapshot <- tibble::tibble(
      full_path = c(
        "D:/packages/pkg/cache/file.txt",
        "D:/packages/pkg/R/hello.R"
      ),
      size = c(1, 1),
      storage_id = "test-storage",
      person_id = "test-person"
    )

    saveRDS(test_snapshot, file.path(tmp, "custom_exclude_test.rds"))

    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 3,
      exclude_patterns = "cache"
    )

    expect_false(any(grepl("cache", res$observed_unit)))
  }
)

test_that(
  "observe_universe never treats files as aggregation units",
  {
    tmp <- tempfile("observe-files-")
    dir.create(tmp)
    
    test_snapshot <- tibble::tibble(
      full_path = c(
        "D:/_packages/fscontextdemo/docs/sitemap.xml",
        "D:/_packages/fscontextdemo/README.md",
        "D:/_packages/fscontextdemo/vignettes/demo.Rmd"
      ),
      size = c(1, 1, 1),
      storage_id = "fscontextdemo",
      person_id = "demo_user"
    )
    
    saveRDS(test_snapshot, file.path(tmp, "file_test.rds"))
    
    res <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 3
    )
    
    # No aggregation unit should end with a filename extension
    expect_false(
      any(grepl("\\.(xml|md|Rmd)$", res$observed_unit))
    )
    
    # Aggregation units must be derived from folders
    expect_true(all(aggregation_depth(res$observed_unit) >= 0))
  }
)

test_that(
  "observe_universe errors on non-tabular RDS objects",
  {
    tmp <- create_demo_snapshot_dir()
    
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    saveRDS(list(a = 1), file.path(tmp, "bad.rds"))

    expect_error(observe_universe(snapshot_dir = tmp), 
                 "does not contain a data frame")
  }
)

test_that(
  "observe_universe errors on empty snapshot directories",
  {
    tmp <- tempfile("observe-universe-empty-")
    
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    
    expect_error(observe_universe(snapshot_dir = tmp),
                 "No snapshot files found")
  }
)

test_that(
  "observe_universe errors on missing snapshot directories",
  {
    expect_error(
      observe_universe(
        snapshot_dir = file.path(
          tempdir(),
          "this_directory_should_not_exist"
        )
      ),
      "snapshot_dir does not exist"
    )
  }
)
