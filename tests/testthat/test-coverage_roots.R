test_that(
  "coverage_roots returns expected columns",
  {
    data("fscontextdemo_snapshot_01")
    tmp <- file.path(tempdir(), "coverage_roots_structure_test")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    saveRDS(
      fscontextdemo_snapshot_01,
      file.path(tmp, "fscontextdemo_snapshot_01.rds")
    )

    provenance <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2
    )

    res <- coverage_roots(
      provenance = provenance,
      roots = provenance$observed_unit[1]
    )

    expect_s3_class(
      res,
      "tbl_df"
    )

    expect_true(
      all(
        c(
          "observed_unit",
          "included",
          "aggregation_depth",
          "max_aggregation_depth"
        ) %in% names(res)
      )
    )
  }
)


test_that(
  "coverage_roots includes selected roots",
  {
    data("fscontextdemo_snapshot_01")
    tmp <- file.path(tempdir(), "coverage_roots_inclusion_test")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

    saveRDS(
      fscontextdemo_snapshot_01,
      file.path(tmp, "snapshot_010.rds")
    )

    provenance <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2
    )

    roots <- unique(
      provenance$observed_unit
    )[1]

    res <- coverage_roots(
      provenance = provenance,
      roots = roots
    )

    expect_true(
      all(
        res$included[
          res$observed_unit %in% roots
        ]
      )
    )
  }
)

test_that(
  "coverage_roots marks included and excluded roots",
  {
    provenance <- data.frame(
      observed_unit = c(
        "D:/alpha",
        "D:/beta",
        "D:/gamma"
      ),
      aggregation_depth = c(
        1,
        1,
        1
      ),
      stringsAsFactors = FALSE
    )

    res <- coverage_roots(provenance = provenance, roots = "D:/beta")

    expect_equal(sum(res$included), 1)

    expect_equal(sum(!res$included), 2)
  }
)

test_that(
  "coverage_roots preserves aggregation semantics",
  {
    data("fscontextdemo_snapshot_02")

    tmp <- file.path(
      tempdir(),
      "coverage_roots_depth_test"
    )

    dir.create(
      tmp,
      recursive = TRUE,
      showWarnings = FALSE
    )

    saveRDS(
      fscontextdemo_snapshot_02,
      file.path(tmp, "fscontextdemo_snapshot_02.rds")
    )

    provenance <- observe_universe(
      snapshot_dir = tmp,
      max_aggregation_depth = 2
    )

    res <- coverage_roots(
      provenance = provenance,
      roots = provenance$observed_unit[1]
    )

    expect_equal(
      res$aggregation_depth,
      aggregation_depth(
        res$observed_unit
      )
    )
  }
)


test_that(
  "coverage_roots normalizes slashes and trailing separators",
  {
    provenance <- tibble::tibble(
      observed_unit =
        c(
          "D:/packages",
          "D:/packages/R"
        ),
      aggregation_depth =
        c(1, 2),
      max_aggregation_depth =
        c(2, 2)
    )

    roots <- c(
      "D:\\packages\\",
      "D:\\packages\\R\\"
    )

    res <- coverage_roots(
      provenance = provenance,
      roots = roots
    )

    expect_true(
      all(res$included)
    )
  }
)

test_that(
  "coverage_roots does not match across aggregation depths",
  {
    provenance <- tibble::tibble(
      observed_unit =
        c(
          "D:/packages",
          "D:/packages/R"
        ),
      aggregation_depth =
        c(1, 2),
      max_aggregation_depth =
        c(2, 2)
    )

    roots <- "D:/packages"

    res <- coverage_roots(
      provenance = provenance,
      roots = roots
    )

    expect_true(
      res$included[1]
    )

    expect_false(
      res$included[2]
    )
  }
)


test_that(
  "coverage_roots rejects duplicated contextual roots",
  {
    provenance <- tibble::tibble(
      observed_unit =
        "D:/packages",
      aggregation_depth =
        1,
      max_aggregation_depth =
        1
    )

    expect_error(
      coverage_roots(
        provenance = provenance,
        roots = c(
          "D:/packages",
          "D:/packages"
        )
      ),
      "Duplicated roots"
    )
  }
)

test_that(
  "coverage_roots accepts context objects",
  {
    provenance <- tibble::tibble(
      observed_unit =
        "D:/packages",
      aggregation_depth =
        1,
      max_aggregation_depth =
        1
    )

    context <- list(
      contexts = list(
        list(
          roots = "D:/packages"
        )
      )
    )

    res <- coverage_roots(
      provenance = provenance,
      roots = context
    )

    expect_true(
      res$included
    )
  }
)


test_that(
  "coverage_roots validates required columns",
  {
    provenance <- tibble::tibble(
      observed_unit = "D:/packages"
    )

    expect_error(
      coverage_roots(
        provenance = provenance,
        roots = "D:/packages"
      ),
      "Missing required columns"
    )
  }
)
