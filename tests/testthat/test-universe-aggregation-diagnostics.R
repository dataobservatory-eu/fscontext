# Tests-only diagnostics for observe_universe().
# Run: devtools::test(filter = "^universe-aggregation-diagnostics$")

universe_fixture <- function() {
  root <- tempfile("fscontext_universe_")
  dir.create(root, recursive = TRUE)
  root
}

universe_rows <- function(paths, sizes, storage = "store-A", person = "person-A") {
  data.frame(
    full_path = as.character(paths),
    size = as.numeric(sizes),
    storage_id = rep(storage, length(paths)),
    person_id = rep(person, length(paths)),
    stringsAsFactors = FALSE
  )
}

universe_write <- function(dir, name, x) {
  path <- file.path(dir, name)
  saveRDS(x, path)
  path
}

test_that("aggregation counts distinct snapshot files and all contributing rows", {
  folder <- universe_fixture()
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  base <- file.path(folder, "source", "docs")
  universe_write(folder, "one.rds", universe_rows(
    file.path(base, c("a.txt", "b.txt")), c(10, 20)
  ))
  universe_write(folder, "two.rds", universe_rows(
    file.path(base, c("a.txt", "c.txt")), c(10, 40)
  ))
  result <- observe_universe(folder, max_aggregation_depth = 100)
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_observations, 2L)
  expect_equal(result$total_files_unit, 4L)
  expect_equal(result$total_size_unit, 80)
  expect_equal(result$avg_files_unit, 2)
  expect_equal(result$avg_size_unit, 40)
})

test_that("groups present in only one snapshot retain one observation", {
  folder <- universe_fixture()
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  base <- file.path(folder, "source")
  universe_write(folder, "one.rds", universe_rows(
    file.path(base, "alpha", "a.txt"), 10
  ))
  universe_write(folder, "two.rds", universe_rows(
    file.path(base, "beta", "b.txt"), 20
  ))
  result <- observe_universe(folder, max_aggregation_depth = 100)
  expect_equal(nrow(result), 2L)
  expect_setequal(result$n_observations, c(1L, 1L))
  expect_setequal(result$total_files_unit, c(1L, 1L))
  expect_equal(sum(result$total_size_unit), 30)
})

test_that("storage grouping can be enabled and disabled", {
  folder <- universe_fixture()
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  path <- file.path(folder, "source", "same.txt")
  universe_write(folder, "one.rds", universe_rows(path, 10, "store-A"))
  universe_write(folder, "two.rds", universe_rows(path, 20, "store-B"))
  separate <- observe_universe(folder,
    max_aggregation_depth = 100,
    by_storage = TRUE
  )
  combined <- observe_universe(folder,
    max_aggregation_depth = 100,
    by_storage = FALSE
  )
  expect_equal(nrow(separate), 2L)
  expect_setequal(separate$storage_id, c("store-A", "store-B"))
  expect_equal(nrow(combined), 1L)
  expect_equal(combined$n_observations, 2L)
  expect_equal(combined$total_size_unit, 30)
})

test_that("person grouping can be enabled and disabled", {
  folder <- universe_fixture()
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  path <- file.path(folder, "source", "same.txt")
  universe_write(folder, "one.rds", universe_rows(path, 10, person = "person-A"))
  universe_write(folder, "two.rds", universe_rows(path, 20, person = "person-B"))
  separate <- observe_universe(folder,
    max_aggregation_depth = 100,
    by_person = TRUE
  )
  combined <- observe_universe(folder,
    max_aggregation_depth = 100,
    by_person = FALSE
  )
  expect_equal(nrow(separate), 2L)
  expect_setequal(separate$person_id, c("person-A", "person-B"))
  expect_equal(nrow(combined), 1L)
  expect_equal(combined$n_observations, 2L)
})

test_that("explicit exclusion patterns remove matching aggregation units", {
  folder <- universe_fixture()
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  base <- file.path(folder, "source")
  universe_write(folder, "one.rds", universe_rows(
    file.path(base, c("keep", "exclude"), "item.txt"), c(10, 20)
  ))
  result <- observe_universe(folder,
    max_aggregation_depth = 100,
    exclude_patterns = "(^|/)exclude$"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$total_files_unit, 1L)
  expect_equal(result$total_size_unit, 10)
  expect_true(grepl("keep", result$observed_unit, fixed = TRUE))
})

test_that("invalid snapshot directory and empty snapshot set are rejected", {
  folder <- universe_fixture()
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  expect_error(
    observe_universe(file.path(folder, "absent")),
    "snapshot_dir does not exist"
  )
  expect_error(observe_universe(folder), "No snapshot files found")
})
