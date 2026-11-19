# Regression tests for filesystem observation identity.
#
# Current contract:
# - storage_path_id identifies a path within a storage context.
# - observation_id combines storage_id, rel_path, and scan_time
#   formatted to whole seconds.
# - Identical identity inputs produce identical identifiers.
#
# Future improvements (not implemented or tested):
# - Distinguish observations made within the same second.
# - Distinguish independent scan events with identical timestamps.
# These would require an explicit identity and compatibility decision.
#
# Run locally:
# devtools::test(filter = "^observation-id-collisions$")

collision_fixture <- function() {
  root <- fs::file_temp("fscontext_id_collision_")
  fs::dir_create(root)

  writeLines("unchanged evidence", fs::path(root, "evidence.txt"))

  root
}

collision_scan <- function(root, scan_time) {
  scan_storage(
    root = root,
    storage_id = "collision-store",
    person_id = "observer",
    scan_time = scan_time,
    compute_signature = FALSE
  )
}

test_that("observation IDs use second-resolution timestamps", {
  root <- collision_fixture()
  withr::defer(fs::dir_delete(root))

  at <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")

  first <- collision_scan(root, at + 0.1)
  second <- collision_scan(root, at + 0.8)

  combined <- add_snapshot_context(rbind(first, second))

  expect_equal(nrow(combined), 2L)

  expect_identical(
    combined$storage_path_id[1],
    combined$storage_path_id[2]
  )

  # The underlying timestamps retain subsecond precision.
  expect_false(identical(
    combined$scan_time[1],
    combined$scan_time[2]
  ))

  # The current identifier format uses whole seconds.
  expect_identical(
    combined$observation_id[1],
    combined$observation_id[2]
  )
})

test_that("identical observation contexts produce identical IDs", {
  root <- collision_fixture()
  withr::defer(fs::dir_delete(root))

  at <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")

  first <- collision_scan(root, at)
  second <- collision_scan(root, at)

  combined <- add_snapshot_context(rbind(first, second))

  expect_equal(nrow(combined), 2L)

  expect_identical(
    combined$storage_path_id[1],
    combined$storage_path_id[2]
  )

  expect_identical(
    combined$scan_time[1],
    combined$scan_time[2]
  )

  expect_identical(
    combined$observation_id[1],
    combined$observation_id[2]
  )
})

test_that("contextualizing the same scan is deterministic", {
  root <- collision_fixture()
  withr::defer(fs::dir_delete(root))

  at <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  scan <- collision_scan(root, at)

  first <- add_snapshot_context(scan)
  second <- add_snapshot_context(scan)

  expect_identical(
    first$observation_id,
    second$observation_id
  )

  expect_identical(
    first$storage_path_id,
    second$storage_path_id
  )
})

test_that("different files in one scan have distinct IDs", {
  root <- collision_fixture()
  withr::defer(fs::dir_delete(root))

  writeLines("second file", fs::path(root, "second.txt"))

  at <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
  scan <- collision_scan(root, at)

  result <- add_snapshot_context(scan)

  expect_equal(nrow(result), 2L)

  expect_equal(
    length(unique(result$observation_id)),
    nrow(result)
  )

  expect_equal(
    length(unique(result$storage_path_id)),
    nrow(result)
  )
})
