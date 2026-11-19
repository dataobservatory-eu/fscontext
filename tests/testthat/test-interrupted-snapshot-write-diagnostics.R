# Controlled write-failure diagnostics for fscontext::save_scan().
# Tests use temporary directories and injected saveRDS() failures.
# They do not terminate R processes or simulate concurrent writers.

interrupt_fixture <- function() {
  root <- tempfile("fscontext_interrupt_")
  dir.create(root, recursive = TRUE)
  root
}

interrupt_snapshot <- function(at, value = "original") {
  x <- data.frame(
    storage_id = "interrupt-store",
    rel_path = "item.txt",
    size = nchar(value),
    stringsAsFactors = FALSE
  )
  attr(x, "created_at") <- at
  attr(x, "schema_version") <- "0.1.3"
  x
}

# Inject saveRDS() without modifying the fscontext namespace.
interrupt_writer <- function(injected_saveRDS) {
  writer <- fscontext::save_scan
  env <- new.env(parent = environment(writer))
  env$saveRDS <- injected_saveRDS
  environment(writer) <- env
  writer
}

interrupt_save <- function(x, folder, writer = fscontext::save_scan) {
  suppressMessages(
    writer(x, storage_id = "interrupt-store", path = folder)
  )
}

test_that("normal persistence produces a readable complete RDS", {
  root <- interrupt_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 10:00:00", tz = "UTC")
  x <- interrupt_snapshot(at)
  destination <- interrupt_save(x, root)

  expect_true(file.exists(destination))
  expect_identical(readRDS(destination), x)
})

test_that("failed initial write does not publish a partial snapshot", {
  root <- interrupt_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 10:01:00", tz = "UTC")
  x <- interrupt_snapshot(at)
  attempted_paths <- character()

  writer <- interrupt_writer(function(object, file, ...) {
    attempted_paths <<- c(attempted_paths, file)
    writeBin(as.raw(c(0x00, 0x01, 0x02)), file)
    stop("injected write interruption")
  })

  expect_error(
    interrupt_save(x, root, writer),
    "injected write interruption"
  )

  expect_length(attempted_paths, 1L)

  # The failed write must not leave a published RDS.
  expect_false(file.exists(attempted_paths[[1]]))

  # Temporary staging files must also be removed.
  expect_length(list.files(root,
    all.files = TRUE,
    pattern = "\\.rds$"
  ), 0L)
})

test_that("existing snapshots are protected from replacement", {
  root <- interrupt_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 10:02:00", tz = "UTC")
  original <- interrupt_snapshot(at, "original")
  destination <- interrupt_save(original, root)
  before <- readRDS(destination)

  replacement <- interrupt_snapshot(at, "longer replacement")
  writer_called <- FALSE

  writer <- interrupt_writer(function(object, file, ...) {
    writer_called <<- TRUE
    writeBin(as.raw(c(0x00, 0x01, 0x02)), file)
    stop("injected write interruption")
  })

  # Issue #5: reject the collision before attempting serialization.
  expect_error(
    interrupt_save(replacement, root, writer),
    "snapshot already exists"
  )

  expect_false(writer_called)

  # Issue #6: previously committed evidence remains unchanged.
  expect_true(file.exists(destination))
  expect_identical(readRDS(destination), before)
})

test_that("failed write does not make the output directory unusable", {
  root <- interrupt_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  at <- as.POSIXct("2026-11-19 10:03:00", tz = "UTC")
  x <- interrupt_snapshot(at)

  writer <- interrupt_writer(function(object, file, ...) {
    stop("injected write interruption")
  })

  expect_error(
    interrupt_save(x, root, writer),
    "injected write interruption"
  )

  expect_true(dir.exists(root))

  # A subsequent normal save must still succeed.
  destination <- interrupt_save(x, root)
  expect_true(file.exists(destination))
  expect_identical(readRDS(destination), x)
})

test_that("truncated RDS is rejected when loading observations", {
  root <- interrupt_fixture()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  corrupt <- file.path(root, "truncated.rds")
  writeBin(as.raw(c(0x00, 0x01, 0x02)), corrupt)

  expect_error(readRDS(corrupt))
  expect_error(read_snapshot(corrupt))
})
