test_that(
  "summarise_duplicates works on fscontextdemo snapshots",
  {
    data("fscontextdemo_snapshot_01")
    data("fscontextdemo_snapshot_02")

    combined_snapshot <- rbind(
      fscontextdemo_snapshot_01,
      fscontextdemo_snapshot_02
    )

    res <- summarise_duplicates(combined_snapshot)

    expect_s3_class(res, "data.frame")

    expect_true(nrow(res) > 0)

    expect_true(all(c(
      "filename",
      "total_copies",
      "identical_copies",
      "versioned_copies",
      "n_versions"
    ) %in% names(res)))

    expect_true(any(res$total_copies > 1))
  }
)

# ---------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------

test_dup_snapshot <- data.frame(
  filename = c(
    "a.R",
    "b.R", "b.R", "b.R",
    "c.R", "c.R",
    "d.R", "d.R", "d.R",
    "e.RData", "e.RData", "e.RData"
  ),
  rel_path = c(
    "proj/a.R",
    "proj1/b.R", "proj2/b.R", "proj3/b.R",
    "proj1/c.R", "proj2/c.R",
    "proj1/d.R", "proj2/d.R", "proj3/d.R",
    "proj1/e.RData", "proj2/e.RData", "proj3/e.RData"
  ),
  quick_sig = c(
    "sig1",
    "sig2", "sig2", "sig2",
    "sig3", "sig4",
    "sig5", "sig5", "sig6",
    "sig7", "sig8", "sig9"
  ),
  stringsAsFactors = FALSE
)

expected_duplicates <- data.frame(
  filename = c("a.R", "b.R", "c.R", "d.R", "e.RData"),
  total_copies = c(1, 3, 2, 3, 3),
  identical_copies = c(0, 3, 0, 2, 0),
  versioned_copies = c(0, 0, 1, 1, 2),
  n_versions = c(1, 1, 2, 2, 3),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------
# Core behaviour
# ---------------------------------------------------------------------

test_that("duplicate classification works", {
  res <- summarise_duplicates(test_dup_snapshot)

  # enforce deterministic ordering
  res <- res[order(res$filename), ]
  exp <- expected_duplicates[order(expected_duplicates$filename), ]

  # enforce identical structure
  expect_equal(names(res), names(exp))

  # compare values
  expect_equal(res, exp)
})

# ---------------------------------------------------------------------
# NA handling
# ---------------------------------------------------------------------

test_that("NA signatures are treated as identical", {
  df <- data.frame(
    filename = c("x.R", "x.R"),
    rel_path = c("a/x.R", "b/x.R"),
    quick_sig = c(NA, NA),
    stringsAsFactors = FALSE
  )

  res <- summarise_duplicates(df)

  expect_equal(res$total_copies, 2)
  expect_equal(res$n_versions, 1) # NA = one group
  expect_equal(res$identical_copies, 2)
  expect_equal(res$versioned_copies, 0)
})

# ---------------------------------------------------------------------
# Mixed NA + non-NA (important forensic case)
# ---------------------------------------------------------------------

test_that("mixed NA and non-NA signatures are versioned", {
  df <- data.frame(
    filename = c("x.R", "x.R"),
    rel_path = c("a/x.R", "b/x.R"),
    quick_sig = c(NA, "sig1"),
    stringsAsFactors = FALSE
  )

  res <- summarise_duplicates(df)

  expect_equal(res$total_copies, 2)
  expect_equal(res$n_versions, 2)
  expect_equal(res$identical_copies, 0)
  expect_equal(res$versioned_copies, 1)
})

# ---------------------------------------------------------------------
# Schema enforcement
# ---------------------------------------------------------------------

test_that("fails without filename", {
  df <- test_dup_snapshot
  df$filename <- NULL

  expect_error(
    summarise_duplicates(df),
    "Missing required columns"
  )
})

test_that("fails without quick_sig", {
  df <- test_dup_snapshot
  df$quick_sig <- NULL

  expect_error(
    summarise_duplicates(df)
  )
})

# ---------------------------------------------------------------------
# No legacy columns leak
# ---------------------------------------------------------------------

test_that("no legacy column names in output", {
  res <- summarise_duplicates(test_dup_snapshot)

  expect_length(
    intersect(names(res), c("file", "files", "folder")),
    0
  )
})
