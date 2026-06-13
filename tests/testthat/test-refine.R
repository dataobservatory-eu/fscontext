test_that("refine() validates inputs", {
  expect_error(
    out <- refine(
      x = list(a = 4),
      target = rep("unresolved", 4),
      rules = tibble::tibble(explored_path = "docs"),
      by = "explored_path",
      assertion = "rendered_reporting",
      match = "starts_with"
    ),
    regexp = "must inherit from data.frame"
  )

  expect_error(
    out <- refine(
      x = tibble::tibble(explored_path = "docs"),
      target = tibble::tibble(explored_path = "docs"),
      rules = list(a = 4),
      by = "explored_path",
      assertion = "rendered_reporting",
      match = "starts_with"
    ),
    regexp = "must inherit from data.frame"
  )
})

test_that("refine() supports exact matching", {
  files <- tibble::tibble(
    extension = c("r", "png", "csv")
  )

  out <- refine(
    x = files,
    target = rep("unresolved", 3),
    rules = tibble::tibble(extension = "r"),
    by = "extension",
    assertion = "source_code"
  )

  expect_equal(
    out,
    c("source_code", "unresolved", "unresolved")
  )
})

test_that("refine() supports starts_with matching", {
  files <- tibble::tibble(
    explored_path = c(
      "docs",
      "docs/deps",
      "docs/deps/bootstrap",
      "R"
    )
  )

  out <- refine(
    x = files,
    target = rep("unresolved", 4),
    rules = tibble::tibble(explored_path = "docs"),
    by = "explored_path",
    assertion = "rendered_reporting",
    match = "starts_with"
  )

  expect_equal(
    out,
    c(
      "rendered_reporting",
      "rendered_reporting",
      "rendered_reporting",
      "unresolved"
    )
  )
})

test_that("refine() supports ends_with matching", {
  files <- tibble::tibble(
    filename = c(
      "report_final.pdf",
      "report_draft.pdf",
      "notes.txt"
    )
  )

  out <- refine(
    x = files,
    target = rep("unresolved", 3),
    rules = tibble::tibble(filename = ".pdf"),
    by = "filename",
    assertion = "publication_output",
    match = "ends_with"
  )

  expect_equal(
    out,
    c(
      "publication_output",
      "publication_output",
      "unresolved"
    )
  )
})

test_that("refine() supports contains matching", {
  files <- tibble::tibble(
    filename = c(
      "film_A_table.rtf",
      "film_B_plot.png",
      "README.md"
    )
  )

  out <- refine(
    x = files,
    target = rep("unresolved", 3),
    rules = tibble::tibble(filename = "film_"),
    by = "filename",
    assertion = "film_case_evidence",
    match = "contains"
  )

  expect_equal(
    out,
    c(
      "film_case_evidence",
      "film_case_evidence",
      "unresolved"
    )
  )
})

test_that("refine() initializes missing target", {
  files <- tibble::tibble(
    extension = c("r", "png")
  )

  out <- refine(
    x = files,
    rules = tibble::tibble(extension = "r"),
    by = "extension",
    assertion = "source_code"
  )

  expect_equal(
    out,
    c("source_code", NA_character_)
  )
})

test_that("refine() preserves row cardinality", {
  files <- tibble::tibble(
    extension = c("r", "png", "csv")
  )

  out <- refine(
    x = files,
    target = rep("unresolved", 3),
    rules = tibble::tibble(extension = "r"),
    by = "extension",
    assertion = "source_code"
  )

  expect_equal(
    length(out),
    nrow(files)
  )
})

test_that("refine() preserves comment attribute", {
  files <- tibble::tibble(
    extension = "r"
  )

  out <- refine(
    x = files,
    target = "unresolved",
    rules = tibble::tibble(extension = "r"),
    by = "extension",
    assertion = "source_code",
    comment = "R files refined as source code."
  )

  expect_equal(
    attr(out, "comment"),
    "R files refined as source code."
  )
})

test_that("refine() errors on missing by columns", {
  files <- tibble::tibble(
    extension = "r"
  )

  expect_error(
    refine(
      x = files,
      rules = tibble::tibble(filename = "x"),
      by = "filename",
      assertion = "x"
    ),
    "must exist in `x`"
  )
})

test_that("refine() errors on wrong target length", {
  files <- tibble::tibble(
    extension = c("r", "png")
  )

  expect_error(
    refine(
      x = files,
      target = "x",
      rules = tibble::tibble(extension = "r"),
      by = "extension",
      assertion = "source_code"
    ),
    "same length"
  )
})

test_that("refine() supports dual contextual rules", {
  files <- tibble::tibble(
    filename = c(
      "filmA.png",
      "filmB.png",
      "film.xlsx",
      "fill.png"
    ),
    extension = c(
      "png",
      "png",
      "xlsx",
      "png"
    )
  )

  out <- refine(
    x = files,
    target =
      rep(
        "unresolved",
        nrow(files)
      ),
    rules =
      tibble::tibble(
        filename = "film",
        extension = "png"
      ),
    by = c(
      "filename",
      "extension"
    ),
    match = c(
      "starts_with",
      "exact"
    ),
    assertion =
      "film_visualisation"
  )

  expect_equal(
    out,
    c(
      "film_visualisation",
      "film_visualisation",
      "unresolved",
      "unresolved"
    )
  )
})
