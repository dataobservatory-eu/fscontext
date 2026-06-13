test_that(
  "matches_context_root matches exact roots",
  {
    expect_equal(
      matches_context_root(
        x = c("D:/alpha", "D:/beta"),
        roots = "D:/alpha"
      ),
      c(TRUE, FALSE)
    )
  }
)

test_that(
  "matches_context_root matches descendants",
  {
    expect_equal(
      matches_context_root(
        x = c(
          "D:/alpha",
          "D:/alpha/R",
          "D:/alpha/R/import"
        ),
        roots = "D:/alpha"
      ),
      c(TRUE, TRUE, TRUE)
    )
  }
)

test_that(
  "matches_context_root excludes unrelated paths",
  {
    expect_equal(
      matches_context_root(
        x = c(
          "D:/alpha",
          "D:/beta",
          "D:/gamma"
        ),
        roots = "D:/beta"
      ),
      c(FALSE, TRUE, FALSE)
    )
  }
)

test_that(
  "matches_context_root normalises separators",
  {
    expect_equal(
      matches_context_root(
        x = "D:\\alpha\\R",
        roots = "D:/alpha"
      ),
      TRUE
    )
  }
)


test_that(
  "matches_context_root does not partially match names",
  {
    expect_equal(
      matches_context_root(
        x = c(
          "D:/alpha",
          "D:/alphabet"
        ),
        roots = "D:/alpha"
      ),
      c(TRUE, FALSE)
    )
  }
)
