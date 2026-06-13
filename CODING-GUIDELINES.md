# Coding Guidelines

## Development Principles

- Prefer readable code over clever code.
- Write code that is easy to maintain by humans.
- Use early validation with informative error messages.
- Assume the code will be reviewed and maintained by experienced R package developers.
- Preserve existing function interfaces unless there is a compelling reason to change them.
- When refactoring, make the smallest change that solves the problem.
- Avoid unnecessary nesting, abstraction, and over-engineering.
- Prefer simple lists, vectors, and helper functions over complex object structures.
- Prefer extending existing code to introducing new architectures.
- When debugging, identify the immediate cause of a failure before proposing broader redesigns.

## Style

- Follow the tidyverse style guide and common rOpenSci conventions.
- Use compact tidyverse style.
- Keep lines under roughly 80 characters.
- Prefer horizontal compactness over vertical expansion.
- A line break must earn its place by improving readability.
- Do not place every argument, parenthesis, comma, or assignment operator
  on a separate line.
- Only introduce line breaks at meaningful semantic boundaries.
- Keep related arguments together.
- Avoid excessive whitespace and vertical fragmentation.
- Use one element per line only when it materially improves readability
  or when line-length constraints require it.

### Vectors

- For short homogeneous vectors, prefer compact inline formatting.
- Use one element per line only when the vector is long, complex, or
  exceeds line-length constraints.

### Lists

- Treat a list that represents a single object similarly to a function
  call: keep related fields together.
- For short lists with a few fields, prefer compact formatting.

### Examples

Prefer:

```
files <- files[basename(files) != basename(output_file)]

list(
  folder = "R", 
  extension = "R"
)

files = c(
  "DESCRIPTION", "README.md",
  "NEWS.md", "NAMESPACE"
)

titles <- c(
  "R package" = "Package review bundle",
  "Quarto project" = "Project review bundle"
)

profiles <- list(
  "R package" = list(
    list(
      files = c(
        "DESCRIPTION", "README.md",
        "NEWS.md", "NAMESPACE"
      )
    ),
    list(folder = "R", extension = "R"),
    list(
      folder = "vignettes",
      extension = c("Rmd", "qmd")
    )
  )
)
```

Over:

```
files <- files[
  basename(files) != basename(output_file)
]

list(folder = "R", extension = "R")

files = c(
  "DESCRIPTION",
  "README.md",
  "NEWS.md",
  "NAMESPACE"
)

titles <- c(
  "R package" =
    "Package review bundle",
  "Quarto project" =
    "Project review bundle"
)

profiles <- list(

  "R package" = list(

    list(
      files = c(
        "DESCRIPTION",
        "README.md",
        "NEWS.md",
        "NAMESPACE"
      )
    ),

    list(
      folder = "R",
      extension = "R"
    )

  )

)
```

## Package Development

- Use roxygen2 for documentation.
- Use testthat for unit testing.
- Use styler::style_pkg() for code styling.
- Prefer examples that run quickly and deterministically.
- Avoid introducing new dependencies unless justified by clear benefits.
- Maintain backward compatibility whenever practical.
- Prefer magrittr pipes (`%>%`) with packages that have already imported an used
  this dependency, have extensive tidyverse dependencies. 
- Use base pipes (`|>`) if there is  a compelling to do so.

## Meaningful Contributions

Changes whose primary effect is stylistic, architectural, or 
organizational should be proposed only when they provide a clear and
demonstrable benefit.

In particular, please avoid:

- introducing additional abstraction without reducing complexity;
- replacing existing, readable code with alternative idioms solely for
  stylistic reasons;
- large-scale refactoring that does not address a documented problem;
- introducing new dependencies without a compelling justification;
- changes that increase maintenance burden without providing a
  corresponding benefit.
