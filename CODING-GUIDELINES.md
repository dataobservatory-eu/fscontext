------------------------------------------------------------------------

editor_options: markdown: wrap: 80 ---

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

### Iteration, External Calls and Debuggability

- Use `lapply()`, `vapply()`, `purrr::map()` and related functions when they improve clarity and are appropriate for in-memory transformations.

- Do not assume that functional iteration is inherently faster than `for` loops. Performance depends on the operation.

- **Strongly prefer explicit `for`, `while` or `repeat` loops for repeated external API calls, command-line executables and other expensive external operations.**

- In these workflows, execution time is usually dominated by external operations, making differences in R iteration overhead negligible.

- Prefer explicit indexing so that the current item, request, file or batch can be identified immediately when execution fails.

- Structure external-call loops to support logging, error inspection, retries and resuming interrupted operations.

- Prefer preserving intermediate results and failure information over compact functional expressions.

- Use progress indicators for long-running batches where practical.

- Do not replace an explicit external-call loop with `lapply()` or `purrr::map()` solely for stylistic reasons.

- Prioritise observability, reproducibility and debuggability over minor performance improvements when interacting with systems outside R.

- Use base R's `utils::txtProgressBar()` and `utils::setTxtProgressBar()` for progress reporting in loops involving repeated external API calls or external executables.

- Prefer `style = 3` for a compact progress bar showing percentage completion.

- Close progress bars explicitly when processing finishes, including when an error interrupts execution.

- Avoid introducing additional progress-bar package dependencies.

## Style

- Follow the tidyverse style guide and common rOpenSci conventions. Even when not aiming for CRAN publication, maintain CRAN-quality documentation, including `@description`, `@details` where needed, `@return`, and executable `@examples`.
- In package development, use explicit `@importFrom` declarations for imported functions. Do not add redundant imports for functions called with explicit `package::function()` qualification.
- Document internal helper functions using `@keywords internal`. Keep internal helpers unexported and do not use `@noRd` unless explicitly requested.
- Internal helper naming: Use descriptive `snake_case` names without a leading dot. Prefer a functional prefix that identifies the related operation or subsystem, such as `copy_sha256_match()`. Internal functions remain unexported. Do not introduce leading-dot names solely to indicate internal visibility. Preserve existing naming conventions when modifying a package.
- Use compact tidyverse style.
- Keep lines under roughly 80 characters.
- Prefer horizontal compactness over vertical expansion.
- A line break must earn its place by improving readability.
- Do not place every argument, parenthesis, comma, or assignment operator on a separate line.
- Only introduce line breaks at meaningful semantic boundaries.
- Keep related arguments together.
- Avoid excessive whitespace and vertical fragmentation.
- Use one element per line only when it materially improves readability or when line-length constraints require it.

### Documentation of Meaningful Wrappers

- When a function substantially wraps, extends, or re-contextualises an existing function from a CRAN-released package, explicitly reference the underlying function in its roxygen2 documentation.

- Explain the relationship where it is not immediately obvious, particularly what the wrapper adds, changes, restricts or assumes.

- Use `@seealso` with an appropriate cross-reference, such as `\code{\link[package]{function}}`, to make the underlying function discoverable.

- Where useful, identify the underlying package and function in `@details`, especially when its behaviour or limitations materially affect the wrapper.

- Do not add such references for incidental dependencies or routine operations such as `dplyr::mutate()`, `base::match()` or `purrr::map()`.

- Preserve attribution when adapting substantive functionality, and respect the upstream package's licence.

- Avoid duplicating the upstream function's documentation; document the differences and refer readers to the original.

**The criterion is a meaningful functional relationship, not merely calling another package's function.**

### Vectors

- For short homogeneous vectors, prefer compact inline formatting.
- Use one element per line only when the vector is long, complex, or exceeds line-length constraints.
- Keep vectors on a single line whenever the complete expression fits within approximately 80 characters, regardless of nesting depth. Do not split vectors merely to align elements or conform to a multiline layout. When a vector exceeds the line-length target, use the fewest readable lines rather than placing each element on a separate line.

### Lists

- Treat a list that represents a single object similarly to a function call: keep related fields together.
- For short lists with a few fields, prefer compact formatting.
- Format each nested expression according to its own complexity. A multiline list does not require its constituent vectors, function calls or short lists to become multiline. Prefer the most compact readable representation of each component.

### Examples

Prefer:

```         
files <- files[basename(files) != basename(output_file)]

list(
  folder = "R", 
  extension = "R"
)

files = c("DESCRIPTION", "README.md", "NEWS.md", "NAMESPACE")

titles <- c(
  "R package" = "Package review bundle",
  "Quarto project" = "Project review bundle"
)

profiles <- list(
  "R package" = list(
    list(
      files = c("DESCRIPTION", "README.md", "NEWS.md", "NAMESPACE")
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

files = c(
  "DESCRIPTION", "README.md",
  "NEWS.md", "NAMESPACE"
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

## Package Development

- Use roxygen2 for documentation.
- Use testthat for unit testing.
- Use styler::style_pkg() for code styling.
- Prefer examples that run quickly and deterministically.
- Avoid introducing new dependencies unless justified by clear benefits.
- Maintain backward compatibility whenever practical.
- Prefer magrittr pipes (`%>%`) with packages that have already imported an used this dependency.
- Use base pipes (`|>`) if where there is no compelling reason to use `%>%.`

## Meaningful Contributions

Changes whose primary effect is stylistic, architectural, or organizational should be proposed only when they provide a clear and demonstrable benefit.

In particular, please avoid:

- introducing additional abstraction without reducing complexity;
- replacing existing, readable code with alternative idioms solely for stylistic reasons;
- large-scale refactoring that does not address a documented problem;
- introducing new dependencies without a compelling justification;
- changes that increase maintenance burden without providing a corresponding benefit.
