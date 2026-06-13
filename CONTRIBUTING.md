# Contributing

Thank you for your interest in contributing to reprextemplates.

This package contains utility functions, templates, and workflows used\
to support reproducible examples, package development, and related\
research software activities. Contributions that improve reliability,\
clarity, maintainability, and developer experience are particularly\
welcome.

## Repository Structure

This repository follows standard R package conventions described in\
*Writing R Extensions*.

The most important locations are:

- `R/` – package source code

- `tests/testthat/` – unit tests

- `man/` – generated documentation

- `vignettes/` – package vignettes

- `inst/` – installed package resources

Some files and directories are excluded from the built package through\
`.Rbuildignore` and exist solely to support package development.

## Documentation

Documentation is generated with `roxygen2`.

Please do not edit files in `man/` or `NAMESPACE` directly.\
Instead, edit the corresponding source files in `R/` and regenerate\
documentation with:

```         
devtools::document()
```

Examples should be included whenever appropriate. Examples requiring\
internet access, user interaction, or substantial execution time should\
be wrapped in `\\dontrun{}`.

## Code Style

Code should follow the tidyverse style guide and common rOpenSci\
conventions.

Project-specific coding conventions are documented in:

- `CODING-GUIDELINES.md`

Please consult that document before making substantial changes.

## Testing

New functionality and bug fixes should be accompanied by appropriate\
unit tests whenever practical.

Tests use the `testthat` framework and are located in\
`tests/testthat/`.

Before submitting changes, please ensure that:

```         
devtools::document() devtools::test() devtools::check()
```

run successfully.

## Pull Requests

For larger changes, please open an issue first to discuss the proposed\
approach.

When submitting a pull request:

- Keep changes focused and easy to review.

- Include tests where appropriate.

- Update documentation where necessary.

- Add a `NEWS.md` entry for user-facing changes.

## Meaningful Contributions

Contributions should solve a demonstrated problem, improve usability,fix a bug,
strengthen testing, improve documentation, or otherwiseprovide clear value to 
users and maintainers.

- As a general principle, this project values maintainability,
  readability, and long-term stewardship over stylistic uniformity or
  micro-optimizations.

- Code that is executed infrequently may reasonably prioritize review clarity
  over marginal runtime performance improvements.
  
- When proposing refactoring changes (and not responding to known issues or 
  known bugs), please explain: the problem being solved or the lack of 
  sufficiency  in the existing implementation and the the expected benefit of 
  the proposed change.

## Code of Conduct

This project follows the Contributor Code of Conduct described in\
`CODE_OF_CONDUCT.md`.

By participating in this project, you agree to abide by its terms.
