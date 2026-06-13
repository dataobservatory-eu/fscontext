
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fscontextdemo

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20382570.svg)](https://doi.org/10.5281/zenodo.20382570)
<!-- badges: end -->

`fscontextdemo` is a minimal demonstration package used to test,
document, and explain provenance-aware filesystem reconstruction
workflows with the `fscontext` and `dataset` packages.

The package provides:

- a small but realistic R package structure
- simple multilingual example functions
- generated artefacts and datasets
- Git history suitable for provenance reconstruction
- reproducible examples for vignettes and testing

The repository is intentionally designed as a small self-contained
playground for:

- filesystem observation
- contextual reconstruction
- provenance-aware reporting
- GitHub event reconstruction
- semantic enrichment workflows

without exposing personal or confidential project material.

## Installation

You can install the development version of `fscontextdemo` from GitHub
with:

``` r
# install.packages("pak")
pak::pak("dataobservatory-eu/fscontextdemo")
```

## Example

``` r
library(fscontextdemo)

hello_world()
#> [1] "Hello world!"
```

## Development goals

The package is intentionally minimal and experimental.

It is used to:

- develop `fscontext` reporting workflows
- test provenance-aware event journals
- demonstrate `dataset` semantic metadata patterns
- create reproducible examples for documentation and tutorials
- provide realistic integration testing scenarios

## Related packages

- `fscontext`
- `dataset`
