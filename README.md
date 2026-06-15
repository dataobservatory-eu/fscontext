
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The fscontext R Package <a href='https://fscontext.dataobservatory.eu/'><img src="man/figures/logo.png" align="right"/></a>

<!-- badges: start -->

[![rhub](https://github.com/dataobservatory-eu/fscontext/actions/workflows/rhub.yaml/badge.svg)](https://github.com/dataobservatory-eu/fscontext/actions/workflows/rhub.yaml)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![devel-version](https://img.shields.io/badge/devel%20version-0.2.0-blue.svg)](https://github.com/dataobservatory-eu/fscontext)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![codecov](https://app.codecov.io/gh/dataobservatory-eu/fscontext/graph/badge.svg)](https://app.codecov.io/gh/dataobservatory-eu/fscontext)

<!-- badges: end -->

`fscontext` provides a provenance-aware contextual reconstruction
framework for file systems and related digital resource collections.

The package creates reproducible observational snapshots of files,
repository structures, and related operational resources, and supports
their contextual abstraction, semantic stabilization, and
reconstruction-oriented analysis.

## Installation

    # CRAN release
    install.packages("fscontext")

    # Latest development version
    pak::pak("dataobservatory-eu/fscontext")

## Getting started

The package includes two introductory vignettes:

- [Introduction to fscontext](articles/intro.html) introduces filesystem
  observations, contextualisation, and Record Set construction.
- [Prelabelled values and semantic
  stabilisation](articles/prelabelled.html) demonstrates progressive
  semantic enrichment and refinement workflows.

These vignettes provide a guided introduction to the observational,
contextual, and semantic layers of the package.

## Contextualisation

Many digital collections contain valuable contextual information but
little documentation explaining how files, datasets, reports, source
code, inventories, or digital surrogates relate to one another.

Examples include research projects spread across multiple repositories,
digitised archival collections with evolving inventories, audiovisual
production environments, long-running analytical projects, and shared
drives that have accumulated over many years.

Before semantic integration, archival description, provenance modelling,
or knowledge graph construction can begin, it is often necessary to
reconstruct the context in which digital resources were created and
used.

`fscontext` approaches filesystems as observational environments. Files,
folders, timestamps, repository structures, and other digital traces are
treated as evidence from which contextual structures can be
reconstructed.

    Filesystem observations  
          ↓ 
    Contextualisation  
          ↓ 
    Record Sets  
          ↓ 
    Semantic stabilisation           
          ↓
    Knowledge systems  

Rather than replacing archival description or provenance models,
`fscontext` focuses on the earlier task of contextual reconstruction.

The package is inspired by the archival conceptual model [Records in
Contexts](https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-ric/)
(RiC), developed by the *International Council on Archives*. Rather than
implementing RiC-CM or RiC-O directly, `fscontext` focuses on the
earlier task of contextual reconstruction: deriving contextual
relationships and candidate `Record Sets` from filesystem observations,
repository structures, inventories, and other digital traces.

For more information, see:

- [RiC-CM
  1.0](https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-conceptual-model/)
  (Records in Contexts Conceptual Model)
- [RiC-O 1.1](https://www.ica.org/standards/RiC/RiC-O_1-1.html) (Records
  in Contexts Ontology)

## A reproducible example

The package includes two example filesystem snapshots derived from the
companion repository `fscontextdemo`.

The demonstration repository is available at:
<https://github.com/dataobservatory-eu/fscontextdemo>

It contains a small but realistic digital work environment including
source code, datasets, generated artefacts, documentation, tests,
package metadata, semantic enrichment examples.

The snapshots, `fscontextdemo_snapshot_01` and
`fscontextdemo_snapshot_02`, capture the repository at different points
in time, allowing reconstruction and longitudinal analysis workflows to
be demonstrated reproducibly.

``` r
library(fscontext)  
data("fscontextdemo_snapshot_02")  
fscontextdemo_snapshot_02 |>
  subset(
    select=c(storage_id, rel_path, filename, quick_sig)
    ) |>
  head() 
#>      storage_id                           rel_path
#> 1 fscontextdemo                 .github/.gitignore
#> 2 fscontextdemo     .github/workflows/pkgdown.yaml
#> 3 fscontextdemo                         .gitignore
#> 4 fscontextdemo                      .Rbuildignore
#> 5 fscontextdemo data/fscontextdemo_snapshot_01.rda
#> 6 fscontextdemo       data/fsdemo_country_data.rda
#>                        filename                  quick_sig
#> 1                    .gitignore                   db6ad734
#> 2                  pkgdown.yaml          5eb4aaba_6cbfbdf4
#> 3                    .gitignore                   e73cf12f
#> 4                 .Rbuildignore                   09ab8617
#> 5 fscontextdemo_snapshot_01.rda 03dd3533_36abd309_cb1736f4
#> 6       fsdemo_country_data.rda                   f7e65210
```

The snapshot records observed filesystem resources together with
contextual information such as relative paths, timestamps, extensions,
and storage identifiers.

Contextual identifiers can then be added:

``` r
data("fscontextdemo_snapshot_02")
snapshot <- add_snapshot_context(fscontextdemo_snapshot_02)

snapshot |>
  subset(
    select = c(storage_path_id, observation_id, rel_path)
  )|>
  head() 
#>                                     storage_path_id
#> 1                 fscontextdemo::.github/.gitignore
#> 2     fscontextdemo::.github/workflows/pkgdown.yaml
#> 3                         fscontextdemo::.gitignore
#> 4                      fscontextdemo::.Rbuildignore
#> 5 fscontextdemo::data/fscontextdemo_snapshot_01.rda
#> 6       fscontextdemo::data/fsdemo_country_data.rda
#>                                                       observation_id
#> 1                 fscontextdemo::.github/.gitignore::20260525-174640
#> 2     fscontextdemo::.github/workflows/pkgdown.yaml::20260525-174640
#> 3                         fscontextdemo::.gitignore::20260525-174640
#> 4                      fscontextdemo::.Rbuildignore::20260525-174640
#> 5 fscontextdemo::data/fscontextdemo_snapshot_01.rda::20260525-174640
#> 6       fscontextdemo::data/fsdemo_country_data.rda::20260525-174640
#>                             rel_path
#> 1                 .github/.gitignore
#> 2     .github/workflows/pkgdown.yaml
#> 3                         .gitignore
#> 4                      .Rbuildignore
#> 5 data/fscontextdemo_snapshot_01.rda
#> 6       data/fsdemo_country_data.rda
```

The examples above demonstrate only the observational layer. Subsequent
workflows can derive contextual Record Sets, compare repeated
observations over time, identify duplicate resources, analyse activity
patterns, and support semantic stabilisation.

See the package vignettes for complete end-to-end examples.

## Core concepts

The package separates three complementary analytical layers:

| Layer | Purpose |
|----|----|
| observational | reproducible observations of digital resources and their filesystem context |
| contextual | grouping observations into Record Sets, projects, collections, and reconstruction workspaces |
| analytical | reconstruction, temporal comparison, activity analysis, and semantic stabilisation |

The framework intentionally separates observational evidence, contextual
abstraction, semantic interpretation, and analytical reconstruction. In
RiC-inspired terms, filesystem observations represent observed digital
resources and their associated instantiations at a particular point in
time. These observations may later be aggregated into contextual
`Record Sets`, while preserving the distinction between the observed
resource itself and the contextual structures derived from it.

## What this package does not do

The package does not modify files. It is not intended to replace version
control systems, reconstruct file contents, infer authoritative archival
hierarchy, or perform ontology-complete provenance modelling.

## Notes

- Large scans may require substantial time on slower or networked
  storage systems.
- Some files may be inaccessible due to permissions or synchronization
  state.
- Observational snapshots are intended for reproducible local or
  institutional analysis workflows.
- Contextual and analytical layers may evolve independently from the
  original observational corpus.

<!-- -->

<!-- -->
