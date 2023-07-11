# MoleculeExperiment

<!-- badges: start -->
<!-- badges: end -->

The goal of MoleculeExperiment is to provide functionality for the representation and summarisation of imaging-based spatial transcriptomics data, including 10X Xenium.

## Installation

The latest release of MoleculeExperiment can be installed using:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("MoleculeExperiment")
```

You can install the development version of MoleculeExperiment from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SydneyBioX/MoleculeExperiment")
```
### System dependencies
Note that the following 3 system dependencies are required:
* libssl-dev
* libmagick++-dev
* libgdal-dev
