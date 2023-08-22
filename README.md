# MoleculeExperiment

<!-- badges: start -->
[![Static Badge](https://img.shields.io/badge/BIOCONDUCTOR%20TUTORIAL-%23001F3F)](https://bioconductor.org/packages/release/bioc/vignettes/MoleculeExperiment/inst/doc/MoleculeExperiment.html)
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

### Citation
Peters Couto et al, 2023, Bioinformatics (accepted)

[![Static Badge](https://img.shields.io/badge/PREPRINT-darkgreen)](https://www.biorxiv.org/content/10.1101/2023.05.16.541040v1#:~:text=MoleculeExperiment%20enables%20consistent%20infrastructure%20for,transcriptomics%20data%20in%20Bioconductor%20%7C%20bioRxiv)
