
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MoleculeExperiment <img src="https://raw.githubusercontent.com/SydneyBioX/SydneyBioXStickers/main/MoleculeExperiment/MoleculeExperiment.png" align="right" width=250 style="margin-left: 10px;">

<!-- badges: start -->

[![name](https://img.shields.io/badge/BIOCONDUCTOR%20TUTORIAL-%23001F3F)](https://bioconductor.org/packages/release/bioc/vignettes/MoleculeExperiment/inst/doc/MoleculeExperiment.html)
<!-- badges: end -->

The goal of MoleculeExperiment is to provide functionality for the
representation and summarisation of imaging-based spatial
transcriptomics data, including 10X Xenium.

MoleculeExperiment will take you from machine output data directly to an
object ready for analyses! 🚀

We used the following data bundles to inform our readXenium, readCosmx
and readMerscope functions respectively. In particular, Xenium data
corresponds to [3 replicates from fresh frozen mouse brain
tissue](https://www.10xgenomics.com/resources/datasets/fresh-frozen-mouse-brain-replicates-1-standard),
accessed on 8 February 2023; CosMx data corresponds to [human non-small
cell lung
cancer](https://nanostring.com/resources/smi-ffpe-dataset-lung9-rep1-data/)
accessed on 27 February 2023; and MERSCOPE data is from [human ovarian
cancer](https://console.cloud.google.com/storage/browser/vz-ffpe-showcase/HumanOvarianCancerPatient2Slice2)
accessed on 27 February 2023.

## Installation

You can install the development version of MoleculeExperiment from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SydneyBioX/MoleculeExperiment")
```

### System dependencies

Note that the following 3 system dependencies are required: \*
libssl-dev \* libmagick++-dev \* libgdal-dev

### Citation
Peters Couto et al, 2023, Bioinformatics

[![Static Badge](https://img.shields.io/badge/LINK%20TO%20PAPER-darkgreen)](https://academic.oup.com/bioinformatics/advance-article/doi/10.1093/bioinformatics/btad550/7271181)
