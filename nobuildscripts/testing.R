# GOAL: test functions in development

setwd("/dski/nobackup/bpeters/SpatialUtils")

library(devtools)
library(lobstr)

# load functions in development, as well as their dependencies
devtools::load_all()

devtools::check()

# regularly add docs to functions
# update docs
devtools::document()

# use_package("package") to add packages needed for functions in DESCRIPTION


# =============================================================================
# test readMolecules()

# test for xenium 10x genomics
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain"

me <- readMolecules(data_dir, technology = "xenium", n_samples = 3)
summary(mols)
str(mols)
lobstr::obj_size(mols) # 2.89 GB 
# note that transcripts.csv files together are 5GB 
# so 2.89 GB is a considerable reduction
# should we try to reduce the size more?

# preview help file
?readMolecules

devtools::check()

# -----------------------------------------------------------------------------
# test readMolecules for cosmx smi nanostring
data_dir <-"/dski/nobackup/bpeters/cellCommData_2023/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images"

me <- readMolecules(data_dir, "nanostring")

# test readMolecules for merscope vizgen 
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/vizgen_HumanOvarianCancerPatient2Slice2"

me <- readMolecules(data_dir, "vizgen")

# =============================================================================
# investigate MoleculeExperiment() class

is(me, "MoleculeExperiment") # should be TRUE
is(me, "SummarizedExperiment") # should be FALSE 
is(me, "SingleCellExperiment") # should be FALSE

# check obj size
lobstr::obj_size(me)

# preview class documentation 
?MoleculeExperiment
class?MoleculeExperiment
getClass("MoleculeExperiment")


# =============================================================================
# check validators
# modified obj that is NOT valid as an ME object should yield an error message

# modifiedObject <-
validObject("modifiedObject")

# =============================================================================
# test METHODS for ME class



# =============================================================================
# test countMolecules()

cme <- countMolecules()

# check class of countMolecules()
is(cme, "MoleculeExperiment") # should be TRUE
is(cme, "SummarizedExperiment") # should be TRUE
is(cme, "SingleCellExperiment") # should be TRUE
is(cme, "SpatialExperiment") # should be TRUE

# run package check 
devtools::check()
# if 0 errors and 0 warnings, check installing package in new script

