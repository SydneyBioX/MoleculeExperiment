# GOAL: test functions in development

setwd("/dski/nobackup/bpeters/SpatialUtils")

library(devtools)

# regularly add docs to functions
# update docs
devtools::document()

# use_package("package") to add packages needed for functions in DESCRIPTION
# e.g., usethis::use_package("magrittr")

# load functions in development, as well as their dependencies
devtools::load_all()

devtools::check()

###############################################################################
# test readMolecules()

# test for xenium 10x genomics --> transcripts.csv.gz
path_transcripts <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs/transcripts.csv.gz"
mdf <- readMolecules(path_transcripts, technology = "xenium")

# test for cosmx smi nanostring --> Lung9_Rep1_tx_file.csv
path_transcripts <-"/dski/nobackup/bpeters/cellCommData_2023/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images/Lung9_Rep1_tx_file.csv"

mdf <- readMolecules(path_transcripts, "nanostring")

# test for merscope vizgen -->
path_transcripts <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"

mdf <- readMolecules(path_transcripts, "vizgen")


# preview help file
?readMolecules

devtools::check()

###############################################################################
# test MoleculeExperiment() constructor
me <- MoleculeExperiment(molecules = mdf)

# preview class documentation 
?MoleculeExperiment
class?MoleculeExperiment
getClass("MoleculeExperiment")

# check that MoleculeExperiment inherits properties from classes of interest 

is(me, "MoleculeExperiment") # should be TRUE
is(me, "SummarizedExperiment") # should be TRUE 
is(me, "SingleCellExperiment") # should be FALSE

# add more functionalities in future
# e.g., add filtered transcripts data to the molecule slot in the object

# output should look like this
# class: MoleculeExperiment
# molecules = list(raw = list(mdf_1, mdf_2), thresholded = list(mdf_1, mdf_2))

###############################################################################
# check validators
# modified obj that is NOT valid as an ME object should yield an error message

# modifiedObject <-
validObject("modifiedObject")

###############################################################################
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

