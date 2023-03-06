# GOAL: test functions in development  

setwd("/dski/nobackup/bpeters/SpatialUtils")

library(devtools)

# regularly add docs to functions
# update docs
devtools::document()

# load functions in development
devtools::load_all()

devtools::check()

###############################################################################
# test readMolecules()
# example arguments: 
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"
technology <- "vizgen"

standardised_transcripts <- readMolecules(data_dir, technology)

# preview help file
?readMolecules

devtools::check()

###############################################################################
# test MoleculeExperiment() constructor
me <- MoleculeExperiment(data_dir)

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

