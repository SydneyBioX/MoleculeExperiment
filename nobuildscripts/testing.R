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
# test readMolecules.R
# example arguments: 
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"
technology <- "vizgen"

standardised_transcripts <- readMolecules(data_dir, technology)

# preview help file
?readMolecules

devtools::check()

###############################################################################
# test MoleculeExperiment.R
obj <- MoleculeExperiment() 


# preview help file
?MoleculeExperiment


# run package check 
devtools::check()
# if 0 errors and 0 warnings, check installing package in new script
