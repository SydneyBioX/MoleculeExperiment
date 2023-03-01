# GOAL: test functions in development  

setwd("/dski/nobackup/bpeters/SpatialUtils")

library(devtools)

# load functions in development
devtools::load_all()

devtools::check()

## test readMolecules.R
# example arguments: 
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"
technology <- "vizgen"

standardised_transcripts <- readMolecules(data_dir, technology)

# document function 
# then update documentation and generate NAMESPACE for that function
devtools::document()

# preview help file
?readMolecules

## test MoleculeExperiment.R
obj <- MoleculeExperiment() 

# add docs to function
# then update docs
devtools::document()

# preview help file
?MoleculeExperiment


# run package check 
devtools::check()
# if 0 errors and 0 warnings, check installing package in new script
