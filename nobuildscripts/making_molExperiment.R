# GOAL: make & test function to create moleculeExperiment object

# set working directory to the directory of the R package
setwd("/dski/nobackup/bpeters/SpatialUtils")

library(devtools)

devtools::load_all() # load functions still in development to test them here

devtools::check() # run sanity check on the R package 

# add documentation to the function file
# then update documentation and generate NAMESPACE
devtools::document()

# preview help file 
?readMolecules



# update docs
devtools::document()


# do final check
devtools::check() 

# install package 
devtools::install()
# package should now be able to be loaded to other scripts via library()
