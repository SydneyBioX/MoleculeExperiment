# GOAL: test functions in development, for XENIUM data

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
# =============================================================================
# testing nested list versus data.frame format with large xenium dataset

data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain"

# create me with just x and y locations
me_xenium <- readMolecules(data_dir,
                    pattern = "transcripts.csv",
                    n_samples = 3,
                    feature_col = "feature_name",
                    x_col = "x_location",
                    y_col = "y_location",
                    keep_cols = "essential")

me_xenium
summary(me_xenium@molecules)
# look at overhead structure of nested list
str(me_xenium@molecules, max.level = 1)
lobstr::obj_size(me_xenium@molecules) # 2.89 GB

# what if all feature data is retained?
me_xenium_entire <- readMolecules(data_dir,
                           pattern = "transcripts.csv",
                           n_samples = 3,
                           feature_col = "feature_name",
                           x_col = "x_location",
                           y_col = "y_location",
                           keep_cols = "all"
                           )
lobstr::obj_size(me_xenium_entire@molecules) # 8.68 GB

# how different is the list of dfs to the alternative huge df with all data?
tran1 <- c("/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs/transcripts.csv.gz")
tran2 <- c("/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_2_outs/transcripts.csv.gz")
tran3 <- c("/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_3_outs/transcripts.csv.gz")
df_1 <- data.table::fread(tran1)
df_2 <- data.table::fread(tran2)
df_3 <- data.table::fread(tran3)

huge_df <- base::rbind(df_1, df_2, df_3)
class(huge_df) # both a data.table and data.frame
dim(huge_df)

lobstr::obj_size(huge_df) # 10.13 GB (redundant data)
# in contrast, simple me is 2.89 GB, and me with all data is 8.68 GB

# ==============================================================================
# ==============================================================================
# working with MINI XENIUM dataset on github repo

repo_dir <- "/dski/nobackup/bpeters/SpatialUtils/inst/extdata/mouse_brain_mini_xenium"

# test readMolecules()
simple_me <- readMolecules(repo_dir,
                            pattern = "transcripts.csv",
                            n_samples = 2,
                            feature_col = "feature_name",
                            x_col = "x_location",
                            y_col = "y_location",
                            keep_cols = "essential")
strMolecules(simple_me)

# test readBoundaries()
nuclei_ls <- readBoundaries(data_dir = repo_dir,
                            pattern = "nucleus_boundaries.csv",
                            n_samples = 2,
                            compartment_id_col = "cell_id",
                            x_col = "vertex_x",
                            y_col = "vertex_y",
                            keep_cols = "essential",
                            boundaries_mode = "nucleus")
nuclei_ls

# test readXenium() wrapper
me <- readXenium(repo_dir,
                        n_samples = 2,
                        keep_cols = "essential",
                        add_boundaries = FALSE)

identical(simple_me@molecules, me@molecules) # TRUE

# try out reading boundary information too
me <- readXenium(repo_dir,
                        n_samples = 2,
                        keep_cols = "essential",
                        add_boundaries = TRUE)
strBoundaries(me)
boundaries(me)

# =============================================================================
# investigate MoleculeExperiment() class

is(me, "MoleculeExperiment") # should be TRUE
is(me, "SummarizedExperiment") # should be FALSE
is(me, "SingleCellExperiment") # should be FALSE

# preview documentation
?readMolecules
?MoleculeExperiment
class?MoleculeExperiment
getClass("MoleculeExperiment")

# =============================================================================
# test METHODS for ME class

# getters
strMolecules(me)
strBoundaries(me)
molecules(me)
molecules(me, "raw")
molecules(me, "raw", flatten = TRUE)
boundaries(me)
boundaries(me, "cells", flatten = TRUE)

# setters
# e.g., add nucleus boundaries to obj that already has cell boundaries
boundaries(me, "nuclei") <- nuclei_ls

# test another getter
boundaries(me, "nuclei")

# =============================================================================
# test transitioning from a MoleculeExperiment to a SpatialExperiment

spe <- countMolecules()

# check class of countMolecules()
is(spe, "MoleculeExperiment") # should be TRUE
is(spe, "SummarizedExperiment") # should be TRUE
is(spe, "SingleCellExperiment") # should be TRUE
is(spe, "SpatialExperiment") # should be TRUE

# run package check
devtools::check()
# if 0 errors and 0 warnings, check installing package in new script