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

# create me with just x and y locations
me <- readMolecules(data_dir,
                    pattern = "transcripts.csv",
                    n_samples = 3,
                    keep_cols = "essential",
                    essential_cols = c("feature_name", "x_location", "y_location")
                    )

me
summary(me@molecules)
# look at overhead structure of nested list
str(me@molecules, max.level = 1)
lobstr::obj_size(me@molecules) # 2.89 GB

# what if all feature data is retained?
me_entire <- readMolecules(data_dir,
                           n_samples = 3,
                           keep_all_cols = TRUE
                           )
lobstr::obj_size(me_entire@molecules) # 8.68 GB

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

# -----------------------------------------------------------------------------
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
# test wrappers around the readMolecules() function
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain"
me_xenium <- readXenium(data_dir,
                        n_samples = 3,
                        keep_cols = "essential",
                        add_boundaries = FALSE)
# try out adding boundary information
me_xenium <- readXenium(data_dir,
                        n_samples = 3,
                        keep_cols = "essential",
                        add_boundaries = TRUE)

# TODO readCosmx and readMerscope for some reason create a tibble for each 
# transcript, not GENE

# test readCosmx()
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images"
me_cosmx <- readCosmx(data_dir,
                      n_samples = 1,
                      keep_cols = "essential")

# test readMerscope()
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/vizgen_HumanOvarianCancerPatient2Slice2"
me_vizgen <- readMerscope(data_dir,
                          n_samples = 1,
                          keep_cols = "essential")

# =============================================================================
# test asME()
# input = data.frame with essential columns, already read into R
transcripts_df <- data.table::fread("/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs/transcripts.csv.gz")
me_csv <- asMe(transcripts_df = transcripts_df,
               n_samples = 1,
               technology = "xenium"
)

# =============================================================================
# check validators
# modified obj that is NOT valid as an ME object should yield an error message

# modifiedObject <-
validObject("modifiedObject")

# =============================================================================
# Test readBoundaries function by itself (only works for Xenium for now)
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain"
full_me <- readBoundaries(me_xenium,
                            boundaries_mode = "cells",
                            data_dir = data_dir,
                            pattern = "cell_boundaries.csv",
                            n_samples = 3)

str(full_me@boundaries, 2)

# =============================================================================
# test METHODS for ME class

# show method
me

# getters
molecules(me)
molecules(me, "raw")
boundaries(me)
boundaries(me, "cells")

# setters
readBoundaries(me,
                boundaries_mode = "nuclei",
                data_dir = "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain",
                pattern = "nucleus_boundaries.csv",
                n_samples = 3
                )

# test another getter
boundaries(me, "nuclei")

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
