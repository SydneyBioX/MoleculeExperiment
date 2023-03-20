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
me <- readMolecules(data_dir, technology = "xenium", n_samples = 3)
summary(me@molecules)
# look at overhead structure of nested list
str(me@molecules, max.level = 1)
lobstr::obj_size(me@molecules) # 2.89 GB

# what if all feature data is retained?
me_entire <- readMolecules(data_dir,
                           technology = "xenium",
                           n_samples = 3,
                           keep_all_cols = TRUE
                           )
lobstr::obj_size(me_entire@molecules) # 8.68 GB

# preview help file
?readMolecules

devtools::check()

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

