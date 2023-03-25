# =============================================================================
# Read Merscope data by Vizgen into a MoleculeExperiment object
# =============================================================================


readMerscope <- function(data_dir,
                         n_samples = 1,
                         keep_cols = "essential") {

    # things specific to Merscope (vizgen)
    # specify essential columns
        # the colnames are changed in readMolecules()
    essential_cols <- c("gene", "global_x", "global_y")

    pattern <- "transcripts.csv"

    # create simple MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = pattern,
                        n_samples = n_samples,
                        keep_cols = keep_cols,
                        essential_cols = essential_cols
                        )

    return(me)

}


