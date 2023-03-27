# =============================================================================
# Read Merscope data by Vizgen into a MoleculeExperiment object
# =============================================================================


readMerscope <- function(data_dir,
                         n_samples = 1,
                         keep_cols = "essential") {

    # create simple MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = "transcripts.csv",
                        n_samples = n_samples,
                        feature_col = "gene",
                        x_col = "global_x",
                        y_col = "global_y",
                        keep_cols = keep_cols)

    return(me)

}


