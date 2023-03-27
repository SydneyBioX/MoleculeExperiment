# =============================================================================
# Wrapper around readMolecules to read Cosmx data from Nanostring into ME obj
# TODO finish docs
# =============================================================================

readCosmx <- function(data_dir,
                      n_samples = NULL,
                      keep_cols = "essential") {

    pattern <- "tx_file"

    # create MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = "tx_file",
                        n_samples = n_samples,
                        feature_col = "target",
                        x_col = "x_global_px",
                        y_col = "y_global_px",
                        keep_cols = keep_cols)

    return(me)

}

