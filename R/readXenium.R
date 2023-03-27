# =============================================================================
# wrapper around readMolecules, specifically for xenium data
# =============================================================================

#' Read xenium data into a MoleculeExperiment object
#'
# TODO inherit documentation for parameters from readMolecules
# TODO add documentation for new parameters specific to readXenium

#' @param data_dir
#' @export
readXenium <- function(data_dir,
                       n_samples = NULL,
                       keep_cols = "essential",
                       add_boundaries = TRUE) {

    # create MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = "transcripts.csv",
                        n_samples = n_samples,
                        feature_col = "feature_name",
                        x_col = "x_location",
                        y_col = "y_location",
                        keep_cols = keep_cols)

    # add CELL boundary information if available
    if (add_boundaries) {
        boundaries_mode <- "cells"
        bds_ls <- readBoundaries(data_dir = data_dir,
                                 pattern = "cell_boundaries.csv",
                                 n_samples = n_samples,
                                 compartment_id_col = "cell_id",
                                 x_col = "vertex_x",
                                 y_col = "vertex_y",
                                 keep_cols = keep_cols,
                                 boundaries_mode = boundaries_mode)

        # add standardised boundaries list to the @boundaries slot
        boundaries(me, boundaries_mode) <- bds_ls
        # guide user
        cat("Boundary information can be accessed with boundaries(me)\n")
    }

    return(me)
}