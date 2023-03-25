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
                       add_boundaries = TRUE,
                       boundaries_mode = "cells"
                       ) {

    # things specific to XENIUM
    transcripts_pattern <- "transcripts.csv"
    essential_cols <- c("feature_name",
                        "x_location",
                        "y_location")


    # create MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = transcripts_pattern,
                        n_samples = n_samples,
                        keep_cols = keep_cols,
                        essential_cols = essential_cols
                        )

    # add boundary information if available
    # boundaries will always want transcripts
    boundaries_pattern <- "cell_boundaries.csv"
    if (add_boundaries) {
        bds_ls <- readBoundaries(boundaries_mode = boundaries_mode,
                        data_dir = data_dir,
                        pattern = boundaries_pattern,
                        n_samples = n_samples)

        # add standardised boundaries list to the @boundaries slot
        boundaries(me, boundaries_mode) <- bds_ls
        # guide user
        cat("Boundary information can be accessed with boundaries(me)\n")
    }

    return(me)
}