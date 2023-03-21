# =============================================================================
# wrapper around readMolecules, specifically for xenium data
# TODO document function
# =============================================================================

#' Read xenium data into a MoleculeExperiment object
#'
# inherit documentation from readMolecules?
#' @param data_dir
#'
#'

readXenium <- function(data_dir,
                       n_samples = 1,
                       keep_cols = "essential"
                       ) {

    # things specific to XENIUM
    technology <- "xenium"
    pattern <- "transcripts.csv"
    essential_cols <- c("feature_name",
                        "x_location",
                        "y_location")


    # create MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        technology = technology,
                        pattern = pattern,
                        n_samples = n_samples,
                        keep_cols = keep_cols,
                        essential_cols = essential_cols
                        )

    return(me)
}

