# ==============================================================================
# future development: a wrapper for cosmx data
# ==============================================================================

#' Read in Cosmx data (Nanostring) as an ME object.
#'
#' This function is a wrapper around the readMolecules function. Note that it
#' can currently only create a simple ME object with the molecules slot filled.
#' Boundary information is not handled yet.
#'
#' @param dataDir Character string specifying the directory with the Cosmx
#' output files.
#' @param keepCols Character string specifying which columns to keep.
#' Defaults to "essential". The other option is to select "all", or custom
#' columns by specifying their names in a vector.
#' @return A MoleculeExperiment object
readCosmx <- function(dataDir,
                      keepCols = "essential") {

    # check arg validity
    .check_if_character(dataDir, keepCols)

    # create MoleculeExperiment object
    pattern <- "tx_file"
    me <- readMolecules(dataDir = dataDir,
                        pattern = "tx_file",
                        featureCol = "target",
                        xCol = "x_global_px",
                        yCol = "y_global_px",
                        keepCols = keepCols)

    return(me)
}