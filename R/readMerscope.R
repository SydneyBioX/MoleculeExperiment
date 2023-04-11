# ==============================================================================
# future development: a wrapper to get vizgen data into an ME object
# ==============================================================================
#' Read in Merscope data to an ME object
#'
#' Reads in Merscope data (Vizgen) from a directory, and standardises it into
#' a MoleculeExperiment object. It is essentially a wrapper around the function
#' readMolecules(). Note that this function can currently only create a simple
#' ME object with the molecules slot filled. Boundary information cannot be
#' handled yet.
#'
#' @param dataDir Character string specifying the directory with the Cosmx
#' output files.
#' @param nSamples Integer specifying number of samples to be read.
#' Defaults to 1.
#' @param keepCols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @return A MoleculeExperiment object
readMerscope <- function(dataDir,
                         nSamples = 1,
                         keepCols = "essential") {

    # create simple MoleculeExperiment object
    me <- readMolecules(dataDir = dataDir,
                        pattern = "transcripts.csv",
                        nSamples = nSamples,
                        featureCol = "gene",
                        xCol = "global_x",
                        yCol = "global_y",
                        keepCols = keepCols)

    return(me)

}