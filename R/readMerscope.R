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
#' @param keepCols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @return A MoleculeExperiment object
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/vizgen_HumanOvarianCancerPatient2Slice2")
#' meMerscope <- readMerscope(repoDir,
#'                           keepCols = "essential")
#' meMerscope
readMerscope <- function(dataDir,
                         keepCols = "essential") {
    # check arg validity
    .check_if_character(dataDir, keepCols)

    # create simple MoleculeExperiment object
    pattern <- "detected_transcripts.csv"
    me <- readMolecules(dataDir = dataDir,
                        pattern = pattern,
                        featureCol = "gene",
                        xCol = "global_x",
                        yCol = "global_y",
                        keepCols = keepCols)

    # future development: handle complex hdf5 segmentation files

    return(me)
}