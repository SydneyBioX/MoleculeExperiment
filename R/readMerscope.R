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
#' @param data_dir Character string specifying the directory with the Cosmx
#' output files.
#' @param n_samples Integer specifying number of samples to be read.
#' Defaults to 1.
#' @param keep_cols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @return A MoleculeExperiment object
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