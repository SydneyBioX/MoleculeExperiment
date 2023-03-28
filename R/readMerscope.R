#' Read in Merscope data to an ME object
#' 
#' Reads in Merscope data (Vizgen) from a directory, and standardises it into
#' a MoleculeExperiment object. It is essentially a wrapper around the function
#' readMolecules(). Note that this function can currently only create a simple
#' ME object with the molecules slot filled. Boundary information cannot be
#' handled yet.
#'
#' @export

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


