# =============================================================================
# Wrapper around readMolecules to read Cosmx data from Nanostring into ME obj
# TODO finish docs
# =============================================================================


#' @param essential_cols Character string specifying the namaes of the columns
#' gene names, and x and y locations. Note that the name of the essential
#' columns needs to be given in the following order: gene names, x location
#' and y location.

readCosmx <- function(data_dir,
                      n_samples = NULL,
                      keep_cols = "essential") {

    # things specific to Cosmx
    # specify essential columns
        # the colnames are changed in readMolecules()
    essential_cols <- c("target", "x_global_px", "y_global_px")

    pattern <- "tx_file"

    # check that there are no rownames
    # better to avoid rownames in large datasets
    # if(length(rownames(input))!=0){
        # remove rownames and keep info in new col
    # }

    # create MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = pattern,
                        n_samples = n_samples,
                        keep_cols = keep_cols,
                        essential_cols = essential_cols
                        )

    return(me)

}

