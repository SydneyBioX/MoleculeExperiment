#' Read in Xenium data into a MoleculeExperiment object
#' 
#' Function to read in, and standardise, Xenium output into an ME object.
#' Detected transcript files are required. Additionally, it is also possible
#' to read in boundary files ("cell", "nuclei", or both). When boundaries are
#' read in, the function also reads in the scale factor information from the
#' experiment.xenium json files for each sample. This function is essentially
#' a wrapper around readMolecules and readBoundaries functions.
#'
#' @param data_dir Inherit docs from readMolecules HERE
#' #TODO @param n_samples DESCRIPTION. Defaults to NULL.
#' #TODO @param keep_cols DESCRIPTION. Defaults to "essential".
#' @param add_boundaries Vector with which to specify the names of the boundary
#' assays to be added to the me object. Can be "cell", "nucleus", both, or NULL.
#' The latter will lead to the creation of a simple ME object with just the
#' molecules slot filled.
#' @export
#' @importFrom rjson fromJSON
#' 
readXenium <- function(data_dir,
                       n_samples = NULL,
                       keep_cols = "essential",
                       add_boundaries = "cell") {

    # create MoleculeExperiment object
    me <- readMolecules(data_dir = data_dir,
                        pattern = "transcripts.csv",
                        n_samples = n_samples,
                        feature_col = "feature_name",
                        x_col = "x_location",
                        y_col = "y_location",
                        keep_cols = keep_cols)

    # for each sample, find experiment.xenium JSON file
    f_paths <- vector("list", n_samples)
    fs <- list.files(data_dir,
                     pattern = "experiment.xenium",
                     # store full path names
                     full.names = TRUE,
                     # look into subdirectories too
                     recursive = TRUE
    )
    f_paths <- replace(f_paths, values = fs)

    # for each sample, get "pixel_size" value
    scale_factors <- sapply(f_paths, function(x) {
        info <- fromJSON(file = x)
        scale_factor <- info$pixel_size
        })

    # assign corresponding sample names to the scale factors
    names(scale_factors) <- .get_sample_id(n_samples, f_paths)

    # TODO add checks so that user is guided to include a directory with
    # exactly the n_samples

    # add boundary information
    if (!is.null(add_boundaries)) {
        boundaries_assay <- add_boundaries
        for(a in boundaries_assay) {
            bds_ls <- readBoundaries(data_dir = data_dir,
                                    pattern = paste0(a, "_boundaries.csv"),
                                    n_samples = n_samples,
                                    segment_id_col = "cell_id",
                                    x_col = "vertex_x",
                                    y_col = "vertex_y",
                                    keep_cols = keep_cols,
                                    boundaries_assay = a,
                                    scale_factor_vector = scale_factors)

            # add standardised boundaries list to the @boundaries slot
            boundaries(me, a) <- bds_ls
        }
        # guide user
        cat("Boundary information can be accessed with boundaries(me)\n")

    }

    return(me)
}
