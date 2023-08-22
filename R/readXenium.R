#' Read in Xenium data into a MoleculeExperiment object
#'
#' Function to read in, and standardise, Xenium output data into an ME object.
#' Detected transcripts files are required. Additionally, it is also possible
#' to read in boundary files ("cell", "nuclei", or both). This function is
#' a wrapper around readMolecules and readBoundaries functions.
#'
#' @param dataDir Character string specifying the directory with the xenium
#' output files.
#' @param keepCols Vector of characters specifying the columns of interest from
#' the transcripts file and boundaries file. Can be "all" or "essential".
#' "essential" selects columns with gene names, x and y locations in the
#' transcripts file; "essential" selects columns with cell ids, and x and y
#' locations for the vertices defining the boundaries in the boundaries file.
#' @param addBoundaries Vector with which to specify the names of the boundary
#' assays to be added to the me object. Can be "cell", "nucleus", both, or NULL.
#' The latter will lead to the creation of a simple ME object with just the
#' molecules slot filled.
#'
#' @return A MoleculeExperiment object containing xenium data.
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
#'
#' me <- readXenium(repoDir,
#'                   keepCols = "essential")
#' me
#' @importFrom rjson fromJSON
readXenium <- function(dataDir,
                       keepCols = "essential",
                       addBoundaries = "cell") {

    # check arg validity
    .check_if_character(dataDir, keepCols, addBoundaries)

    # create MoleculeExperiment object
    me <- readMolecules(dataDir = dataDir,
                        pattern = "transcripts.csv",
                        featureCol = "feature_name",
                        xCol = "x_location",
                        yCol = "y_location",
                        keepCols = keepCols)

    # add boundary information
    if (!is.null(addBoundaries)) {
        boundariesAssay <- addBoundaries
        for(a in boundariesAssay) {
            bds_ls <- readBoundaries(dataDir = dataDir,
                                    pattern = paste0(a, "_boundaries.csv"),
                                    segmentIDCol = "cell_id",
                                    xCol = "vertex_x",
                                    yCol = "vertex_y",
                                    keepCols = keepCols,
                                    boundariesAssay = a,
                                    # boundary info and transcript info in
                                    # xenium are both in microns
                                    scaleFactorVector = 1)

            # add standardised boundaries list to the @boundaries slot
            boundaries(me, a) <- bds_ls
        }
    }

###############################################################################
    # future dev: use pixel size info for working with images
    # for each sample, find experiment.xenium JSON file
    f_paths <- list.files(dataDir,
                     pattern = "experiment.xenium",
                     # store full path names
                     full.names = TRUE,
                     # look into subdirectories too
                     recursive = TRUE
    )

    # for each sample, get "pixel_size" value
    scale_factors <- lapply(f_paths, function(x) {
        info <- fromJSON(file = x)
        scale_factor <- info$pixel_size
        })

    # assign corresponding sample names to the scale factors
    names(scale_factors) <- .get_sample_id(n_samples = length(f_paths), f_paths)
###############################################################################

    return(me)
}
