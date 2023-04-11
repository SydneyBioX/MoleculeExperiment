#' Read in csv boundary information and convert to ME list format.
#'
#' This function reads in csv boundary files and converts them to the ME list
#' format, so that they can be added to an ME object later on. To account for
#' different coordinate scales possible being used by the boundary versus
#' transcript information, this function scales the coordinate values of the
#' boundaries to match the unit of the detected transcript locations.
#' The various arguments offer flexibility to standardise data from different
#' molecule-based ST technologies into the ME list format.
#'
#' @param dataDir Path of the directory containing the boundary csv files.
#' @param pattern Character string specifying the unique pattern with which to
#' identify the files of interest in the directory. This is useful to work with
#' multiple samples. Defaults to NULL.
#' @param nSamples Integer indicating the number of samples. Defaults to NULL.
#' @param segmentIDCol Character string specifying the name of the column
#' containing the sample id. Defaults to NULL.
#' @param xCol Character string specifying the name of the column containing
#' the x coordinates of the vertices defining the boundaries. Defaults to NULL.
#' @param yCol Character string specifying the name of the column containing
#' the y coordinates of the vertices defining the boundaries. Defaults to NULL.
#' @param keepCols Character string specifying which columns to keep.
#' Defaults to "essential". The other option is to select "all", or custom
#' columns by specifying their names in a vector.
#' @param boundariesAssay Character string specifying the name with which to
#' identify the boundary data in the ME object later on. Defaults to NULL.
#' @param scaleFactorVector Vector containing the scale factor/s with which to
#' change the coordinate data from pixel to micron. It can be either a single
#' integer, or multiple scale factors for the different samples. The default
#' value is 1.
#' @return An ME list containing the boundary information. This can be used as
#' input to the boundaries slot of an ME object.
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' nucleiMEList <- readBoundaries(dataDir = repoDir,
#'                             pattern = "nucleus_boundaries.csv",
#'                             nSamples = 2,
#'                             segmentIDCol = "cell_id",
#'                             xCol = "vertex_x",
#'                             yCol = "vertex_y",
#'                             keepCols = "essential",
#'                             boundariesAssay = "nucleus",
#'                             scaleFactorVector = 1)
#' nucleiMEList
readBoundaries <- function(dataDir,
                            pattern = NULL,
                            nSamples = NULL,
                            segmentIDCol = NULL,
                            xCol = NULL,
                            yCol = NULL,
                            keepCols = "essential",
                            boundariesAssay = NULL,
                            scaleFactorVector = 1
                            ) {
    if (is.null(pattern)) {
        stop("Please specify the character pattern with which to uniquely
        identify the boundary files of interest. For example, 
        cell_boundaries.csv.")
    } else if (is.null(nSamples)) {
        stop("Please specify the number of samples being considered.")
    } else if (is.null(boundariesAssay)) {
        stop("Please specify the name of the list in which to store
        boundary information in the boundaries slot. For example, cells
        if importing cell boundaries, or nucleus if importing nucleus
        boundaries.")
    } else if (is.null(segmentIDCol)) {
        stop("Please specify the name of the column containing the segment 
        IDs. For example, \"cell_id\" for cell boundary files.")
    }

    # locate files with pattern in specified data directory
    f_paths <- vector("list", nSamples)

    fs <- list.files(dataDir,
                    pattern = pattern,
                    # store full path names
                    full.names = TRUE,
                    # look into subdirectories too
                    recursive = TRUE
    )

    f_paths <- replace(f_paths, values = fs)

    if (length(f_paths) == 0) {
        stop("Could not find any files with the specified pattern. Please
        specify the pattern with which to find boundary files of interest.")
    }

    # get vector of scale factors for all samples
    if (length(scaleFactorVector) == 1) {
        # if all samples have same scale factor, create vector with rep numbers
        scaleFactorVector <- rep(scaleFactorVector, nSamples)
    } else if (identical(length(scaleFactorVector), nSamples)) {
        stop("The vector of scale factors should be either one value for all
        samples, or a vector of the length of the number of samples, specifying
        a scale factor for each sample")
    }

    # read in files for each sample
    bds_ls <- vector("list", nSamples)
    for (s in seq_along(bds_ls)) {
        # read in data
        bds_df <- data.table::fread(f_paths[[s]])

        # standardise column names
        essential_cols <- .get_essential_cols(factor_col = segmentIDCol,
                                                x_col = xCol,
                                                y_col = yCol)

        standard_cols <- .get_standard_cols(df_type = "boundaries")

        bds_df <- .standardise_cols(bds_df, standard_cols, essential_cols)

        cols <- .select_cols(bds_df, keep_cols = keepCols, standard_cols)
        # scale column
        bds_df <- .scale_locations(bds_df,
                                    scale_factor = scaleFactorVector[[s]])

        # standardise csv to same list of lists format as readMolecules
        # structure should be: me@boundaries$cells$sample1$cellID$vertex_df
        bds_ls[[s]] <- .standardise_to_list(bds_df, cols, "segment_id")
    }

    # specify id names
    names(bds_ls) <- .get_sample_id(n_samples = nSamples, f_paths)

    # add list header to specify location in boundaries slot
    bds_ls <- list(bds_ls)
    names(bds_ls) <- get(quote(boundariesAssay))
    return(bds_ls)
}
