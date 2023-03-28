# ==============================================================================
# Function to read in boundary information and convert to ME list format.
# ==============================================================================

#' @param scale_factor_vector Vector containing the scale factor/s with which to
#' change the coordinate data from pixel to micron. It can be either a single
#' integer, or multiple scale factors for the different samples. The default
#' value is 1.
#' 
#' @export
readBoundaries <- function(data_dir,
                            pattern = NULL,
                            n_samples = NULL,
                            segment_id_col = NULL,
                            x_col = NULL,
                            y_col = NULL,
                            keep_cols = "essential",
                            boundaries_assay = NULL,
                            scale_factor_vector = 1
                            ) {
    if (is.null(pattern)) {
        stop("Please specify the character pattern with which to uniquely
        identify the boundary files of interest. For example, 
        cell_boundaries.csv.")
    } else if (is.null(n_samples)) {
        stop("Please specify the number of samples being considered.")
    } else if (is.null(boundaries_assay)) {
        stop("Please specify the name of the list in which to store
        boundary information in the boundaries slot. For example, cells
        if importing cell boundaries, or nucleus if importing nucleus
        boundaries.")
    } else if (is.null(segment_id_col)) {
        stop("Please specify the name of the column containing the segment 
        IDs. For example, \"cell_id\" for cell boundary files.")
    }

    # locate files with pattern in specified data directory
    f_paths <- vector("list", n_samples)

    fs <- list.files(data_dir,
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
    if (length(scale_factor_vector) == 1) {
        # if all samples have same scale factor, create vector with rep numbers
        scale_factor_vector <- rep(scale_factor_vector, n_samples)
    } else if (identical(length(scale_factor_vector), n_samples)) {
        stop("The vector of scale factors should be either one value for all
        samples, or a vector of the length of the number of samples, specifying
        a scale factor for each sample")
    }

    # read in files for each sample
    bds_ls <- vector("list", n_samples)
    for (s in seq_along(bds_ls)) {
        # read in data
        bds_df <- data.table::fread(f_paths[[s]])

        # standardise column names
        essential_cols <- .get_essential_cols(factor_col = segment_id_col,
                                                x_col,
                                                y_col)

        standard_cols <- .get_standard_cols(df_type = "boundaries")

        bds_df <- .standardise_cols(bds_df, standard_cols, essential_cols)

        cols <- .select_cols(bds_df, keep_cols, standard_cols)
        # scale column
        bds_df <- .scale_locations(bds_df,
                                    scale_factor = scale_factor_vector[[s]])

        # standardise csv to same list of lists format as readMolecules
        # structure should be: me@boundaries$cells$sample1$cellID$vertex_df
        bds_ls[[s]] <- .standardise_to_list(bds_df, cols, segment_id)
    }

    # specify id names
    names(bds_ls) <- .get_sample_id(n_samples, f_paths)

    # add list header to specify location in boundaries slot
    bds_ls <- list(bds_ls)
    names(bds_ls) <- get(quote(boundaries_assay))
    return(bds_ls)
}
