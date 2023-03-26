# ==============================================================================
# Setter method to add boundary file information to a MoleculeExperiment object.
# ==============================================================================

#' @rdname
readBoundaries <- function(boundaries_mode = NULL,
                            data_dir,
                            pattern = NULL,
                            n_samples = NULL,
                            compartment_id_col = NULL) {
    if (is.null(pattern)) {
        stop("Please specify the character pattern with which to uniquely
        identify the boundary files of interest. For example, 
        cell_boundaries.csv.")
    } else if (is.null(n_samples)) {
        stop("Please specify the number of samples being considered.")
    } else if (is.null(boundaries_mode)) {
        stop("Please specify the name of the list in which to store
        boundary information in the boundaries slot. For example, cells
        if importing cell boundaries, or nuclei if importing nucleus
        boundaries.")
    } else if (is.null(compartment_id_col)) {
        stop("Please specify the name of the column containing the compartment
        id's. For example, \"cell_id\" for cell boundary files.")
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

    # read in files
    bds_ls <- vector("list", n_samples)

    # iterate through each sample
    for (s in seq_along(bds_ls)) {
        # read in data
        bds_df <- data.table::fread(f_paths[[s]])

        # TODO standardise column names to x_location and y_location


        cols <- colnames(bds_df)
        # standardise csv to same list of lists format as readMolecules
        # structure should be: me@boundaries$cells$sample1$cellID$vertex_df
        bds_ls[[s]] <- .standardiseToList(bds_df, cols, compartment_id_col)
    }

    # specify id names
    names(bds_ls) <- .getSampleID(n_samples, f_paths)

    # add list header to specify location in boundaries slot
    bds_ls <- list(bds_ls)
    names(bds_ls) <- get(quote(boundaries_mode))
    return(bds_ls)
}