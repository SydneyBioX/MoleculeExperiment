# ==============================================================================
# Setter method to add boundary file information to a MoleculeExperiment object.
# ==============================================================================

setMethod("readBoundaries",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object,
                    boundaries_mode = NULL,
                    data_dir,
                    pattern = NULL,
                    n_samples = NULL) {
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

        # TODO check that cols are in xenium format

        cols <- colnames(bds_df)
        # standardise csv to same list of lists format as readMolecules
        # structure should be: me@boundaries$cells$sample1$cellID$vertex_df
        bds_ls[[s]] <- .standardiseToList(bds_df, cols, cell_id)
    }

    # specify id names
    names(bds_ls) <- .getSampleID(n_samples, f_paths)

    # add list header to specify location in boundaries slot
    bds_ls <- list(bds_ls)
    names(bds_ls) <- get(quote(boundaries_mode))

    # add standardised boundaries list to an already existing ME object
    object@boundaries <- bds_ls
    # check that modification is valid
    validObject(object)

    # guide user
    cat("Boundary information can be accessed with boundaries(me).")

# end of method definition
            }
)