# =============================================================================
# read in boundary information: readBoundaries()
# =============================================================================

#' Read segmentation boundary files into a MoleculeExperiment object.
#'
#' This function reads in boundary files of interest, standardises the data
#' format, and adds the data to the "boundaries" slot of a specified
#' MoleculeExperiment object. NOTE: currently, this function works with csv
#' segmentation files with the format given by the segmentation method of
#' 10X genomics. I.e., a csv file with the following columns and column order:
#' "cell_id", "vertex_x" and "vertex_y".
#'
#' @param me A MoleculeExperiment object. Note: a MoleculeExperiment object can
#' only be created with a filled molecules slot.
#' @param boundaries_mode Character string naming the list of the boundaries
#' slot that will contain the imported boundary information. This is important
#' to recognise the imported boundaries later on. For example, one might want
#' to store cell boundaries in a list entry named "cells", and the nuclei
#' boundaries in a list entry named "nuclei". These different entries can be
#' accessed with the boundaries() accessor (see boundaries() for more
#' information).
#' @param data_dir Character string indicating path to the parent directory
#' containing the boundary files for the sample/s.
#' @param pattern Character string specifying a pattern unique to the name of
#' the csv boundary file/s of interest. E.g., "cell_boundaries.csv"
#' @param n_samples Integer indicating the number of samples from which to read
#' boundary files.

readBoundaries <- function(me,
                                boundaries_mode = NULL,
                                data_dir,
                                pattern = NULL,
                                n_samples = NULL) {
        # TODO maybe
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
        me@boundaries <- bds_ls
        # check that modification is valid
        validObject(me)

        # guide user
        cat("Boundary information can be accessed with boundaries(me).")
}