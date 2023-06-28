#' Read in Merscope data to an ME object
#'
#' Reads in Merscope data (Vizgen) from a directory, and standardises it into
#' a MoleculeExperiment object. It is essentially a wrapper around the function
#' readMolecules(). Note that this function can currently only create a simple
#' ME object with the molecules slot filled. Boundary information cannot be
#' handled yet.
#'
#' @param dataDir Character string specifying the directory with the Cosmx
#' output files.
#' @param keepCols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @param addBoundaries A string with which to specify the name of the boundary
#'     assay to be added to the me object. Can be a string, or NULL.
#'     If NULL, a simple ME object with no boundaries will be created.
#' @return A MoleculeExperiment object
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/vizgen_HumanOvarianCancerPatient2Slice2")
#' meMerscope <- readMerscope(repoDir,
#'     keepCols = "essential"
#' )
#' meMerscope
readMerscope <- function(dataDir,
                         keepCols = "essential",
                         addBoundaries = "cell") {
    # check arg validity
    .check_if_character(dataDir, keepCols)

    # create simple MoleculeExperiment object
    pattern <- "detected_transcripts.csv"

    if (!is.null(addBoundaries)) {
        cli::cli_progress_step(
            "1/2 Reading transcripts",
            .auto_close = FALSE,
            spinner = TRUE
        )
    }
    me <- readMolecules(
        dataDir = dataDir,
        pattern = pattern,
        featureCol = "gene",
        xCol = "global_x",
        yCol = "global_y",
        keepCols = keepCols
    )
    if (!is.null(addBoundaries)) cli::cli_progress_done()


    if (!is.null(addBoundaries)) {
        cli::cli_progress_step(
            "2/2 Reading boundaries",
            .auto_close = FALSE,
            spinner = TRUE
        )

        segFiles <- list.files(
            path = dataDir, pattern = "*.hdf5", full.names = TRUE
        )

        df_all_list <- list()

        for (segFile in segFiles) {
            out <- rhdf5::h5read(segFile, "featuredata")
            out <- lapply(out, "[[", "zIndex_0")
            out <- lapply(out, "[[", "p_0")
            out <- lapply(out, "[[", "coordinates")
            out <- lapply(out, "[", , , 1)
            out <- lapply(out, t)
            dfList <- mapply(
                function(nms, locations) {
                    data.frame(
                        x_location = locations[, 1],
                        y_location = locations[, 2],
                        cell_id = nms,
                        sample_id = sampleName
                    )
                },
                names(out), out,
                SIMPLIFY = FALSE
            )
            df <- do.call(rbind, dfList)

            df_all_list[[segFile]] <- df
        }

        df_all <- do.call(rbind, df_all_list)
        cli::cli_progress_done()

        me@boundaries <- MoleculeExperiment::dataframeToMEList(df_all,
            dfType = "boundaries",
            assayName = "cells",
            sampleCol = "sample_id",
            factorCol = "cell_id",
            xCol = "x_location",
            yCol = "y_location"
        )
    }
    # future development: handle complex hdf5 segmentation files

    return(me)
}
