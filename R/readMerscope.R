#' Read in Merscope data to an ME object
#'
#' Reads in Merscope (Vizgen) molecule and boundary data from a directory,
#' and standardises it into a MoleculeExperiment object.
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
#'     keepCols = "essential",
#'     addBoundaries = "cell"
#' )
#' meMerscope
readMerscope <- function(dataDir,
                         keepCols = "essential",
                         addBoundaries = "cell") {
    # check arg validity
    .check_if_character(dataDir, keepCols)

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

        if (length(list.files(dataDir, pattern = pattern)) == 1) {
            nSamples <- 1
        } else {
            nSamples <- length(list.files(dataDir))
        }

        segDirs <- grep(
            "cell_boundaries",
            list.dirs(dataDir, full.names = TRUE, recursive = TRUE),
            value = TRUE
        )
        if (length(segDirs) == 0) {
            cli::cli_abort(c(
                "Didn't find any `cell_boundaries` directories!",
                "i" = paste0(
                    "Ensure all HDF5 files are in the `cell_boundaries` ",
                    "subdir for each sample."
                )
            ))
        } else if (length(segDirs) != nSamples) {
            cli::cli_warn(c(
                "Not all samples have boundaries.",
                "i" = paste0(
                    "At least one of the samples is missing a",
                    " `cell_boundaries` subdirectory."
                ),
                " " = "`cell_boundaries` should contain the sample's HDF5 files"
            ))
        }

        sampleNames <- .get_sample_id(nSamples, segDirs)
        df_all_list <- list()

        for (i in seq_along(segDirs)) {
            segFiles <- list.files(
                path = segDirs[i], pattern = "*.hdf5", full.names = TRUE,
            )

            if (length(segFiles) == 0) {
                cli::cli_abort(c(
                    "No boundary files found in sample: {sampleNames[i]}!",
                    "i" = paste0(
                        "Ensure your data includes boundary files or else",
                        " set {.var addBoundaries} to NULL."
                    )
                ))
            }
            m <- paste0("2/2 Loading boundaries for sample: ", sampleNames[i])
            for (j in cli::cli_progress_along(segFiles, name = m)) {
                out <- rhdf5::h5read(segFiles[j], "featuredata")
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
                            sample_id = sampleNames[i]
                        )
                    },
                    names(out), out,
                    SIMPLIFY = FALSE
                )
                df <- do.call(rbind, dfList)

                df_all_list[[segFiles[j]]] <- df
            }
        }
        df_all <- do.call(rbind, df_all_list)

        me@boundaries <- MoleculeExperiment::dataframeToMEList(df_all,
            dfType = "boundaries",
            assayName = "cell",
            sampleCol = "sample_id",
            factorCol = "cell_id",
            xCol = "x_location",
            yCol = "y_location"
        )
    }

    return(me)
}
