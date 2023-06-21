# ==============================================================================
# future development: a wrapper for cosmx data
# ==============================================================================

#' Read in Cosmx data (Nanostring) as an ME object.
#'
#' This function is a wrapper around the readMolecules function. Note that it
#' can currently only create a simple ME object with the molecules slot filled.
#' Boundary information is not handled yet.
#'
#' @param dataDir Character string specifying the directory with the Cosmx
#'     output files.
#' @param keepCols Character string specifying which columns to keep.
#'     Defaults to "essential". The other option is to select "all", or custom
#'     columns by specifying their names in a vector.
#' @param addBoundaries Vector with which to specify the names of the boundary
#'     assays to be added to the me object. Can be a string, or NULL.
#'     If NULL, a simple ME object with no boundaries will be created.
#' @return A MoleculeExperiment object
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/nanostring_Lung9_Rep1")
#' #
#' meCosmx <- readCosmx(repoDir,
#'     keepCols = "essential"
#' )
#' meCosmx
readCosmx <- function(dataDir,
                      keepCols = "essential",
                      addBoundaries = NULL) {
    # check arg validity
    .check_if_character(dataDir, keepCols)

    # create MoleculeExperiment object
    pattern <- "tx_file"
    # according to README file from cosmx, 1 pixel = 0.18 µm
    scaleFactor <- 0.18
    me <- readMolecules(
        dataDir = dataDir,
        pattern = pattern,
        featureCol = "target",
        xCol = "x_global_px",
        yCol = "y_global_px",
        keepCols = keepCols,
        scaleFactorVector = scaleFactor
    )

    #TODO: Find and account for a multisample dataset.
    if (!is.null(addBoundaries)) {
        cell_mask_dir <- paste(data_dir, "CellLabels", sep = "/")
        file <- "Lung9_Rep1_fov_positions_file.csv"

        topology <- data.table::fread(paste(data_dir, file, sep = "/"))
        mask_names <- list.files(
            cell_mask_dir,
            pattern = "*.tif", full.names = TRUE
        )

        # check if right number of images
        if (!nrow(topology) == length(mask_names)) {
            stop(
                "fov_positions and CellLabels have a different number of",
                " images.\n\tCheck if you have valid CosMX data."
            )
        }

        # convert each image to polygons
        poly_list <- lapply(
            cli::cli_progress_along(
                mask_names,
                name = "1/2 Transforming masks into polygons:"
            ),
            function(i) {
                mask <- terra::rast(mask_names[[i]])

                xmin <- topology[i, 2][[1]]
                xmax <- topology[i, 2][[1]] + ncol(mask)

                ymin <- topology[i, 3][[1]] - nrow(mask)
                ymax <- topology[i, 3][[1]]

                terra::ext(mask) <- c(xmin, xmax, ymin, ymax)
                poly <- terra::as.polygons(mask, round = FALSE)
            }
        )

        merged_vector <- terra::vect()
        ext(merged_vector) <- ext(poly_list[[1]])

        for (i in cli::cli_progress_along(
            poly_list,
            name = "2/2 Joining patches:"
        )) {
            merged_vector <- rbind(merged_vector, poly_list[[i]])
        }
    }

    return(me)
}
