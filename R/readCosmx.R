#' Read in Cosmx data (Nanostring) as an ME object.
#'
#' This function is a wrapper around the readMolecules function. It can
#' read both molecule and mask information. The segmentation masks are
#' converted to boundaries, and these are added to the boundaries slot
#' of the MoleculeExperiment object.
#'
#' @param dataDir Character string specifying the directory with the Cosmx
#'     output files.
#' @param keepCols Character string specifying which columns to keep.
#'     Defaults to "essential". The other option is to select "all", or custom
#'     columns by specifying their names in a vector.
#' @param addBoundaries A string with which to specify the name of the boundary
#'     assay to be added to the me object. Can be a string, or NULL.
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
#' @importFrom cli cli_progress_step cli_progress_done
#' @importFrom data.table fread
#' @importFrom terra rast ext as.polygons vect geom
#' @importFrom dplyr bind_rows filter group_by n_distinct ungroup
#'     mutate consecutive_id
#' @importFrom utils tail
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data
readCosmx <- function(dataDir,
                      keepCols = "essential",
                      addBoundaries = "cell") {
    # check arg validity
    .check_if_character(dataDir, keepCols)

    # create MoleculeExperiment object
    pattern <- "tx_file"
    # according to README file from cosmx, 1 pixel = 0.18 µm
    scaleFactor <- 0.18
    if (!is.null(addBoundaries)) {
        cli::cli_progress_step(
            "1/3 Reading transcripts",
            .auto_close = FALSE,
            spinner = TRUE
        )
    }
    me <- readMolecules(
        dataDir = dataDir,
        pattern = pattern,
        featureCol = "target",
        xCol = "x_global_px",
        yCol = "y_global_px",
        keepCols = keepCols,
        scaleFactorVector = scaleFactor
    )
    if (!is.null(addBoundaries)) cli::cli_progress_done()

    if (!is.null(addBoundaries)) {
        cell_mask_dirs <- grep(
            "CellLabels",
            list.dirs(dataDir, full.names = TRUE, recursive = TRUE),
            value = TRUE
        )

        topology_files <- list.files(
            dataDir,
            "fov_positions_file.csv",
            recursive = TRUE
        )

        topology <- lapply(
            topology_files,
            function(f) data.table::fread(paste(dataDir, f, sep = "/"))
        )
        mask_names <- lapply(
            cell_mask_dirs,
            function(dir) {
                list.files(
                    dir,
                    pattern = "*.tif", full.names = TRUE
                )
            }
        )

        check <- unlist(lapply(
            seq_along(topology),
            function(i) {
                nrow(topology[[i]]) == length(mask_names[[i]])
            }
        ))
        # check if right number of images
        if (!all(check)) {
            bad_dirs <- paste(
                list.dirs(
                    dataDir,
                    recursive = FALSE, full.names = TRUE
                )[which(check == FALSE)]
            )
            cli::cli_abort(c(
                paste0(
                    "fov_positions CSV and CellLabels folder have a different ",
                    "number of images."
                ),
                "x" = paste0(
                    "{sum(check == FALSE)}/{length(check)} bad sample{?s}: ",
                    "{bad_dirs}"
                ),
                "i" = "Check if you have valid CosMX data."
            ))
        }

        cli::cli_progress_step(
            "2/3 Transforming masks into polygons",
            .auto_close = FALSE,
            spinner = TRUE
        )
        # convert each image to polygons
        poly_list <- lapply(
            seq_along(mask_names),
            function(i) {
                lapply(seq_along(mask_names[[i]]), function(j) {
                    mask <- terra::rast(mask_names[[i]][[j]])

                    xmin <- topology[[i]][j, 2][[1]]
                    xmax <- topology[[i]][j, 2][[1]] + ncol(mask)

                    ymin <- topology[[i]][j, 3][[1]]
                    ymax <- topology[[i]][j, 3][[1]] + nrow(mask)

                    terra::ext(mask) <- c(xmin, xmax, ymin, ymax)
                    poly <- terra::as.polygons(mask)
                })
            }
        )
        cli::cli_progress_done()

        cli::cli_progress_step(
            "3/3 Merging patches",
            .auto_close = FALSE,
            spinner = TRUE
        )
        merged_vectors_list <- list()
        for (i in seq_along(poly_list)) {
            merged_vector <- terra::vect()
            for (j in seq_along(poly_list[[i]])) {
                merged_vector <- rbind(merged_vector, poly_list[[i]][[j]])
            }
            merged_vectors_list[[paste0("sample_", i)]] <- as.data.frame(
                terra::geom(merged_vector)
            )
        }
        merged_vectors_df <- dplyr::bind_rows(
            merged_vectors_list,
            .id = "sample_id"
        ) %>%
            dplyr::filter(
                !.data[["geom"]] == 1
            ) %>%
            dplyr::group_by(.data[["geom"]]) %>%
            dplyr::filter(
                dplyr::n_distinct(.data[["part"]]) < 2
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                # create ID col
                cell_id = dplyr::consecutive_id(
                    .data[["sample_id"]], .data[["geom"]]
                ),
                # scale x and y to microns
                x = scaleFactor * .data[["x"]],
                y = scaleFactor * .data[["y"]]
            )
        if (length(cell_mask_dirs) == 1) {
            merged_vectors_df %<>% dplyr::mutate(
                sample_id = .get_sample_id(1, cell_mask_dirs)
            )
        }
        me@boundaries <- dataframeToMEList(
            merged_vectors_df,
            dfType = "boundaries", assayName = addBoundaries,
            sampleCol = "sample_id", factorCol = "cell_id", xCol = "x",
            yCol = "y"
        )
        cli::cli_progress_done()
    }

    return(me)
}
