#' @title Read a segmentation mask
#' @description Reads a segmentation mask TIFF and transforms it into a
#'     ME boundaries object. One must provide either the path or the loaded
#'     image object.
#' @param extent The extent of the loaded segmentation mask in micrometers.
#'     Used to align the mask with the transcripts. This must be of the form
#'     c(xmin, xmax, ymin, ymax).
#' @param path The path of the segmenation mask, Default: NULL
#' @param image The loaded image object, Default: NULL
#' @param assayName The name of the segmentation (e.g. cell, or nucleus),
#'     Default: 'cell'
#' @param background_value The value corrisponding to the backgorund in the
#'     segmentation, Default: NULL
#' @param sample_id What the sample should be named, Default: NULL
#' @return A boundaries object.
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' segMask <- paste0(repoDir, "/BIDcell_segmask.tif")
#' data <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain/sample1")
#' me <- readXenium(data,
#'     keepCols = "essential",
#'     addBoundaries = NULL
#' )
#' boundaries(me, "BIDcell_segmentation") <- readSegMask(
#'     extent(me), # use the molecule extent to define the boundary extent
#'     path = segMask, assayName = "BIDcell_segmentation",
#'     sample_id = "sample1", background_value = 0
#' )
#' ggplot_me() +
#'     geom_polygon_me(
#'         me,
#'         assayName = "BIDcell_segmentation", fill = NA, colour = "black"
#'     ) +
#'     geom_point_me(me, byColour = "feature_name", size = 0.1) +
#'     geom_polygon_me(
#'         me,
#'         assayName = "BIDcell_segmentation", fill = NA, colour = "red"
#'     )
#' @rdname readSegMask
#' @export
#' @importFrom terra ext rast geom as.polygons
#' @importFrom cli cli_abort
#' @importFrom methods is
#' @importFrom EBImage imageData
#' @importFrom dplyr filter mutate group_by n_distinct ungroup consecutive_id
readSegMask <- function(
    extent, path = NULL, image = NULL, assayName = "cell",
    background_value = NULL, sample_id = NULL) {
    # TODO: Deal with the sample id name situation.

    # Input validation.
    e <- tryCatch(
        terra::ext(extent),
        error = function(err) {
            cli::cli_abort(c(
                "Invalid extent.",
                "i" = paste0(
                    "{.var extent} must be of the form",
                    " c(xmin, xmax, ymin, ymax)."
                ),
                "x" = "From `terra::ext`:",
                " " = "{err$message}"
            ))
        }
    )

    if (is.null(path) && is.null(image)) {
        cli::cli_abort(c(
            "No valid mask was provided.",
            "i" = paste0(
                "Please provide either a path to a mask in TIF format or",
                " a loaded in-memory image."
            )
        ))
    } else if (!is.null(path) && is.null(image)) {
        if (!file.exists(path) || dir.exists(path)) {
            cli::cli_abort(c(
                "Invalid mask path.",
                "x" = "{path} does not exist or is a directory."
            ))
        }
        type <- tail(strsplit(path, ".", fixed = TRUE)[[1]], n = 1)
        # TODO: include tiff
        if (type != "tif") {
            cli::cli_abort(c(
                "Unsupported segmentation mask.",
                "x" = "{type} files are not supported",
                "i" = "{.var path} must point to a TIF file."
            ))
        }
        mask <- terra::rast(path)
    } else if (is.null(path) && !is.null(image)) {
        if (!methods::is(image, "Image")) {
            cli::cli_abort(c(
                "{.var mask} is not of type Image.",
                "i" = ""
            ))
        }
        # need to transpose imageData for rast
        # ensure this is correct!
        mask <- terra::rast(t(EBImage::imageData(image)))
    } else {
        cli::cli_abort(c(
            "Both {.var path} and {.var image} were supplied. Choose one!"
        ))
    }

    terra::ext(mask) <- e

    geom_df <- as.data.frame(terra::geom(terra::as.polygons(mask)))

    if (!is.null(background_value)) {
        if (is.nan(background_value)) {
            cli::cli_abort(c(
                "Invalid background value:",
                " " = "{.var background_value} is {background_value}.",
                "i" = "Background value should be an integer."
            ))
        }
        geom_df %<>%
            dplyr::filter(
                !.data[["geom"]] == background_value
            )
    }
    geom_df %<>%
        dplyr::mutate(
            sample_id = ifelse(is.null(sample_id), "sample1", sample_id)
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
            )
        )

    dataframeToMEList(
        geom_df,
        dfType = "boundaries", assayName = assayName,
        sampleCol = "sample_id", factorCol = "cell_id", xCol = "x",
        yCol = "y"
    )
}
