readSegMask <- function(
    extent, path = NULL, image = NULL, assayName = "cell",
    background_value = NULL, sample_id = NULL) {
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
        if (!file.exists(path) && dir.exists(path)) {
            cli::cli_abort(c(
                "Invalid mask path.",
                "x" = "{path} does not exist or is a directory."
            ))
        }
        type <- tail(strsplit(path, ".", fixed = TRUE)[[1]], n = 1)
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

    # TODO: How to figure out the extent of the image?
    ext(mask) <- e

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
            sample_id = ifelse(is.null(sample_id), "sample_1", sample_id)
        ) %>%
        dplyr::group_by(.data[["geom"]]) %>%
        dplyr::filter(
            dplyr::n_distinct(.data[["part"]]) < 1
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
