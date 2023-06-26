readSegMask <- function(path = NULL, image = NULL) {
    # Input validation.
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
    
}
