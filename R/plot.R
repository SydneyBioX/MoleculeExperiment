#' @title Plotting functions for SpatialUtils
#'
#' @description  A set of ggplot functions to build customized plots for imaging
#'   based spatial transcriptomics data.
#'
#' @param me MoleculeExperiment object.
#' @param assayName Character string specifying name of assay from which to get
#' data.
#' @param byColour Character string specifying the column name to colour by.
#' @param byFill Character string specifying the column name to fill by.
#' @param path Path of the image. Default: NULL
#' @param image Image object to be plotted as raster. Default: NULL
#' @param displacement the x-y coordinate of the top-left pixel of the image. Default: c(0, 0)
#' @param pixelSize the pixel size in micron, Default: 1
#' @param ... Additional parameters to be passed to ggplot.
#'
#' @aliases
#' ggplot_me
#' geom_point_me
#' geom_polygon_me
#' 
#' @return A plot with transcripts and/or segmentation information for imaging
#' based spatial transcriptomics data.
#'
#' @name plotting-functions
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
#' me <- readXenium(repoDir,
#'                   keepCols = "essential",
#'                   addBoundaries = c("cell", "nucleus"))
#'
#' g = ggplot_me() +
#'         geom_polygon_me(me, byFill = "segment_id", colour = "black") +
#'         geom_point_me(me, byColour = "feature_id", size = 0.1) +
#'         geom_polygon_me(me, assayName = "nucleus", fill = NA, colour = "red")
#' g
NULL

#' @rdname plotting-functions
#' @export
ggplot_me <- function() {
    # base ggplot for me object
    ggplot2::ggplot() +
        ggplot2::facet_wrap(~sample_id) +
        ggplot2::theme_classic() +
        ggplot2::theme(aspect.ratio = 1) +
        ggplot2::theme(legend.position = "none")
}

#' @rdname plotting-functions
#' @export
#' @importFrom rlang .data
geom_point_me <- function(me, assayName = "detected", byColour = NULL, ...) {
    # creates ggplot layer for points
    if (is.null(byColour)) {
        gprot <- ggplot2::geom_point(
            ggplot2::aes(x = .data[["x_location"]], y = .data[["y_location"]]),
            data = molecules(me, assayName = assayName, flatten = TRUE), ...
        )
    } else {
        gprot <- ggplot2::geom_point(
            ggplot2::aes(
                x = .data[["x_location"]],
                y = .data[["y_location"]],
                colour = .data[[byColour]]
            ),
            data = molecules(me, assayName = assayName, flatten = TRUE), ...
        )
    }
    return(gprot)
}

#' @rdname plotting-functions
#' @export
#' @importFrom rlang .data
geom_polygon_me <- function(me, assayName = "cell", byFill = NULL, ...) {
    # creates ggplot layer for polygon
    if (is.null(byFill)) {
        gprot <- ggplot2::geom_polygon(
            ggplot2::aes(x = .data[["x_location"]], y = .data[["y_location"]],
                group = .data[["segment_id"]]),
            data = boundaries(me, assayName = assayName, flatten = TRUE), ...
        )
    } else {
        gprot <- ggplot2::geom_polygon(
            ggplot2::aes(
                x = .data[["x_location"]], y = .data[["y_location"]],
                group = .data[["segment_id"]], fill = .data[[byFill]]
            ),
            data = boundaries(me, assayName = assayName, flatten = TRUE), ...
        )
    }
    return(gprot)
}

#' @rdname plotting-functions
#' @export
#' @importFrom rlang .data
geom_raster_img <- function(path = NULL, image = NULL, displacement = c(0, 0), pixelSize = 1, ...) {
  # Neither path nor image was provided
  if (is.null(path) && is.null(image)) {
    cli::cli_abort(c(
      "No valid image was provided.",
      "i" = paste0(
        "Please provide either a path to a mask in TIF format or",
        " a loaded in-memory image."
      )
    ))
  # path was provided
  } else if (!is.null(path) && is.null(image)) {
    if (!file.exists(path) || dir.exists(path)) {
      cli::cli_abort(c(
        "Invalid image path.",
        "x" = "{path} does not exist or is a directory."
      ))
    }
    type <- tail(strsplit(path, ".", fixed = TRUE)[[1]], n = 1)
    if (type != "tif" & type != "tiff") {
      cli::cli_abort(c(
        "Unsupported image format.",
        "x" = "{type} files are not supported",
        "i" = "{.var path} must point to a TIF file."
      ))
    }
    # Read image by path
    image <- EBImage::readImage(path)
  # image was provided 
  } else if (is.null(path) && !is.null(image)) {
    if (!methods::is(image, "Image")) {
      cli::cli_abort(c(
        "x" = "{.var image} is not of type Image."
      ))
    }
  # Both path and image were provided
  } else {
    cli::cli_abort(c(
      "Both {.var path} and {.var image} were supplied. Choose one!"
    ))
  }
  
  # normalisation pixel intensity values between 0 and 1
  imageData(image) <- image / max(image)
  
  # Reshape image array to dataframe
  df <- as.data.frame(ftable(image)) %>% 
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Var"), as.numeric)
    )
  # Set the name of each column
  names(df) <- c("x", "y", "value")
  
  
  # Convert pixel to micron by pixel size (scale) and origin coordinate (translate)
  df <- df %>% mutate(
    x = (x - 1) * pixelSize + displacement[1],
    y = (y - 1) * pixelSize + displacement[2]
  )
  
  # gprot means ggproto object
  ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = value)) #+
    # default fill gradient is B+W
    #scale_fill_gradient(low = "black", high = "white")
  
  

}
