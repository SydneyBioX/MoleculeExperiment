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