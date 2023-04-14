ggplot_me <- function() {
    # base ggplot for me object
    ggplot() +
        facet_me() +
        theme_classic() +
        theme(aspect.ratio = 1) +
        theme(legend.position = "none")
}

geom_point_me <- function(me, assayName = "detected", by_colour = NULL, ...) {
    # creates ggplot layer for points
    if (is.null(by_colour)) {
        gprot <- geom_point(aes(x = x_location, y = y_location),
            data = molecules(me, assayName = assayName, flatten = TRUE), ...
        )
    } else {
        gprot <- geom_point(
            aes(x = x_location, y = y_location, colour = .data[[by_colour]]),
            data = molecules(me, assayName = assayName, flatten = TRUE), ...
        )
    }
    return(gprot)
}

geom_polygon_me <- function(me, assayName = "cell", by_fill = NULL, ...) {
    # creates ggplot layer for polygon
    if (is.null(by_fill)) {
        gprot <- geom_polygon(
            aes(x = x_location, y = y_location, group = segment_id),
            data = boundaries(me, assayName = assayName, flatten = TRUE), ...
        )
    } else {
        gprot <- geom_polygon(
            aes(
                x = x_location, y = y_location,
                group = segment_id, fill = .data[[by_fill]]
            ),
            data = boundaries(me, assayName = assayName, flatten = TRUE), ...
        )
    }
    return(gprot)
}

facet_me <- function(me) {
    # looks at number of samples in me and facets according to it
    facet_wrap(~sample_id)
}


#' @title plotBoudaries
#' @description Plots the molucules and boundaries of a ME object.
#' @param me A molecule experiment with boundaries.
#' @return A boundary plot.
#' @examples
#' repo_dir <- system.file("extdata", package = "MoleculeExperiment")
#'
#' me <- readXenium(
#'     repo_dir,
#'     keepCols = "essential"
#' )
#' me
#'
#' nuclei_ls <- readBoundaries(
#'     repo_dir,
#'     pattern = "nucleus_boundaries.csv",
#'     segmentIDCol = "cell_id",
#'     xCol = "vertex_x",
#'     yCol = "vertex_y",
#'     keepCols = "essential",
#'     boundariesAssay = "nucleus",
#'     scaleFactorVector = 1
#' )
#'
#' boundaries(me, "nucleus") <- nuclei_ls
#'
#' plotBoudaries(me)
#'
#' @rdname plotBoudaries
#' @export
plotBoudaries <- function(me) {
    g <- ggplot_me() +
        geom_polygon_me(
            me,
            by_fill = "segment_id", colour = "black"
        ) +
        geom_point_me(
            me,
            by_colour = "feature_name", size = 0.1
        ) +
        geom_polygon_me(
            me,
            assayName = "nucleus", fill = NA, colour = "red"
        )
    g
}
