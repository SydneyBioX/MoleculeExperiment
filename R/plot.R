library(MoleculeExperiment)
library(ggplot2)

repo_dir <- system.file("extdata", package = "MoleculeExperiment")

me <- readXenium(repo_dir,
    n_samples = 2,
    keep_cols = "essential"
)
me

nuclei_ls <- readBoundaries(
    data_dir = repo_dir,
    pattern = "nucleus_boundaries.csv",
    n_samples = 2,
    segment_id_col = "cell_id",
    x_col = "vertex_x",
    y_col = "vertex_y",
    keep_cols = "essential",
    boundaries_assay = "nucleus",
    scale_factor_vector = 1
)

boundaries(me, "nucleus") <- nuclei_ls
me # note the addition of the nucleus boundaries to the boundaries slot

ggplot_me <- function() {
    # base ggplot for me object
    ggplot() +
        facet_me() +
        theme_classic() +
        theme(aspect.ratio = 1) +
        theme(legend.position = "none")
}

geom_point_me <- function(me, assay_name = "detected", by_colour = NULL, ...) {
    # creates ggplot layer for points
    if (is.null(by_colour)) {
        gprot <- geom_point(aes(x = x_location, y = y_location),
            data = molecules(me, assay_name = assay_name, flatten = TRUE), ...
        )
    } else {
        gprot <- geom_point(aes(x = x_location, y = y_location, colour = .data[[by_colour]]),
            data = molecules(me, assay_name = assay_name, flatten = TRUE), ...
        )
    }
    return(gprot)
}

geom_polygon_me <- function(me, assay_name = "cell", by_fill = NULL, ...) {
    # creates ggplot layer for polygon
    if (is.null(by_fill)) {
        gprot <- geom_polygon(aes(x = x_location, y = y_location, group = segment_id),
            data = boundaries(me, assay_name = assay_name, flatten = TRUE), ...
        )
    } else {
        gprot <- geom_polygon(aes(x = x_location, y = y_location, group = segment_id, fill = .data[[by_fill]]),
            data = boundaries(me, assay_name = assay_name, flatten = TRUE), ...
        )
    }
    return(gprot)
}

facet_me <- function(me) {
    # looks at number of samples in me and facets according to it
    facet_wrap(~sample_id)
}


g <- ggplot_me() +
    geom_polygon_me(me, by_fill = "segment_id", colour = "black") + # add molecule points
    geom_point_me(me, by_colour = "feature_name", size = 0.1) + # add cell segmentds
    geom_polygon_me(me, assay_name = "nucleus", fill = NA, colour = "red") # add nuclei segments
g
