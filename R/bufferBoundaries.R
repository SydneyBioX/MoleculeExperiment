#' Create a new boundaries assay with buffers
#'
#' This function takes in an existing MoleculeExperiment object and generates
#' a new boundaries assay with added buffers. This can be useful for
#' visualisation and for countMolecules.
#'
#' @param me A MoleculeExperiment object.
#' @param assayName Character string (default "cell") specifying the existing
#' boundaries assay that should have buffer added.
#' @param ... Arguments that pass to internal functions. The most relevant
#' parameter is buffer (default 0).
#' @return A boundaries assay with essential columns and vertices with added
#' buffer.
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
#' me <- readXenium(repoDir,
#'   keepCols = "essential"
#' )
#' MoleculeExperiment::boundaries(me, "cell_buffer") <- bufferBoundaries(
#'   me,
#'   assayName = "cell", buffer = 1
#' )
#'
#' library(ggplot2)
#' ggplot_me() +
#'   geom_polygon_me(me, assayName = "cell", fill = "grey") +
#'   geom_polygon_me(me, assayName = "cell_buffer", fill = NA, colour = "red") +
#'   geom_point_me(me) +
#'   coord_cartesian(
#'     xlim = c(4900, 4919.98),
#'     ylim = c(6400.02, 6420)
#'   )
bufferBoundaries <- function(me, assayName = "cell", ...) {
  bds_all <- MoleculeExperiment::boundaries(me, assayName = assayName)
  bds_all <- lapply(bds_all, function(x1) lapply(x1, function(x2) lapply(x2, function(x3) as.matrix(x3))))

  bds_buffer_all <- rapply(bds_all,
    .add_buffer_boundary,
    classes = "matrix",
    how = "list",
    ...
  )

  return(bds_buffer_all)
}
