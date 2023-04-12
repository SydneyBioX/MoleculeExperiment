#' Count molecules per region of interest (e.g., cell)
#'
#' This function takes the information from the molecules and boundaries slot,
#' and counts the molecules per region of interest. Its input is a
#' MoleculeExperiment object, and its output a SpatialExperiment object.
#' That way, if one is interested in doing downstream analyses at the cell
#' level, one can do so.
#'
#' @param object MoleculeExperiment object containing both the transcript data as
#' well as the boundaries data. I.e., the "molecules" and "boundaries" slots
#' need to be filled. See MoleculeExperiment() for more information.
#' @param boundariesAssay Character string naming the list of the boundaries
#' slot from which boundary information should be retrieved from.
#' For example, for counting transcripts per cell, the list containing the cell
#' boundaries (e.g., "cell") should be selected.
#' @param segmentationInfo Character string specifying the type of segmentation
#' information available. Can be either "boundaries" or "masks". Currently,
#' only the "boundaries" information is supported.
#' @param moleculesAssay Character string naming the list of the molecules slot
#' from which transcript information should be retrieved from.
#' The default is the detected transcript data that is read in when creating a
#' MoleculeExperiment object. It is possible to change it to another mode, e.g.,
#' "high_threshold" will access the transcript information that has been stored
#' in the "high_threshold" element of the list in the molecules slot.
#'
#' @return A SpatialExperiment object derived from a MoleculeExperiment object.
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#'
#' me <- readXenium(repoDir,
#'                   keepCols = "essential")
#'
#' spe <- countMolecules(me)
#' spe
countMolecules <- function(object,
                           boundariesAssay = "cell",
                           segmentationInfo = "boundaries",
                           moleculesAssay = "detected") {
    # check arg validity
    .check_if_me(object)
    .stop_if_null(boundariesAssay, segmentationInfo, moleculesAssay)
    .check_if_character(boundariesAssay, segmentationInfo, moleculesAssay)

    if (boundariesAssay == "cell") {
        message("The boundaries were retrieved from the \"cell\" assay. A
different assay (e.g., \"nucleus\") can be specified in the boundariesAssay
argument)")
    }
    # Function should be flexible to different segmentation information
    # priority for boundaries as 10x and vizgen have this info, but not masks
    if (segmentationInfo == "boundaries") {
        spe <- .count_molecules_boundaries(object,
                                        molecules_assay = moleculesAssay,
                                        boundaries_assay = boundariesAssay)
    }
    # if (is(segmentationInfo, "masks")) {
    #    return(.count_molecules_masks(object, segmentation))
    # }
    return(spe)
}

.count_molecules_boundaries <- function(object,
                                      molecules_assay = NULL,
                                      boundaries_assay = NULL) {
    # check matching of sample ids
    if (isFALSE(
        identical(
            names(MoleculeExperiment::molecules(object, molecules_assay)[[molecules_assay]]),
            names(boundaries(object, boundaries_assay)[[boundaries_assay]])
        )
    )) {
        stop("Sample IDs to do not match between the @molecules slot and the
            @boundaries slot.")
    }

    # create SpatialPolygon object for each sample (from sp package)
    srList <- lapply(
        boundaries(object, boundaries_assay)[[boundaries_assay]],
        function(bds) {
            sp::SpatialPolygons(mapply(
                # create Polygons obj from Polygon objects for each segment_id
                function(x, y, nm) {
                    sp::Polygons(list(sp::Polygon(cbind(x, y))), nm)
                },
                x = lapply(bds, "[", "x_location"),
                y = lapply(bds, "[", "y_location"),
                # nm corresponds to segment_id (e.g., cell_id)
                nm = as.character(names(bds))
            ))
        }
    )

    readsList <- lapply(
        MoleculeExperiment::molecules(object, molecules_assay)[[molecules_assay]],
        function(reads) {
            reads <- lapply(reads, function(rds) {
                sp::coordinates(rds) <- ~ x_location + y_location
                return(rds)
            })
            return(reads)
        }
    )

    getOutGenes <- function(sr, reads) {
        lapply(reads, function(rds) {
            out <- sp::over(sr, rds, returnList = TRUE)
            return(unlist(lapply(out, length)))
        })
    }

    out <- mapply(getOutGenes, srList, readsList, SIMPLIFY = FALSE)

    all_x <- unlist(out)
    all_i_names <- rep(
        unlist(sapply(
            seq_len(length(out)),
            function(i) names(out[[i]])
        )),
        times = lapply(unlist(out, recursive = FALSE), length)
    )
    all_i <- as.integer(factor(all_i_names))
    all_j_names <- names(
        unlist(lapply(out, function(x) stats::setNames(x, NULL)))
    )
    all_j <- as.integer(factor(all_j_names))
    i_names <- unique(all_i_names)
    j_names <- unique(all_j_names)

    X <- Matrix::sparseMatrix(all_i, all_j,
        x = all_x, dimnames = list(i_names, j_names)
    )

    X

    sample_id <- rep(names(boundaries(object, boundaries_assay)[[boundaries_assay]]),
        times = lapply(boundaries(object, boundaries_assay)[[boundaries_assay]], length)
    )

    centroids <- do.call(rbind, lapply(unlist(boundaries(object, boundaries_assay)[[boundaries_assay]],
        recursive = FALSE
    ), colMeans))

    cData <- data.frame(
        sample_id = sample_id,
        x_location = centroids[, "x_location"],
        y_location = centroids[, "y_location"],
        cell_id = colnames(X)
    )

    # important--> ONLY LOAD spatialexperiment construction function
    spe <- SpatialExperiment::SpatialExperiment(
        assays = list(counts = X),
        colData = cData,
        spatialCoords = as.matrix(cData[, c("x_location", "y_location")]),
        reducedDims = list(spatial = as.matrix(cData[, c("x_location", "y_location")]))
    )
    return(spe)
}
# .count_molecules_masks(){
#    # should recognise an array
# }
#