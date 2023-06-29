#' Count molecules per region of interest (e.g., cell)
#'
#' This function takes the information from the molecules and boundaries slot,
#' and counts the molecules per region of interest. Its input is a
#' MoleculeExperiment object, and its output a SpatialExperiment object.
#' That way, if one is interested in doing downstream analyses at the cell
#' level, one can do so.
#'
#' @param object MoleculeExperiment object containing both the transcript data
#' as well as the boundaries data. I.e., the "molecules" and "boundaries" slots
#' need to be filled. See MoleculeExperiment() for more information.
#' @param boundariesAssay Character string naming the list of the boundaries
#' slot from which boundary information should be retrieved from.
#' For example, for counting transcripts per cell, the list containing the cell
#' boundaries (e.g., "cell") should be selected.
#' @param moleculesAssay Character string naming the list of the molecules slot
#' from which transcript information should be retrieved from.
#' The default is the detected transcript data that is read in when creating a
#' MoleculeExperiment object. It is possible to change it to another mode, e.g.,
#' "high_threshold" will access the transcript information that has been stored
#' in the "high_threshold" element of the list in the molecules slot.
#' @param buffer Single numeric value (default 0) indicating value to buffer
#' beyond segment boundaries, i.e. to count molecules just outside of a segment
#' boundary
#' @param matrixOnly Logical value indicating whether to return a matrix of the
#' counted molecules per segment (e.g., cell). Is FALSE by default, i.e., the
#' default output is a SpatialExperiment object.
#'
#' @return A SpatialExperiment object derived from a MoleculeExperiment object.
#' Alternatively, a matrix with the counted molecules per segment.
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
#' me <- readXenium(repoDir,
#'     keepCols = "essential"
#' )
#'
#' spe <- countMolecules(me)
#' spe
countMolecules <- function(object,
                           moleculesAssay = "detected",
                           boundariesAssay = "cell",
                           buffer = 0,
                           matrixOnly = FALSE) {
    # check arg validity
    .check_if_me(object)
    .stop_if_null(boundariesAssay, moleculesAssay)
    .check_if_character(boundariesAssay, moleculesAssay)

    init_mols <- MoleculeExperiment::molecules(object, molecules_assay)
    init_bds <- MoleculeExperiment::boundaries(object, boundaries_assay)

    if (isFALSE(identical(
        names(init_mols[[molecules_assay]]),
        names(init_bds[[boundaries_assay]])
    ))) {
        stop("Sample IDs do not match between the @molecules slot and the\n
                @boundaries slot.")
    }

    samples <- names(object@molecules$detected)
    features <- sort(unique(unlist(MoleculeExperiment::features(object))))

    xvalsList <- vector(mode = "list", length = length(samples))
    xvalsList <- lapply(
        xvalsList,
        function(x) vector(mode = "list", length = length(features))
    )

    ivalsList <- vector(mode = "list", length = length(samples))
    ivalsList <- lapply(
        ivalsList,
        function(x) vector(mode = "list", length = length(features))
    )

    jnamesList <- vector(mode = "list", length = length(samples))
    jnamesList <- lapply(
        jnamesList,
        function(x) vector(mode = "list", length = length(features))
    )

    bds_all <- init_bds[[boundaries_assay]]

    # suppress messages to avoid repeating getter messages
    bds_all_flat <- suppressMessages(
        MoleculeExperiment::boundaries(object,
            assayName = boundaries_assay,
            flatten = TRUE
        )
    )

    for (sample in samples) {
        bds_df <- bds_all_flat %>% dplyr::filter(sample_id == sample)
        factors <- interaction(bds_df$sample_id, bds_df$segment_id)
        factors_int <- as.integer(factors)
        bds_levels <- levels(factors)
        bds_mat <- as.matrix(cbind(
            factors_int,
            bds_df[, c("x_location", "y_location")]
        ))
        colnames(bds_mat) <- c("factors_int", "x", "y")
        bds <- terra::vect(bds_mat, type = "polygons")
        bds <- terra::buffer(bds, width = buffer)

        for (feature in features) {
            mols_df <- init_mols[[molecules_assay]][[sample]][[feature]]

            if (is.null(mols_df)) next

            mols_mat <- as.matrix(mols_df[, c("x_location", "y_location")])
            colnames(mols_mat) <- c("x", "y")

            mols <- terra::vect(mols_mat, type = "points")

            out <- terra::relate(bds, mols, "covers", pairs = TRUE)
            xvals <- tapply(out[, 1], out[, 1], length)
            ivals <- rep(which(features == feature), length(xvals))
            jvals <- unique(out[, 1])
            jnames <- bds_levels[jvals]

            xvalsList[[sample]][[feature]] <- xvals
            ivalsList[[sample]][[feature]] <- ivals
            jnamesList[[sample]][[feature]] <- jnames
        }
    }

    xvals <- unlist(xvalsList)
    ivals <- unlist(ivalsList)
    jnames_all <- unlist(jnamesList)
    jnames <- sort(unique(jnames_all))
    jvals <- match(jnames_all, jnames)
    inames <- sort(features)

    X <- Matrix::sparseMatrix(ivals,
        jvals,
        x = xvals,
        dimnames = list(inames, jnames)
    )

    if (matrix_only) {
        return(X)
    }

    sample_id <- rep(names(bds_all), times = lapply(bds_all, length))
    centroids <- do.call(rbind, lapply(
        unlist(bds_all, recursive = FALSE),
        colMeans
    ))
    cData <- data.frame(
        sample_id = sample_id,
        x_location = centroids[, "x_location"],
        y_location = centroids[, "y_location"]
    )[colnames(X), ]
    cData[, "cell_id"] <- colnames(X)

    spe <- SpatialExperiment::SpatialExperiment(
        assays = list(counts = X),
        colData = cData,
        spatialCoords = as.matrix(cData[, c("x_location", "y_location")]),
        reducedDims = list(
            spatial = as.matrix(cData[, c("x_location", "y_location")])
        )
    )

    return(spe)
}
