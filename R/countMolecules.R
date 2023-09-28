#' Count molecules per region of interest (e.g., cell)
#'
#' This function takes the information from the molecules and boundaries slot,
#' and counts the molecules per region of interest. Its input is a
#' MoleculeExperiment object, and its output a SpatialExperiment object.
#' That way, if one is interested in doing downstream analyses at the cell
#' level, one can do so.
#'
#' @param me MoleculeExperiment object containing both the transcript data
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
#' @param nCores Number of cores to use for the operation.
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
#' @importFrom BiocParallel bplapply SerialParam MulticoreParam
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom terra vect relate buffer
#' @importFrom Matrix sparseMatrix
#' @importFrom SpatialExperiment SpatialExperiment
#' @importFrom stats setNames
countMolecules <- function(me,
                             moleculesAssay = "detected",
                             boundariesAssay = "cell",
                             buffer = 0,
                             matrixOnly = FALSE,
                             nCores = 1) {
    # check arg validity
    .check_if_me(me)
    .stop_if_null(boundariesAssay, moleculesAssay)
    .check_if_character(boundariesAssay, moleculesAssay)
    if (!moleculesAssay %in% names(me@molecules)) {
            stop("Assay name specified does not exist in molecules slot.
Please specify another assay name in the assayName argument.")
        }
    if (!boundariesAssay %in% names(me@boundaries)) {
            stop("Assay name specified does not exist in boundaries slot.
Please specify another assay name in the assayName argument.")
        }

    init_mols <- MoleculeExperiment::molecules(me, moleculesAssay)
    init_bds <- MoleculeExperiment::boundaries(me, boundariesAssay)
    if (isFALSE(identical(
        names(init_mols[[moleculesAssay]]),
        names(init_bds[[boundariesAssay]])
    ))) {
        stop("Sample IDs do not match between the @molecules slot and the\n
                @boundaries slot.")
    }
    samples <- names(me@molecules[[moleculesAssay]])
    features <- sort(unique(unlist(
                    MoleculeExperiment::features(me, moleculesAssay))))


    bds_all <- init_bds[[boundariesAssay]]
    bds_all_flat <- suppressMessages(MoleculeExperiment::boundaries(me,
        assayName = boundariesAssay, flatten = TRUE
    ))

    valsList <- list()
    factors_levels <- list()
    centroids_list <- list()

    BPPARAM <- .generateBPParam(cores = nCores)

    for (sample in samples) {
        bds_df <- bds_all_flat %>% dplyr::filter(sample_id ==
            sample)

        sample_id_levels <- bds_df$sample_id[!duplicated(bds_df$sample_id)]
        segment_id_levels <- bds_df$segment_id[!duplicated(bds_df$segment_id)]
        factors <- interaction(
            factor(bds_df$sample_id, levels = sample_id_levels),
            factor(bds_df$segment_id, levels = segment_id_levels)
        )
        factors_levels[[sample]] <- levels(factors)

        factors_int <- as.integer(factors)
        bds_levels <- levels(factors)
        bds_mat <- cbind("factors_int" = factors_int, bds_df[, c(
            "x_location",
            "y_location"
        )])

        centroids <- bds_mat %>%
            dplyr::group_by(factors_int) %>%
            dplyr::summarise(
                x_location = mean(x_location),
                y_location = mean(y_location)
            ) %>%
            dplyr::mutate(sample_id = sample) %>%
            dplyr::mutate(cell_id = levels(factors)[factors_int])
        centroids_list[[sample]] <- centroids

        bds <- terra::vect(as.matrix(stats::setNames(
            bds_mat, c("factors_int", "x", "y")
        )), type = "polygons")
        bds <- terra::buffer(bds, width = buffer)

        names(features) <- unlist(features)
        result <- BiocParallel::bplapply(
            features, function(feature) {
                vals <- list()

                mols_df <- init_mols[[moleculesAssay]][[sample]][[feature]]
                if (is.null(mols_df)) {
                    return(NULL)
                }
                mols_mat <- as.matrix(mols_df[, c("x_location", "y_location")])
                colnames(mols_mat) <- c("x", "y")
                mols <- terra::vect(mols_mat, type = "points")
                out <- terra::relate(bds, mols, "covers", pairs = TRUE)
                xvals <- tapply(out[, 1], out[, 1], length)
                ivals <- rep(which(features == feature), length(xvals))
                jvals <- unique(out[, 1])
                jnames <- bds_levels[jvals]

                vals[["xvals"]] <- xvals
                vals[["ivals"]] <- ivals
                vals[["jnames"]] <- jnames
                vals
            },
            BPPARAM = BPPARAM
        )

        valsList[[sample]][["ivals"]] <- unlist(
            Map(
                function(x) {
                    as.numeric(x$ivals)
                },
                result
            )
        )
        valsList[[sample]][["xvals"]] <- unlist(
            Map(
                function(x) {
                    as.numeric(x$xvals)
                },
                result
            )
        )
        valsList[[sample]][["jnames"]] <- unlist(
            Map(
                function(x) {
                    x$jnames#[!is.null(x$jnames)]
                },
                result
            )
        )
    }

    xvals <- unlist(Map(function(x) {
        x$xvals
    }, valsList))
    ivals <- unlist(Map(function(x) {
        x$ivals
    }, valsList))
    jnames_all <- unlist(Map(function(x) {
        x$jnames
    }, valsList))
    jnames <- unlist(factors_levels)
    jvals <- match(jnames_all, jnames)
    inames <- sort(features)
    X <- Matrix::sparseMatrix(ivals, jvals, x = xvals, dims = c(length(inames),length(jnames)), dimnames = list(
        inames,
        jnames
    ))
    if (matrixOnly) {
        return(X)
    }

    centroids_all <- do.call(rbind, centroids_list)
    cData <- data.frame(
        sample_id = centroids_all[["sample_id"]],
        cell_id = centroids_all[["cell_id"]],
        x_location = centroids_all[["x_location"]],
        y_location = centroids_all[["y_location"]],
        row.names = centroids_all[["cell_id"]]
    )

    # they should be identical
    if (!identical(as.character(colnames(X)), as.character(rownames(cData)))) {
        cData <- cData[colnames(X), ]
    }

    spe <- SpatialExperiment::SpatialExperiment(
        assays = list(counts = X),
        colData = cData, spatialCoords = as.matrix(cData[, c(
            "x_location",
            "y_location"
        )]), reducedDims = list(spatial = as.matrix(cData[
            ,
            c("x_location", "y_location")
        ]))
    )
    return(spe)
}
