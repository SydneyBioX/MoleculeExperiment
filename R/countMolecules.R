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
#' @param matrixOnly Logical value indicating whether to return a matrix of the
#' counted molecules per segment (e.g., cell). Is FALSE by default, i.e., the
#' default output is a SpatialExperiment object.
#'
#' @return A SpatialExperiment object derived from a MoleculeExperiment object.
#' Alternatively, a matrix with the counted molecules per segment.
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
                           moleculesAssay = "detected",
                           segmentationInfo = "boundaries",
                           boundariesAssay = "cell",
                           matrixOnly = FALSE) {
    # check arg validity
    .check_if_me(object)
    .stop_if_null(boundariesAssay, segmentationInfo, moleculesAssay)
    .check_if_character(boundariesAssay, segmentationInfo, moleculesAssay)

    # notify user of argument values chosen
    message(paste0("The \"", moleculesAssay, "\" assay was retrieved from the
      molecules slot. To select another assay, specify it in the moleculesAssay
      argument."))
    message(paste0("The \"", boundariesAssay, "\" assay was retrieved from the
      boundaries slot. To select another assay, specify it in the boundariesAssay
      argument."))

    # Function should be flexible to different segmentation information
    # priority for boundaries as 10x and vizgen have this info, but not masks
    if (segmentationInfo == "boundaries") {
        spe <- .count_molecules_boundaries(object,
                                        molecules_assay = moleculesAssay,
                                        boundaries_assay = boundariesAssay,
                                        matrix_only = matrixOnly)
    }
    # if (is(segmentationInfo, "masks")) {
    #    return(.count_molecules_masks(object, segmentation))
    # }
    return(spe)
}

# TERRA implementation by default, as it seems faster than sp (see below)
.count_molecules_boundaries <- function(object,
                                        molecules_assay = NULL,
                                        boundaries_assay = NULL,
                                        matrix_only = FALSE) {

  suppressMessages(
    if (isFALSE(identical(
        names(MoleculeExperiment::molecules(object, molecules_assay)[[molecules_assay]]),
        names(MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]])))) 
    {
        stop("Sample IDs do not match between the @molecules slot and the\n
                @boundaries slot.")
    }
  )

  samples <- names(object@molecules$detected)
  features <- sort(unique(unlist(MoleculeExperiment::features(object))))

  xvalsList <- vector(mode = "list", length = length(samples))
  xvalsList <- lapply(xvalsList, function(x) vector(mode = "list", length = length(features)))

  ivalsList <- vector(mode = "list", length = length(samples))
  ivalsList <- lapply(ivalsList, function(x) vector(mode = "list", length = length(features)))

  jnamesList <- vector(mode = "list", length = length(samples))
  jnamesList <- lapply(jnamesList, function(x) vector(mode = "list", length = length(features)))

  bds_all <- MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]]

  bds_all_flat <- MoleculeExperiment::boundaries(object, assayName = boundaries_assay, flatten = TRUE)

  for (sample in samples) {
    bds_df <- bds_all_flat %>% dplyr::filter(sample_id == sample)
    factors <- interaction(bds_df$sample_id, bds_df$segment_id)
    factors_int <- as.integer(factors)
    bds_levels <- levels(factors)
    bds_mat <- as.matrix(cbind(factors_int,
                               bds_df[,c("x_location", "y_location")]))
    colnames(bds_mat) <- c("factors_int", "x", "y")
    bds <- terra::vect(bds_mat, type = "polygons")

    for (feature in features) {
      suppressMessages(
        mols_df <- MoleculeExperiment::molecules(object,
          assayName = molecules_assay)[[molecules_assay]][[sample]][[feature]]
      )

      if (is.null(mols_df)) next

      mols_mat <- as.matrix(mols_df[, c("x_location", "y_location")])
      colnames(mols_mat) <- c("x", "y")

      mols <- terra::vect(mols_mat, type = "points")

      out <- terra::relate(bds, mols, "covers", pairs = TRUE)
      xvals <- tapply(out[,1], out[,1], length)
      ivals <- rep(which(features == feature), length(xvals))
      jvals <- unique(out[,1])
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
                            dimnames = list(inames, jnames))

  if (matrix_only) return(X)

  sample_id <- rep(names(bds_all), times = lapply(bds_all, length))
  centroids <- do.call(rbind, lapply(unlist(bds_all, recursive = FALSE),
                                     colMeans))
  cData <- data.frame(sample_id = sample_id,
                      x_location = centroids[, "x_location"],
                      y_location = centroids[, "y_location"])[colnames(X),]
  cData[,"cell_id"] <- colnames(X)

  spe <- SpatialExperiment::SpatialExperiment(
            assays = list(counts = X),
            colData = cData,
            spatialCoords = as.matrix(cData[, c("x_location", "y_location")]),
            reducedDims = list(spatial = as.matrix(cData[, c("x_location", "y_location")])))

  return(spe)
}

# .count_molecules_masks(){
#    # should recognise an array
# }
#


# ==============================================================================
# SP implementation of countMolecules
# not intended for end user
# ==============================================================================

countMolecules_sp <- function(object,
                                moleculesAssay = "detected",
                                segmentationInfo = "boundaries",
                                boundariesAssay = "cell",
                                matrixOnly = FALSE) {
    # check arg validity
    .check_if_me(object)
    .stop_if_null(boundariesAssay, segmentationInfo, moleculesAssay)
    .check_if_character(boundariesAssay, segmentationInfo, moleculesAssay)

    # notify user of argument values chosen
    message(paste0("The \"", moleculesAssay, "\" assay was retrieved from the
      molecules slot. To select another assay, specify it in the moleculesAssay
      argument."))
    message(paste0("The \"", boundariesAssay, "\" assay was retrieved from the
      boundaries slot. To select another assay, specify it in the boundariesAssay
      argument."))

    if (segmentationInfo == "boundaries") {
        spe <- countMoleculesBoundaries_sp(object,
                                            molecules_assay = moleculesAssay,
                                            boundaries_assay = boundariesAssay,
                                            matrix_only = matrixOnly)
    }
    return(spe)
}

countMoleculesBoundaries_sp <- function(object,
                                        molecules_assay = NULL,
                                        boundaries_assay = NULL,
                                        matrix_only = FALSE) {

  suppressMessages(
    if (isFALSE(
          identical(
              names(MoleculeExperiment::molecules(object, molecules_assay)[[molecules_assay]]),
              names(MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]])))) {
      stop("Sample IDs do not match between the @molecules slot and the\n
      @boundaries slot.")
    }
  )

  # create SpatialPolygon object for each sample (from sp package)
  srList <- lapply(
    MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]],
    function(bds) {
        sp::SpatialPolygons(mapply(
            # create Polygons obj from Polygon objects for each segment_id
            function(x, y, nm) {
                sp::Polygons(list(sp::Polygon(cbind(x, y))), nm)
            },
            x = lapply(bds, "[", "x_location"),
            y = lapply(bds, "[", "y_location"),
            nm = as.character(names(bds))))
    }
    )

  suppressMessages(
    readsList <- lapply(
      MoleculeExperiment::molecules(object, molecules_assay)[[molecules_assay]],
      function(reads) {
          reads <- lapply(reads, function(rds) {
              sp::coordinates(rds) <- ~x_location + y_location
              return(rds)
          })
          return(reads)
      })
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
                    unlist(
                        sapply(
                            seq_len(length(out)), function(i) names(out[[i]]))),
                    times = lapply(unlist(out, recursive = FALSE), length))
  i_names <- sort(unique(all_i_names))
  all_i <- match(all_i_names, i_names)

  all_j_names <- names(unlist(lapply(out, function(x) stats::setNames(x, NULL))))
  j_names <- sort(unique(all_j_names))
  all_j <- match(all_j_names, j_names)
  
  X <- Matrix::sparseMatrix(all_i,
                            all_j,
                            x = all_x,
                            dimnames = list(i_names, j_names))

  if (matrix_only) return(X)

  suppressMessages(
    sample_id <- rep(
      names(MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]]),
      times = lapply(MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]], length))
  )

  suppressMessages(
    centroids <- do.call(rbind,
                   lapply(
                      unlist(MoleculeExperiment::boundaries(object, boundaries_assay)[[boundaries_assay]],
                          recursive = FALSE),
                      colMeans))
  )

  cData <- data.frame(sample_id = sample_id,
                        x_location = centroids[, "x_location"],
                        y_location = centroids[, "y_location"],
                        cell_id = colnames(X))

  spe <- SpatialExperiment::SpatialExperiment(
            assays = list(counts = X),
            colData = cData,
            spatialCoords = as.matrix(cData[, c("x_location", "y_location")]),
            reducedDims = list(spatial = as.matrix(cData[, c("x_location", "y_location")])))

  return(spe)
}