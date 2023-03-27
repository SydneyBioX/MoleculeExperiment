##############################################################################

countMolecules = function(me, moleculesAssay = NULL, boundariesAssay = NULL) {
  
  # check matching of sample ids
  if (isFALSE(identical(names(SpatialUtils::molecules(me, moleculesAssay)),
            names(boundaries(me, boundariesAssay))))) {
      stop("Sample IDs to do not match between the @molecules slot and the
@boundaries slot.")
  }
  
##############################################################################
# create SpatialPolygon object for each sample (from sp package)
srList <- lapply(boundaries(me, boundariesAssay), function(bds) {
    # to each boundary
    sp::SpatialPolygons(
                        mapply(
            # create Polygons obj from Polygon objects for each compartment_ID
            function(x,y,nm) {sp::Polygons(list(sp::Polygon(cbind(x,y))), nm)},
            x = lapply(bds, "[", "x_location"),
            y = lapply(bds, "[", "y_location"),
            # nm corresponds to compartment_ID (e.g., cell_id)
            nm = as.character(names(bds))
                              )
                       )
})

##############################################################################
readsList <- lapply(SpatialUtils::molecules(me, moleculesAssay),
                    function(reads) {
                        reads <- lapply(reads, function(rds) {
                            sp::coordinates(rds) <- ~x_location + y_location
                            return(rds)
                            })
                        return(reads)
                    })
##############################################################################
# MAYBE MAKE MORE EFFICIENT BY PRE-ASSIGNING MEMORY
# outgenes = vector("list", length = )
# cells_n = length(srList[[1]])
# out = vector("list", length = cells_n)

getOutGenes <- function(sr, reads) {
  lapply(reads, function(rds) {
    out <-  sp::over(sr, rds, returnList = TRUE)
    return(unlist(lapply(out, length)))
  })
}

out <- mapply(getOutGenes, srList, readsList)

##############################################################################

all_x <- unlist(out)
all_i_names <- rep(unlist(sapply(seq_len(length(out)),
                                 function(i) names(out[[i]]))),
                   times = lapply(unlist(out, recursive = FALSE), length))
all_i <- as.integer(factor(all_i_names))
all_j_names <- names(unlist(lapply(out, function(x) setNames(x, NULL))))
all_j <- as.integer(factor(all_j_names))
i_names <- unique(all_i_names)
j_names <- unique(all_j_names)

X <- Matrix::sparseMatrix(all_i, all_j, x = all_x, dimnames = list(i_names, j_names))
X

##############################################################################
sample_id <- rep(names(boundaries(me, boundariesAssay)),
                 times = lapply(boundaries(me, boundariesAssay), length))

centroids <- do.call(rbind, lapply(unlist(boundaries(me, boundariesAssay),
                                          recursive = FALSE), colMeans))


cData <- data.frame(sample_id = sample_id,
                   x_location = centroids[,"x_location"],
                   y_location = centroids[,"y_location"],
                   cell_id = colnames(X)
)

##############################################################################

# important--> ONLY LOAD spatialexperiment construction function
spe <- SpatialExperiment::SpatialExperiment(assays = list(counts = X),
            colData = cData,
            spatialCoords = as.matrix(cData[,c("x_location", "y_location")]),
            reducedDims = list(spatial = as.matrix(cData[,c("x_location", "y_location")])))
return(spe)
}

