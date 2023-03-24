# =============================================================================
# Generics for MoleculeExperiment class, and their default methods
# =============================================================================

#' @export
setGeneric("addMoleculeData<-",
    function(object, ..., value) standardGeneric("addMoleculeData<-"))

#' @export
setGeneric("boundaries",
    function(object, ...) standardGeneric("boundaries"))

#' @export
setGeneric("molecules",
    function(object, ...) standardGeneric("molecules"))

#' @export
setGeneric("nFeatures",
    function(object, ...) standardGeneric("nFeatures"))

#' @export
setGeneric("readBoundaries",
    function(object, ...) standardGeneric("readBoundaries"))

#' @export
setGeneric("summariseMolecules",
    function(object, ...) standardGeneric("summariseMolecules"))

#' @export
setGeneric("summariseBoundaries",
    function(object, ...) standardGeneric("summariseBoundaries"))