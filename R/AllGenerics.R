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
setGeneric("boundaries<-",
    function(object, ..., value) standardGeneric("boundaries<-"))

#' @export
setGeneric("features",
    function(object, ...) standardGeneric("features"))

#' @export
setGeneric("molecules",
    function(object, ...) standardGeneric("molecules"))

#' @export
setGeneric("molecules<-",
    function(object, ..., value) standardGeneric("molecules<-"))

#' @export
setGeneric("nFeatures",
    function(object, ...) standardGeneric("nFeatures"))

#' @export
setGeneric("nTranscripts",
    function(object, ...) standardGeneric("nTranscripts"))

#' @export
setGeneric("strMolecules",
    function(object, ...) standardGeneric("strMolecules"))

#' @export
setGeneric("strBoundaries",
    function(object, ...) standardGeneric("strBoundaries"))