# =============================================================================
# Generics for MoleculeExperiment class
# ordered alphanumerically to avoid collation problems
# =============================================================================

#' @export
setGeneric("addMoleculeData<-",
    function(x, value) standardGeneric("addMoleculeData<-"))

#' @export
setGeneric("nFeatures",
    function(x) standardGeneric("nFeatures"))

#' @export
setGeneric("molecules",
    function(x) standardGeneric("molecules"))

