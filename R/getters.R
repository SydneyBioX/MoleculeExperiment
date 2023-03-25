# =============================================================================
# Getters for accessing data from the MoleculeExperiment object
# TODO redirect documentation to main ME definition documentation page
# =============================================================================

#' @rdname main ManualPageName?
#' @export
setMethod("molecules",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = "raw", flatten = FALSE) {
                if (flatten) {
                    unlist(object@molecules[assay_name])
                } else {
                    object@molecules[assay_name]
                }
            }
)

#' @rdname main ManualPageName?
#' @export
setMethod("boundaries",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, flatten = FALSE) {
                if (flatten) {
                    unlist(object@boundaries[assay_name])
                    
                } else {
                    object@boundaries[assay_name]
                }
            }
)