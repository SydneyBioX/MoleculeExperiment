# =============================================================================
# Getters for accessing data from the MoleculeExperiment object
# TODO document
# =============================================================================

#' @rdname MoleculeExperiment-class
#' @export
setMethod("molecules",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = "raw", flatten = FALSE) {
                if (assay_name == "raw") {
                    warning("The transcripts from the raw assay were retrieved.
Other assay transcripts can be retrieved by specifying the assay_name argument."
                    )
                }
                if (flatten) {
                    unlist(object@molecules[assay_name])
                } else {
                    object@molecules[assay_name]
                }
            }
)

#' @rdname MoleculeExperiment-class
#' @export
setMethod("boundaries",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, flatten = FALSE) {

                if(is.null(assay_name)) {

                    warning(paste0("All boundaries assays were returned: ",
                    names(object@boundaries), ". To select only a specific 
 boundary subslot, specify the assay_name argument."))

                    if (flatten) {
                        unlist(object@boundaries)
                    } else {
                        object@boundaries
                    } 
                } else {
                    if (flatten) {
                        unlist(object@boundaries[assay_name])
                    } else {
                        object@boundaries[assay_name]
                    }
                }
            }
)