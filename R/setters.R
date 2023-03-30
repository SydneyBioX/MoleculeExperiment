# =============================================================================
# setters to modify data in a MoleculeExperiment object
# =============================================================================

# document setters

#' @param assay_name Name of the assay from which to retrieve data in the ME
#' list.
#' @param value New value to be added to the slot and assay of interest.
#' @rdname MoleculeExperiment-class
setMethod("molecules<-",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, value) {
                if (is.null(assay_name)) {
                    stop("No assay name specified in the assay_name argument.
                    Please specify a title with which to identify this molecule
                    information later on.")
                }
                object@molecules[assay_name] <- value
                methods::validObject(object)
                return(object)
            }
)

#' @param assay_name Name of the assay from which to retrieve data in the ME
#' list.
#' @param value New value to be added to the slot and assay of interest.
#' @rdname MoleculeExperiment-class
setMethod("boundaries<-",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, value)  {
                if (is.null(assay_name)) {
                    stop("No assay name specified in the assay_name argument.
                    Please specify a title with which to identify this boundary
                    information later on.")
                }
                object@boundaries[assay_name] <- value
                methods::validObject(object)
                return(object)
            }
)
