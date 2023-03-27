# =============================================================================
# setters to modify MoleculeExperiment object
# =============================================================================

# TODO docs
#' @param assay_name
setMethod("molecules<-",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, value) {
                if (is.null(assay_name)) {
                    stop("No assay name specified in the assay_name argument.
                    Please specify a title with which to identify this molecule
                    information later on.")
                }
                object@molecules[assay_name] <- value
                validObject(object)
                return(object)
            }
)

# TODO docs
# copy docs from molecules<-
setMethod("boundaries<-",
            signature = signature(object = "MoleculeExperiment"),
            definition = function(object, assay_name = NULL, value)  {
                if (is.null(assay_name)) {
                    stop("No assay name specified in the assay_name argument.
                    Please specify a title with which to identify this boundary
                    information later on.")
                }
                object@boundaries[assay_name] <- value
                validObject(object)
                return(object)
            }
)
