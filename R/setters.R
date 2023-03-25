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

# docs: add new column with molecule data to each tibble in the @molecules slot
#' @export
setMethod("addMoleculeData<-",
            signature = signature(object = "MoleculeExperiment"),
            function(object, value) {
    object@molecules <- value

    # check that modified object is still a valid instance of ME class
    validObject(object)
    object
    }
)


# docs: method to add new sample data to an existing object.
# ARGS: method should be able to 1) read in new data from new transcripts.csv
# or 2) read in new data from a data frame already in memory.


    # check that modified object is still a valid instance of ME class
#    validObject(object)
