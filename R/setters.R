# =============================================================================
# setters to modify MoleculeExperiment object
# =============================================================================

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
