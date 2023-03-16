# docs: add new column with molecule data to each tibble in the @molecules slot
# TODO change file name to avoid collation problems (should be loaded after class definition)

##' @export
#
#setMethod("addMoleculeData<-", "MoleculeExperiment", function(x, value) {
#    x@molecules<- value
#
#    # check that modified object is still a valid instance of ME class
#    validObject(x)
#    x
#    }
#)
#

