## TODO: define setters for MoleculeExperiment class
#
###############################################################################

#' @export

# below, specify the methods I want to modify from SummarizedExperiment class

#' @importFrom SummarizedExperiment methodName

###############################################################################


##' @rdname write docs page here for main ME class definition
## method definitions should be in same page as ME class definition docs 
#
#
## which methods do we get from SummarizedExperiment class out-of-the-box?
#
## use lowerCamelCase
#setGeneric("addData<-", function(x, value) standardGeneric("addData<-"))
#setMethod("addData<-", "Person", function(x, value) {
#    x@name <- value
#    # check that modified object is still a valid instance of ME class
#    validObject(x)
#    x
#    }
#)
#
