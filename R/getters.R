## TODO: define getters for MoleculeExperiment class
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
#
## create new methods of interest
## e.g., for method that accesses the molData slot
#setGeneric("get_molData", 
#    function(x) standardGeneric("get_molData"))
#
#setMethod("get_molData", "className", function(x) x@get_molData) 
#
#
