## TODO: define getters and setters for MoleculeExperiment class
## NOTE: methods need to be sorted alphanumerically to avoid collate problems
## NOTE: methods should work optimally with the various column classes given
## as output by the ST machines 
## NOTE: use lowerCamelCase for naming methods
## REMEMBER: new slots usually require new generics and new methods
###############################################################################

## which methods do we get from SummarizedExperiment class out-of-the-box?

## are there any methods we are missing? if yes, we can:
## 1) add a new method to an already existing generic.
## 2) create a new generic with the new method.
 

###############################################################################

## method definitions should be in same page as ME class definition docs 
# thus, redirect to manual page for main ME class definition

##' @rdname mainManualPageName? 

###############################################################################
## define a show() method, by extending the show() method from SE class
#
##' @export
##' @importMethodsFrom SummarizedExperiment show
#setMethod("show", "MoleculeExperiment", function(obj) {
#    callNextMethod()
#    cat(
#        # imagine MoleculeNumbers was a new getter method
#        "molecules has ", ncol(MoleculeNumbers(obj)), " rows\n",
#        sep=""
#    )
#})
#

###############################################################################
#
## example for a new getter 
## remember to add generic function first to "AllGenerics"
#
##' @export
#setMethod("molData", "MoleculeExperiment", function(x) {x@molData})
#
################################################################################
## example for a new setter
#
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
## setReplaceMethod() function???
#
################################################################################
## example of modifying a method from an already existing generic in SE class 
#
##' @export
##' @importMethodsFrom SummarizedExperiment rowData
#setMethod("rowData", "MoleculeExperiment", function(x, ...) {
#    out <- callNextMethod()
#    
#    # add new method features here 
#
#    # Returning the new rowData object
#    out
#})
#
###############################################################################
## ensure that subsetting abilities from SE class are retained in ME class
#
##' @export
#setMethod("[", "MoleculeExperiment", function(obj, i, j, drop=TRUE) {
#    # call data from custom slots and with custom methods
#    mol <- molData(obj) 
#
#    # check that subsetting index i is valid
#    if (!missing(i)) {
#        if (is.character(i)) {
#            fmt <- paste0("<", class(obj), ">[i,] index out of bounds: %s")
#            i <- SummarizedExperiment:::.SummarizedExperiment.charbound(
#                i, rownames(obj), fmt
#            )
#        }
#        # subset data in custom slots
#        i <- as.vector(i)
#        molecules <- molecules[i,,drop=FALSE]
#    }
#
#    # check that subsetting index j is valid
#    if (!missing(j)) {
#        if (is.character(j)) {
#            fmt <- paste0("<", class(obj), ">[,j] index out of bounds: %s")
#            j <- SummarizedExperiment:::.SummarizedExperiment.charbound(
#                j, colnames(obj), fmt
#            )
#        }
#        # subset data in custom slots
#        j <- as.vector(j)
#        molecules <- molecules[,j,drop=FALSE]
#    }
#
#    # extend capabilities of the [ method from the SE class
#    out <- callNextMethod()
#    BiocGenerics:::replaceSlots(out, rowVec=rv, colVec=cv,
#        rowToRowMat=rrm, colToColMat=ccm, 
#        rowToColMat=rcm, colToRowMat=crm, check=FALSE)
#})
#
###############################################################################
# should we care about coercion methods?



