# GOAL: make simplest getter for @molecules slot
# remember to redirect documentation to main ME definition documentation page
#' @rdname main ManualPageName?
# remember to add generic function first to AllGenerics

# molecules method gives data for @molecules slot

#' @export
setMethod("molecules", "MoleculeExperiment", function(obj){obj@molecules})

