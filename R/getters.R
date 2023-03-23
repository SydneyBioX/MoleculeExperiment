# =============================================================================
# Getters for accessing data from the MoleculeExperiment object
# TODO remember to add generic function first to AllGenerics
# TODO redirect documentation to main ME definition documentation page
# TODO make getters such that one can index inside a list:
    # me@boundaries$nuclei
    # to then be able to use boundaries(me, "nuclei")
# =============================================================================

#' @rdname main ManualPageName?
#' @export
setMethod("molecules",
            "MoleculeExperiment",
            function(object){object@molecules})
#setMethod("molecules", "MoleculeExperiment", function(x, i){
#    # as default should work with RAW transcripts
#    # if i is missing --> x@molecules$raw 
#    # if otherwise specified, access other transcripts
#    # if i is provided --> x@molecules$i 
#    x@molecules$
# 
#    # method should NOT print all output to the screen
#})

#' @rdname main ManualPageName?
#' @export
setMethod("boundaries",
            "MoleculeExperiment",
            function(object){object@boundaries})