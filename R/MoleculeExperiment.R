#' S4 class for imaging-based ST data summarisation and visualisation 
#'
#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment
###############################################################################
#' The moleculeExperiment object ...
#'
#' @param mdf molecule data frame from ST experiment. Output from readMolecules().

# simple version for now

MoleculeExperiment <- function(molecules, ...){
    # the ... argument enables us to pass arguments to the 
    # SummarizedExperiment constructor

    # do slot assigning for SummarizedExperiment class instance
    # me = molecule experiment
    me <- SummarizedExperiment(list(molecules = molecules), ...)

    # convert me obj to our class, via helper function .MoleculeExperiment
    .MoleculeExperiment(me)
}

# do we even need the SE class?

# simpler version that does NOT use the SummarizedExperiment class
simple_me <- setClass("simple_me", slots = c(molecules = "numeric"))
simple_me <- function(molecules){
    me <- new("simple_me", molecules = molecules)
    return(me)
}

