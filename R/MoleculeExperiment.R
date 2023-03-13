#' S4 class for imaging-based ST data summarisation and visualisation 
#'
#' The ME obj enables standardised data storage and data querying for 
#' imaging based ST data.
#' 
###############################################################################
#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment
###############################################################################
##' The moleculeExperiment object ...
##'
##' @param mdf molecule data frame from ST experiment. Output from readMolecules().
#
#
#MoleculeExperiment <- function(molecules, ...){
#    # the ... argument enables us to pass arguments to the 
#    # SummarizedExperiment constructor
#
#    # do slot assigning for SummarizedExperiment class instance
#    # me = molecule experiment
#    me <- SummarizedExperiment(list(molecules = molecules), ...)
#
#    # convert me obj to our class, via helper function .MoleculeExperiment
#    .MoleculeExperiment(me)
#}
#
###############################################################################
# do we even need the SE class?
# simple version for now

MoleculeExperiment <- function(molecules){
    me <- new("MoleculeExperiment", molecules = molecules)
    return(me)
}

