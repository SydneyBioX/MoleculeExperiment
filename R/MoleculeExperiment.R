#' S4 class for imaging-based ST data summarisation and visualisation 
#'
#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment
###############################################################################
#' The moleculeExperiment object ...
#'
#' @param mdf molecule data frame from ST experiment. Output from readMolecules().

# simple version for now

MoleculeExperiment <- function(mdf, ...){
    # the ... argument enables us to pass arguments to the 
    # SummarizedExperiment constructor

    # do slot assigning for SummarizedExperiment class instance
    # me = molecule experiment
    me <- SummarizedExperiment(list(mdf = mdf), ...)

    # convert me obj to our class, via helper function .MoleculeExperiment
    .MoleculeExperiment(me)

    # alternative
    #me <- new("MoleculeExperiment", molecules = mdf)
    #return(me)
    
}

