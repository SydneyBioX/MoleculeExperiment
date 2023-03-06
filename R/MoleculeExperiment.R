#' S4 class for summarising imaging-based spatial transcriptomics data at the
#' molecule level
#'
#' The moleculeExperiment object is an extension of the SummarizedExperiment
#' class.
#'
#' @param mdf molecule data frame from ST experiment. Output from readMolecules().
#' @param
#'
#' maybe importing classes is not needed here, since we import them in
#' class definition already
#' @importClassesFrom SummarizedExperiment SummarizedExperiment

MoleculeExperiment <- function(molecules = list(mdf),
                               ...){
    ## call readMolecules() function
    #mdf <- .readMolecules()
    # OR
    # use readMolecule() as a stand-alone function, and ask the user to put it 
    # as input for this ME object constructor

    # extend SummarizedExperiment class as needed

    # do slot assigning for new class instance
    new("MoleculeExperiment", molecules = mdf)

    # return obj (me = molecule experiment)
    return(me)
}

