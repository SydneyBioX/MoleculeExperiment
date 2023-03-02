#' S4 class for summarising imaging-based spatial transcriptomics data at the
#' molecule level
#'
#' The moleculeExperiment object is an extension of the SummarizedExperiment
#' class. 
#'
#' @param
#' @param
#'
#' @importClassesFrom SummarizedExperiment SummarizedExperiment

MoleculeExperiment <- function(
    data_dir,
    molecules = list()
    ... ){

    # call readMolecules() function
    mdf <- .readMolecules()

    # use SummarizedExperiment constructor
    SummarizedExperiment()

    # extend SummarizedExperiment class as needed

    # return obj (me = molecule experiment)
    return(me)
}
