#' An S4 class to analyse imaging-based ST data at the molecule level
#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
###############################################################################
#' @rdname name of file where documentation of class should be found
#' @slot molecules Slot for detected transcripts data.
###############################################################################

.MoleculeExperiment <- setClass("MoleculeExperiment",
        # make class inherit properties from SummarizedExperiment class
        contains = "SummarizedExperiment",
        # define new slots
        slots = c(molecules = list()),

        # specify default behaviour if no input data is provided
        prototype = list(molecules = list(ANY))
)

# define validty checks
#' @importFrom S4Vectors setValidity2
S4Vectors:::setValidity2(class = 'MoleculeExperiment', .molecules_validity)

.molecules_validity <- function(obj){
    # define error messages to direct user to use valid input data
    msg <- NULL
    if(){
        msg <- c(msg, "impose valid check specifications here")
    }
    # if object is valid, enable creation of class instance
    if(is.null(msg)){
        TRUE
    }
    else msg
}

