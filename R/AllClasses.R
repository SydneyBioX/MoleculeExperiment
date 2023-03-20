# =============================================================================
# The MoleculeExperiment class: documentation and definition
# =============================================================================

#' MoleculeExperiment class: An S4 class container to store imaging-based 
#' spatial transcriptomics data.
#'
#' This class enables the analysis of imaging-based ST data at the molecule
#' level, and standardises data across vendors, which hopefully facilitates
#' ST data integration and comparison.
#' @name MoleculeExperiment-class
## TODO @aliases

#' @docType class
## TODO explain slot properly with Roxygen2
#' @slot molecules 
## TODO explain how to construct ME object 
#' @section Creating an ME object
## TODO explain methods in this same documentation page
#' @section Methods
## TODO add examples of ME obj construction, and methods being used on ME obj
#' @examples
NULL

#' @export
setClass("MoleculeExperiment",
         slots = c(molecules = "list")
)

# ----------------------------------------------------------------------------- 
# Define validity checks
# ----------------------------------------------------------------------------- 
.me_validity <- function(object){
    msg <- NULL
    # if incorrect input, guide user to give correct input
    if (!class(object@molecules) == "list") {
        msg <- c("The molecules slot should contain a list")
    }

    # TODO make more complex validity checks
    #if(){
    #msg <- c(msg, "add more input specifications here")
    #}

    # if object is valid, enable creation of class instance
    if (is.null(msg)) {
        TRUE
    }
}

setValidity("MoleculeExperiment", .me_validity)

