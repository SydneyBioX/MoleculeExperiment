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
#' @slot molecules Slot containing information about the detected transcripts.
#' This slot is designed as a list of lists, where each sample contains a list
#' of tibbles with information for each gene. The basic information required
#' for this slot are the gene names of the transcripts, as well as their x and
#' y locations.
#' @slot boundaries Slot containing the boundaries defining each segmented cell.
#' The slot is designed as a list of lists, where each sample contains a list
#' of tibbles for each cell, consisting of the x and y coordinates of the
#' polygon vertices defining the cell boundary.
## TODO explain how to construct ME object
#' @section Creating an ME object
## TODO explain methods in this same documentation page
#' @section Methods
## TODO add examples of ME obj construction, and methods being used on ME obj
#' @examples
NULL


#' @export
setClass("MoleculeExperiment",
         slots = c(molecules = "list",
                   boundaries = "list")
)

# CHANGE VALIDATOR
# TODO the class should NOT be able to be created when the molecules slot is 
# empty

# ----------------------------------------------------------------------------- 
# Define validity checks
# ----------------------------------------------------------------------------- 
.me_validity <- function(object){
    msg <- NULL
    # if incorrect input, guide user to give correct input
    if (object@molecules == NULL) {
        msg <- c("Can not create a MoleculeExperiment object without the
        transcripts information.")

    } else if (!class(object@molecules) == "list") {
        msg <- c("The molecules slot should contain a list")
    }

    # TODO make more complex validity checks
    # else if(){
    #msg <- c(msg, "add more input specifications here")
    #}

    # if object is valid, enable creation of class instance
    else if (is.null(msg)) {
        TRUE
    }
}

setValidity("MoleculeExperiment", .me_validity)

