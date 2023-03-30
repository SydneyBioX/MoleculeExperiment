setClassUnion("list_OR_NULL", c("list", "NULL"))


# =============================================================================
# The MoleculeExperiment class: documentation and definition
# =============================================================================

#' MoleculeExperiment class: An S4 class container to store imaging-based
#' spatial transcriptomics data.
#'
#' This class enables the analysis of imaging-based ST data at the molecule
#' level, and standardises data across vendors, which hopefully facilitates
#' ST data integration and comparison.
#' @name MoleculeExperiment
#' @rdname MoleculeExperiment-class
#' @aliases MoleculeExperiment MoleculeExperiment-class
#' @docType class
#'
#' @slot molecules Slot containing information about the detected transcripts.
#' This slot is designed as a list of lists, where each sample contains a list
#' of tibbles with information for each gene. The basic information required
#' for this slot are the gene names of the transcripts, as well as their x and
#' y locations.
#' @slot boundaries Slot containing the boundaries defining each segmented cell.
#' The slot is designed as a list of lists, where each sample contains a list
#' of tibbles for each cell, consisting of the x and y coordinates of the
#' polygon vertices defining the cell boundary.
#' 
## TODO explain how to construct ME object
#' @section Creating an ME object
## TODO explain methods in this same documentation page
#' @section Methods
## TODO add examples of ME obj construction, and methods being used on ME obj
#' @examples
#'
#'    molecules_df <- data.frame(
#'        sample_id = rep(c("sample1", "sample2"), times = c(30, 20)),
#'        features = rep(c("gene1", "gene2"), times = c(20, 30)),
#'        x_coords = runif(50),
#'        y_coords = runif(50)
#'    )
#'    boundaries_df <- data.frame(
#'        sample_id = rep(c("sample1", "sample2"), times = c(16, 6)),
#'        cell_id = rep(c("cell1", "cell2", "cell3", "cell4",
#'                        "cell1", "cell2"),
#'                      times = c(4, 4, 4, 4, 3, 3)),
#'        vertex_x = rnorm(22),
#'        vertex_y = rnorm(22)
#'    )
#'    molecules_ls <- dataframeToMEList(molecules_df,
#'                                      df_type = "transcripts",
#'                                      assay_name = "detected",
#'                                      sample_col = "sample_id",
#'                                      factor_col = "features",
#'                                      x_col = "x_coords",
#'                                      y_col = "y_coords")
#'
#'    boundaries_ls <- dataframeToMEList(boundaries_df,
#'                                       df_type = "boundaries",
#'                                       assay_name = "cell",
#'                                       sample_col = "sample_id",
#'                                       factor_col = "cell_id",
#'                                       x_col = "vertex_x",
#'                                       y_col = "vertex_y")
#'
#'    toy_me <- MoleculeExperiment(molecules = molecules_ls,
#'                                 boundaries = boundaries_ls)
#'
#'    toy_me
#' @exportClass MoleculeExperiment
NULL

setClass("MoleculeExperiment",
         slots = c(molecules = "list",
                   boundaries = "list_OR_NULL")
)

# ----------------------------------------------------------------------------- 
# Define validity checks
# ----------------------------------------------------------------------------- 
.me_validity <- function(object) {
    msg <- NULL
    # if incorrect input, guide user to give correct input
    if (is.null(object@molecules)) {
        msg <- c("Can not create a MoleculeExperiment object without the
        transcripts information.")

    } else if (!methods::is(object@molecules, "list")) {
        msg <- c("The molecules slot should contain a list")
    } else if (!methods::is(object@boundaries, "list_OR_NULL")) {
        msg <- c("The boundaries slot should either be empty, or contain a
        list")
    }

    # TODO make more complex validity checks for contents within the list
    # else if(){
    #msg <- c(msg, "add more input specifications here")
    #}

    # if object is valid, enable creation of class instance
    else if (is.null(msg)) {
        TRUE
    }
}

setValidity("MoleculeExperiment", .me_validity)
