#setClass("MEList",
#        slots = c())
#
#setClassUnion("MElist_OR_NULL", c("MElist", "NULL"))
#
##' @export
##' @rdname MoleculeExperiment
#setClass("MoleculeExperiment",
#         slots = c(molecules = "MElist",
#                   boundaries = "MElist_OR_NULL")
#)

setClassUnion("list_OR_NULL", c("list", "NULL"))

#' @export
#' @rdname MoleculeExperiment
setClass("MoleculeExperiment",
         slots = c(molecules = "list",
                   boundaries = "list_OR_NULL")
)

# ------------------------------------------------------------------------------
# Define validity checks
# ------------------------------------------------------------------------------
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

    # if object is valid, enable creation of class instance
    else if (is.null(msg)) {
        TRUE
    }
}

setValidity("MoleculeExperiment", .me_validity)
