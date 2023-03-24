#' @param molecules Detected transcripts information in a list format, as is
#' generated with the readMolecules() function.
#' @param boundaries Slot with boundary information in a list format, as is
#' generated with the readBoundaries() function.
#' @rdname MoleculeExperiment-class

#' @export
MoleculeExperiment <- function(molecules, boundaries = NULL) {
    me <- new("MoleculeExperiment",
                molecules = molecules,
                boundaries = boundaries)
    return(me)
}

