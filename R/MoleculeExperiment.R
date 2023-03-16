#' @param molecules Detected transcripts information in a list format, as is
#' generated with the readMolecules() function.
#' @rdname MoleculeExperiment-class

#' @export
MoleculeExperiment <- function(molecules){
    me <- new("MoleculeExperiment", molecules = molecules)
    return(me)
}

