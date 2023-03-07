#' Constructor for creating a MoleculeExperiment object 
#'
#' The moleculeExperiment object ...
#'
#' @param mdf molecule data frame from ST experiment. Output from readMolecules().

# simple version for now

MoleculeExperiment <- function(molecules = list(mdf)){
    # TODO does not work yet as MoleculeExperiment class is still virtual

    # do slot assigning for new class instance
    me <- new("MoleculeExperiment", molecules = mdf)

    # return obj (me = molecule experiment)
    return(me)
}

