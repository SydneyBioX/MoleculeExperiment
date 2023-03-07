#' An S4 class to analyse imaging-based ST data at the molecule level
#' @export
# TODO  document 
 #' @rdname name of file where documentation of class should be found
 #' @slot molecules Slot for detected transcripts data.
 #' @importFrom any required functions?
 #' @importClassesFrom SummarizedExperiment

# simple version for now
setClass("MoleculeExperiment",
         slots = c(molecules = list())
)

#setClass("MoleculeExperiment",
#         # make class inherit properties from SummarizedExperiment class
#         contains = "SummarizedExperiment",
#         # define new slots
#
#         # what slots do we want`?
#         slots = c(molecules = list(mdf_1, mdf_2)),
#
#         # specify default behaviour if no input data is provided
#         prototype = list(mdf = list(ANY))
#)
