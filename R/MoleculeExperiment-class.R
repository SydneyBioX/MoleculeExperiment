#' An S4 class to analyse imaging-based ST data at the molecule level
#' @export
#' @rdname name of file where documentation of class should be found
#' @slot molecules Slot for molecule-resolved data.
#' @importFrom any required functions?
#' @importClassesFrom SummarizedExperiment

setClass("MoleculeExperiment",
         # make class inherit properties from SummarizedExperiment class
         contains = "SummarizedExperiment",
         # define new slots

         # what slots do we want`?
         slots = list(molecules = list(mdf_1, mdf_2)),

         # specify default behaviour if no input data is provided
         prototype = list(mdf = list(ANY))
)
