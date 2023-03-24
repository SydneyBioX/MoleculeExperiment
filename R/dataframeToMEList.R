#' Convert a transcripts or boundaries file to the ME list format
#'
#' The goal of this function is to prepare transcripts and boundaries files for
#' input to a MoleculeExperiment object.
#' @param df A data.frame containing the transcripts information or the
#' boundaries information. NOTE: this data.frame should, at a minimum, have the
#' following 4 columns: sample_id, feature_name, x_location and y_location.
#' @param list_id Character string specifying the header name of the list. This
#' is important to store different versions of the transcripts information in a
#' MoleculeExperiment object downstream.
#' @export

dataframeToMEList <- function(df, list_id) {
    # function should be applicable to both molecules_ls and boundaries_ls
    .standardiseToList(df)

    # handle sample and sample naming automatically inside this function
    # name samples based on what is found

} 
