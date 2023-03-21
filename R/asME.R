# =============================================================================
# alternative to readXenium, when transcript csv file/s is already read into R,
# and one wants to create an ME object with it.
# TODO finish docs --> how to inherit docs from another function?
# =============================================================================

#' Make MoleculeExperiment with transcript data already read into R.
#'
#' This function is an alternative to readXenium, for when transcript file/s
#' of interest that are already read in R need to be converted to an ME obj.
#'
#' @param transcripts_df A data.frame containing the transcripts information
#' for one or more samples. 
#' @param n_samples Integer specifying the number of samples in the transcripts_df
#' @param technology Character string specifying the technology used for the
#' data. Can be "xenium", "cosmx" or "merscope".

# how to switch between xenium and nanostring?
# TODO make thi

asME <- function(transcripts_df,
                 n_samples = 1,
                 technology = NULL
                 ){
    # add already read transcript data to molecules slot of ME object

    # standardise data
    .splitMolecules()

    # add data to new ME object
    MoleculeExperiment(molecules = )

}
