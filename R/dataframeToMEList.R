#' Convert a transcripts or boundaries file to the ME list format
#'
#' The goal of this function is to prepare transcripts and boundaries files for
#' input to a MoleculeExperiment object.
#' @param df A data.frame containing the transcripts information or the
#' boundaries information. NOTE: this data.frame should, at a minimum, have the
#' following 4 columns: sample_id, feature_name, x_location and y_location.
#' @param sample_col Character string specifying the name of the column with the
#' sample id.
#' @param factor_col Character string specifying the name of the column with the
#' factors with which to group the data in the lists. When working with
#' molecules, this column would be e.g., "feature_name" in xenium. When working
#' with boundaries, this column would be e.g., "cell_id" in xenium.
#' @param x_coord_col Character string specifying the name of the column with
#' global x coordinates.
#' @param y_coord_col Character string specifying the name of the column with
#' global y coordinates.
#' @param keep_cols Character string which can be either "essential" or "all".
#' If "essential", the function will only work with the x and y location
#' information.
#' @export

dataframeToMEList <- function(df,
                                sample_col,
                                factor_col,
                                x_coord_col,
                                y_coord_col,
                                keep_cols = "essential"
                                ) {
    if (keep_cols == "essential") {
        cols <- c(sample_col, factor_col, x_coord_col, y_coord_col)
    } else if (keep_cols == "all") {
        cols <- colnames(df)
    }

    temp_ls <- .standardiseToList(df, cols, get(sample_col))
    .standardiseToList(df, cols, quote((factor_col)))

    # handle sample and sample naming automatically inside this function
    # name samples based on what is found
    return(ls)

}
