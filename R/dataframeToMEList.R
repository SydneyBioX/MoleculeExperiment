#' Convert a transcripts or boundaries file to the ME list format
#'
#' The goal of this function is to prepare transcripts and boundaries files for
#' input to a MoleculeExperiment object.
#' @param df A data.frame containing the transcripts information or the
#' boundaries information. NOTE: this data.frame should, at a minimum, have the
#' following 4 columns: sample_id, feature_name, x_location and y_location.
#' @param assay_name Name with which to identify the information later on in an
#' ME object.
#' @param sample_col Character string specifying the name of the column with the
#' sample id.
#' @param factor_col Character string specifying the name of the column with the
#' factors with which to group the data in the lists. When working with
#' molecules, this column would be e.g., "feature_name" in xenium. When working
#' with boundaries, this column would be e.g., "cell_id" in xenium.
#' @param x_location Character string specifying the name of the column with
#' global x coordinates.
#' @param y_location Character string specifying the name of the column with
#' global y coordinates.
#' @param keep_cols Character string which can be either "essential" or "all".
#' If "essential", the function will only work with the x and y location
#' information.
#' @export

dataframeToMEList <- function(df,
                                assay_name = NULL,
                                sample_col,
                                factor_col,
                                x_location,
                                y_location,
                                keep_cols = "essential"
                                ) {
    if (is.null(assay_name)) {
        stop("Please specify an assay name with the assay_name argument.")
    }

    if (keep_cols == "essential") {
        cols <- c(sample_col, factor_col, x_location, y_location)
    } else if (keep_cols == "all") {
        cols <- colnames(df)
    }

    # TODO standardise column names
        # in transcript df:
            # gene name info is in "feature_name"
        # in boundaries AND transcripts dfs
            # x location info is in "x_location"
            # y location info is in "y_location"

    # for each sample, standardise data
    sample_level <- .standardiseToList(df, cols, sample_col)
    ls <- lapply(sample_level, .standardiseToList, 
                        cols = setdiff(cols, sample_col), factor_col)

    # specify assay name for compatibility with ME methods
    ls <- list(ls)
    names(ls) <- assay_name
    return(ls)
}
