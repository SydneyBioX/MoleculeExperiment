#' Convert a transcripts or boundaries file to the ME list format
#'
#' The goal of this function is to prepare transcripts and boundaries files for
#' input to a MoleculeExperiment object.
#' @param df A data.frame containing the transcripts information or the
#' boundaries information. NOTE: this data.frame should, at a minimum, have the
#' following 4 columns: sample_id, feature_name, x_location and y_location.
#' @param df_type Character string specifying contents of the data frame. Can be
#' either "transcripts" or "boundaries".
#' @param assay_name Name with which to identify the information later on in an
#' ME object.
#' @param sample_col Character string specifying the name of the column with the
#' sample id.
#' @param factor_col Character string specifying the name of the column with the
#' factors with which to group the data in the lists. When working with
#' molecules, this column would be e.g., "feature_name" in xenium. When working
#' with boundaries, this column would be e.g., "cell_id" in xenium.
#' @param x_col Character string specifying the name of the column with
#' global x coordinates.
#' @param y_col Character string specifying the name of the column with
#' global y coordinates.
#' @param keep_cols Character string which can be either "essential" or "all".
#' If "essential", the function will only work with the x and y location
#' information.
#' @export

dataframeToMEList <- function(df,
                                df_type = NULL,
                                assay_name = NULL,
                                sample_col,
                                factor_col,
                                x_col,
                                y_col,
                                keep_cols = "essential"
                                ) {
    if (is.null(assay_name)) {
        stop("Please specify an assay name with the assay_name argument.")
    }
    if (is.null(df_type)) {
        stop("Please specify the df_type argument as i) \"transcripts\" or
ii) \"boundaries\".")
    }
    # standardise sample col name
    idx <- grep(sample_col, colnames(df))
    colnames(df)[idx] <- "sample_id"

    # standardise colnames of essential columns
    essential_cols <- .get_essential_cols(factor_col, x_col, y_col)
    # add sample col to essential cols
    essential_cols <- c("sample_id", essential_cols)

    standard_cols <- .get_standard_cols(df_type)
    # add sample col to standard cols
    standard_cols <- c("sample_id", standard_cols)

    df <- .standardise_cols(df, standard_cols, essential_cols)

    # select cols of interest
    cols <- .select_cols(df, keep_cols, standard_cols)

    # for each sample, standardise data
    sample_level <- .standardise_to_list(df, cols, sample_id)

    if (df_type == "transcripts") {
        ls <- lapply(sample_level, .standardise_to_list,
                            cols = setdiff(cols, sample_col), feature_name)
    } else if (df_type == "boundaries") {
        ls <- lapply(sample_level, .standardise_to_list,
                            cols = setdiff(cols, sample_col), compartment_ID)
    }

    # specify assay name for compatibility with ME methods
    ls <- list(ls)
    names(ls) <- assay_name
    return(ls)
}
