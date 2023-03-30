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
#' @param scale_factor Integer specifying the scale factor by which to change
#' the scale of the x and y locations (e.g., to change from pixel to micron).
#' The default value is 1.
#'
#' @examples
#'    molecules_df <- data.frame(
#'        sample_id = rep(c("sample1", "sample2"), times = c(30, 20)),
#'        features = rep(c("gene1", "gene2"), times = c(20, 30)),
#'        x_coords = runif(50),
#'        y_coords = runif(50)
#'    )
#'
#'    molecules_ls <- dataframeToMEList(molecules_df,
#'                                      df_type = "transcripts",
#'                                      assay_name = "detected",
#'                                      sample_col = "sample_id",
#'                                      factor_col = "features",
#'                                      x_col = "x_coords",
#'                                      y_col = "y_coords")
#'
#' @export

dataframeToMEList <- function(df,
                                df_type = NULL,
                                assay_name = NULL,
                                sample_col = "sample_id",
                                factor_col,
                                x_col = "x_location",
                                y_col = "y_location",
                                keep_cols = "essential",
                                scale_factor = 1
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

    df <- .scale_locations(df, scale_factor = scale_factor)

    # for each sample, standardise data
    sample_level <- .standardise_to_list(df, cols, sample_id)

    if (df_type == "transcripts") {
        ls <- lapply(sample_level, .standardise_to_list,
                            cols = setdiff(cols, sample_col), feature_name)
    } else if (df_type == "boundaries") {
        ls <- lapply(sample_level, .standardise_to_list,
                            cols = setdiff(cols, sample_col), segment_id)
    }

    # specify assay name for compatibility with ME methods
    ls <- list(ls)
    names(ls) <- assay_name
    return(ls)
}
