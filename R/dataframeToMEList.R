#' Convert a transcript (molecule) or boundary dataframe to the ME list format
#'
#' The goal of this function is to standardise transcript and boundary files
#' for input to a MoleculeExperiment object.
#' @param df A data.frame containing the transcript information or the
#' boundary information. NOTE: this dataframe should, at a minimum, have the
#' following 4 columns: sample_id, factorCol (e.g., feature_id in
#' transcripts, or cell_id in boundaries), x_location and y_location.
#' @param dfType Character string specifying contents of the dataframe. Can be
#' either "molecules" or "boundaries".
#' @param assayName Character string specifying the name with which to identify
#' the information later on in an ME object.
#' @param sampleCol Character string specifying the name of the column with the
#' sample id.
#' @param factorCol Character string specifying the name of the column with the
#' factors with which to group the data in the lists. When working with
#' molecules, this column would be e.g., "feature_id" in xenium. When working
#' with boundaries, this column would be e.g., "cell_id" in xenium.
#' @param xCol Character string specifying the name of the column with
#' global x coordinates.
#' @param yCol Character string specifying the name of the column with
#' global y coordinates.
#' @param keepCols Character string which can be either "essential" or "all".
#' If "essential", the function will only work with the x and y location
#' information.
#' @param scaleFactor Integer specifying the scale factor by which to change
#' the scale of the x and y locations (e.g., to change from pixel to micron).
#' The default value is 1.
#'
#' @return A list with the format required to input it into slots of a
#' MoleculeExperiment object.
#'
#' @examples
#' moleculesDf <- data.frame(
#'     sample_id = rep(c("sample1", "sample2"), times = c(30, 20)),
#'     features = rep(c("gene1", "gene2"), times = c(20, 30)),
#'     x_coords = runif(50),
#'     y_coords = runif(50)
#' )
#'
#' moleculesMEList <- dataframeToMEList(moleculesDf,
#'                                   dfType = "molecules",
#'                                   assayName = "detected",
#'                                   sampleCol = "sample_id",
#'                                   factorCol = "features",
#'                                   xCol = "x_coords",
#'                                   yCol = "y_coords")
#'
#' moleculesMEList
#' @export

dataframeToMEList <- function(df,
                                dfType = NULL,
                                assayName = NULL,
                                sampleCol = "sample_id",
                                factorCol = NULL,
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "essential",
                                scaleFactor = 1
                                ) {
    # check arg validity
    .stop_if_null(dfType, assayName, sampleCol, factorCol, xCol, yCol, keepCols)
    .check_if_character(dfType, assayName, sampleCol,
                        factorCol, xCol, yCol, keepCols)

    # standardise sample col name
    idx <- grep(sampleCol, colnames(df))
    colnames(df)[idx] <- "sample_id"

    # get names of essential cols
    essential_cols <- .get_essential_cols(factor_col = factorCol,
                                            x_col = xCol,
                                            y_col = yCol)
    # add sample col to essential cols
    essential_cols <- c("sample_id", essential_cols)

    # get names of standard cols
    standard_cols <- .get_standard_cols(df_type = dfType)
    # add sample col to standard cols
    standard_cols <- c("sample_id", standard_cols)

    # standardise colnames of essential columns
    df <- .standardise_cols(df, standard_cols, essential_cols)

    # get names of cols of interest
    cols <- .select_cols(df, keep_cols = keepCols, standard_cols)

    # if needed, scale coordinate values
    df <- .scale_locations(df, scale_factor = scaleFactor)

    # for each sample, standardise data format
    sample_level <- .standardise_to_list(df, cols, "sample_id")

    if (dfType == "molecules") {
        ls <- lapply(sample_level, .standardise_to_list,
                            cols = setdiff(cols, "sample_id"), "feature_id")
    } else if (dfType == "boundaries") {
        ls <- lapply(sample_level, .standardise_to_list,
                            cols = setdiff(cols, "sample_id"), "segment_id")
    } else {
        cli::cli_abort(c(
            "{.var dfType} must be {.emph molecules} or {.emph boundaries}!",
            "x" = "Supplied {.var dfType} was {dfType}"
        ))
    }

    # specify assay name for compatibility with ME methods
    ls <- list(ls)
    names(ls) <- assayName
    return(ls)
}
