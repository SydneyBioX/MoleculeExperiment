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
#' @param x_location Character string specifying the name of the column with
#' global x coordinates.
#' @param y_location Character string specifying the name of the column with
#' global y coordinates.
#' @param keep_cols Character string which can be either "essential" or "all".
#' If "essential", the function will only work with the x and y location
#' information.
#' @export

dataframeToMEList <- function(df,
                                sample_col,
                                factor_col,
                                x_location,
                                y_location,
                                keep_cols = "essential"
                                ) {
    if (keep_cols == "essential") {
        cols <- c(sample_col, factor_col, x_location, y_location)
    } else if (keep_cols == "all") {
        cols <- colnames(df)
    }

    # keeps redundant factor_col in the tibbles

    .standardiseToList(df, cols, factor_col)
    .standardiseToList(df, cols, factor_col)


    # handle sample and sample naming automatically inside this function
    # name samples based on what is found
    return(ls)

}

########################################
## test ME object
## create molecules_ls from molecules_df
#sample_col = "sample_id"
#factor_col = "feature_name"
#x_coord_col = "x_location"
#y_coord_col = "y_location"
#cols <- c(sample_col, factor_col, x_coord_col, y_coord_col)
#
## from mol_df to mol_n should be its own function
#mol_tmp <- .standardiseToList(molecules_df, cols, sample_id)
#molecules_ls <- lapply(mol_tmp, .standardiseToList, 
#                setdiff(cols, "sample_id"), feature_name)
#
## create boundaries_ls
#sample_col = "sample_id"
#factor_col = "cell_id"
#x_coord_col = "x_location"
#y_coord_col = "y_location"
#cols <- c(sample_col, factor_col, x_coord_col, y_coord_col)
#boundaries_tmp <- .standardiseToList(boundaries_df, cols, sample_id)
#boundaries_ls <- lapply(boundaries_tmp, .standardiseToList,
#                setdiff(cols, "sample_id"), cell_id)
#
#me <- MoleculeExperiment(molecules = molecules_ls,
#                          boundaries = boundaries_ls)
#
#######################################

