# ==============================================================================
# helper functions for main functions
# ==============================================================================

# ------------------------------------------------------------------------------
# get essential columns
.get_essential_cols <- function(factor_col, x_col, y_col) {
    essential_cols <- list(factor_col,
                        x_col,
                        y_col)
    # check essential columns

    v <- unlist(lapply(essential_cols, is.null))

    if (isTRUE(any(v))) {
        stop("Essential columns have not been specified. Please specify column
names in the three \"col\" arguments to this function")

    }
    return(unlist(essential_cols))
}

# ------------------------------------------------------------------------------
# get standard columns depending on the slot
.get_standard_cols <- function(df_type) {
    if (df_type == "transcripts") {
        standard_cols <- c("feature_name",
                            "x_location",
                            "y_location")
    }
    if (df_type == "boundaries") {
        standard_cols <- c("segment_id",
                            "x_location",
                            "y_location")
    }
    return(standard_cols)
}

# ------------------------------------------------------------------------------
# standardise column names across technologies
.standardise_cols <- function(df,
                                standard_cols,
                                essential_cols) {

    if (!identical(essential_cols, standard_cols)) {
        # get index for essential cols
        for (col in essential_cols) {
            idx <- grep(col, colnames(df))
            # change colnames to standards
            colnames(df)[idx] <- standard_cols[which(essential_cols == col)]
        }
    }

    return(df)
}

# -----------------------------------------------------------------------------
# select specified columns
.select_cols <- function(df,
                            keep_cols,
                            standard_cols) {
    
    if (keep_cols == "essential") {
        cols <- standard_cols
    } else if (keep_cols == "all") {
        cols <- colnames(df)
    } else {
        cols <- keep_cols
        # check that essential columns have been selected too
        for (c in standard_cols){
            if (!c %in% cols) {
                stop("Essential columns could not be identified in the
keep_cols argument. Essential columns are those specified in the \"col\"
arguments of this function.")
            }
        }   
    }
    return(cols)
}

# -----------------------------------------------------------------------------
# scale location column data to unit microns
.scale_locations <- function(df, scale_factor) {
    df[["x_location"]] <- df[["x_location"]] / scale_factor
    df[["y_location"]] <- df[["y_location"]] / scale_factor
    return(df)
}

# -----------------------------------------------------------------------------
# standardise csv file
# use ... argument to pass column by which to factor df

#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
.standardise_to_list <- function(df, cols, ...) {

    df %<>%
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::group_by(dplyr::pick(...))

    ls <- df %>%
        dplyr::group_split(.keep = FALSE) %>%
        purrr::set_names(unlist(dplyr::group_keys(df))) %>%
        as.list()

    return(ls)
}

# -----------------------------------------------------------------------------
# get sample IDs by retrieving name of parent directory
.get_sample_id <- function(n_samples, f_paths) {

    ids <- vector("character", length = n_samples)

    for (f in seq_along(f_paths)) {
        id <- base::strsplit(f_paths[[f]], "/") %>%
            unlist(use.names = FALSE) %>%
            utils::tail(2) %>%
            utils::head(1)

        ids <- replace(ids, f, values = id)
    }
    return(ids)
}

# ------------------------------------------------------------------------------
# flatten ME list whilst retaining information of list headers
.addColumnToNestedListFlatten <- function(listObject, column_name = NULL) {
  listObject_added = mapply(function(df, nm) {
    df[, column_name] <- nm
    return(df)
  }, listObject, names(listObject), SIMPLIFY = FALSE)
  return(do.call(rbind, listObject_added))
}

.flatten_molecules <- function(me, assay_name) {
  molecules_flat <- addColumnToNestedListFlatten(
    lapply(me@molecules[assay_name], function(mol_2) addColumnToNestedListFlatten(
      lapply(mol_2, function(mol_1) addColumnToNestedListFlatten(
        mol_1, "feature_name")), "sample_id")),
    column_name = NULL)

  return(molecules_flat)
}

.flatten_boundaries <- function(me, assay_name) {
  molecules_flat <- addColumnToNestedListFlatten(
    lapply(me@boundaries[assay_name],
        function(mol_2) addColumnToNestedListFlatten(
            lapply(mol_2, function(mol_1) addColumnToNestedListFlatten(
                mol_1, "segment_id")), "sample_id")),
    column_name = NULL)
  return(molecules_flat)
}
