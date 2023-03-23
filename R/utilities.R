# =============================================================================
# helper functions for main functions
# =============================================================================

# -----------------------------------------------------------------------------
# enable boundaries slot to be empty or list

#####' @export
####setClassUnion("list_OR_NULL", c("list", "NULL"))

# -----------------------------------------------------------------------------
# function to standardise csv file
# use ... argument to pass column by which to factor df
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
.standardiseToList <- function (df, cols, ...) {

    df %<>% 
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::group_by(...)

    ls <- df %>%
        dplyr::group_split(.keep = FALSE) %>%
        purrr::set_names(unlist(dplyr::group_keys(df))) %>%
        as.list()

    return(ls)
}

# -----------------------------------------------------------------------------
# function to get sample IDs by retrieving name of parent directory
.getSampleID <- function(n_samples, f_paths) {

    ids <- vector("character", length = n_samples)

    for (f in seq_along(f_paths)) {
        id <- base::strsplit(f_paths[[f]], "/") %>%
            unlist(use.names = FALSE) %>%
            tail(2) %>%
            head(1)

        ids <- replace(ids, f, values = id)
    }
    return(ids)

    # TODO so far it works with structure where each directory is for one sample
    # identify IDs when transcripts files for different samples are in the same
    # directory
}
