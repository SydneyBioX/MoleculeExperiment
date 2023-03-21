# =============================================================================
# helper functions for main functions
# =============================================================================


# -----------------------------------------------------------------------------
# standardise transcripts file in one sample
.splitMolecules <- function(mol_df, cols) {

    mol_df %<>% 
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::group_by(feature_name)

    mol_ls <- mol_df %>%
        dplyr::group_split(.keep = FALSE) %>%
        purrr::set_names(unlist(dplyr::group_keys(mol_df))) %>%
        as.list()

    return(mol_ls)

}

# -----------------------------------------------------------------------------

