# =============================================================================
# Main function to generate a MoleculeExperiment: ReadMolecules()
## TODO write examples
# =============================================================================

#' Read and standardise the detected transcripts file/s into a 
#' MoleculeExperiment object
#'
#' A function to standardise transcripts.csv files across different molecule-
#' based ST technologies. It is technology agnostic, so it is accompanied with
#' wrappers for the specific technologies (e.g., see readXenium).
#'
#' @param data_dir String specifying directory with the file/s with detected
#' transcripts for different runs/samples.
#' @param technology String specifying whether input data was generated with
#' "xenium" (10X Genomics), "cosmx" (Nanostring), or "merscope" (Vizgen).
#' @param pattern Character string specifying the pattern with which to find
#' the transcripts files. For example, in Xenium data, the pattern would be
#' "transcripts.csv". In contrast, in Cosmx data, the pattern would be
#' "tx_file".
#' @param n_samples Integer specifying number of samples to be read.
#' @param keep_cols Vector of characters specifying the columns of interest from the
#' transcript file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note, that this personalised vector needs to contain the essential columns.
#' @param essential_cols Character vector specifying the essential columns to
#' be retrieved from the transcripts files. This value is inherited from the
#' wrapper functions. 
#'
#' @return A standardised detected transcripts file across different
#' imaging-based spatial transcriptomics technologies. This file can be used
#' as input for creating a MoleculeExperiment object.
#' @examples

#' @export
#'
#' @importFrom magrittr %>%
# TODO maybe use base R pipe operator |>


# TODO --> do we even need the technology argument? maybe for printing messages
readMolecules <- function(data_dir,
                          # technology should NOT be specified here
                          # rather give error messages
                          technology = technology,
                          # alert user to put pattern of transcripts file
                          pattern = NULL,
                          n_samples = 1,
                          # seaprate the arguments of the essential columns
                          keep_cols = "essential",
                          essential_cols = NULL
                          )
{
    # use browser() and Q for following variable values within local environ
    # browser()
    # use lobstr::tracemem(obj) to see whenever copies are being made

# -----------------------------------------------------------------------------
    # TODO check that technology string makes sense
# -----------------------------------------------------------------------------

    # locate paths for all transcripts files
    f_paths <- vector("list", n_samples)

    fs <- list.files(data_dir,
                     pattern = pattern,
                     # store full path names
                     full.names = TRUE,
                     # look into subdirectories too
                     recursive = TRUE
    )

    f_paths <- replace(f_paths, values = fs)

    # DO DATA CONVERSION
    mol_n <- vector("list", n_samples)

    for (f in seq_along(mol_n)) {

        # read in data
        # read.csv takes too long
        # read_csv modifies transcript_id col and is slower than data.table
        mol_df <- data.table::fread(f_paths[[f]])
        # sprintf function shows that values are not actually changed

        ######################################################################
        # check user has specified the essential cols
        if(is.null(essential_cols)){
            stop("Essential columns have not been specified.
Please specify column names for columns containing gene names, x and y
locations in the essential_cols argument of this function.")
        }


        # Standardise colnames such that, regardless of technology:
        # gene name info is in "feature_name"
        # x location info is in "x_location"
        # y location info is in "y_location"

        standard_cols <- c("feature_name",
                            "x_location",
                            "y_location")

        # essential_cols value is inherited from wrapper functions
        if(!identical(essential_cols, standard_cols)){
            # get index for essential cols
            indxs <- grep(paste(essential_cols, collapse = "|"), colnames(mol_df))
            
            # change name of the essential cols with the standard names
            colnames(mol_df)[indxs] <- standard_cols
        }

        # choose cols of interest
        if (keep_cols == "essential") {
            cols <- standard_cols 
        } else if (keep_cols == "all") {
            cols <- colnames(mol_df)
        } else {
            cols <- keep_cols
        }

        # check that essential columns have been selected too
        for (c in standard_cols){
            if (!c %in% cols) {
                stop("Required columns could not be identified.
Please specify column names for columns containing gene names, x and y 
locations in the keep_cols argument of this function.")
            }
        }

        # TODO check classes of cols
        # feature_name should be character
        # x_location and y_location should be numeric


        # standardise data format
        # coerce df to list to reduce redundancy and save storage space
        mol_n[[f]] <- .splitMolecules(mol_df, cols)

    }

    # specify sample_ids
    ids <- vector("character", length = n_samples)

    # take the name of the upper directory as the sample_id

    # TODO so far it works with structure where each directory is for one sample 
    # identify IDs when transcripts files for different samples are in the same
    # directory

    for (f in seq_along(f_paths)) {
        id <- base::strsplit(f_paths[[f]], "/") %>%
            unlist(use.names = F) %>%
            tail(2) %>%
            head(1)

        ids <- replace(ids, f, values = id)
    }

    names(mol_n) <- ids

    # CONSTRUCT ME OBJECT
    me <- MoleculeExperiment(molecules = mol_n)

    return(me)
}

# HELPER FUNCTION FOR CHECKING COLUMN CLASS

