# =============================================================================
# Main function to generate a MoleculeExperiment: ReadMolecules()
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
#' @param pattern Character string specifying the pattern with which to find
#' the transcripts files. For example, in Xenium data, the pattern would be
#' "transcripts.csv". In contrast, in Cosmx data, the pattern would be
#' "tx_file".
#' @param n_samples Integer specifying number of samples to be read.
#' @param keep_cols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @param essential_cols Character vector specifying the names of the columns
#' with the gene names of the detected transcripts, as well as the x and y
#' location columns. Specifically in that order. The values for this argument
#' are inherited from the wrapper functions as it depends on the ST technology.
#' @param molecules_mode Character string specifying the name of the list in
#' which the transcript information is going to be stored in the molecules slot.
#' The default name is "raw", as we envision that a MoleculeExperiment will
#' usually be created with raw transcript information.
#'
#' @return A standardised detected transcripts file across different
#' imaging-based spatial transcriptomics technologies. This file can be used
#' as input for creating a MoleculeExperiment object.
#' @examples

#' @export
#'
#' @importFrom magrittr %>%
#' 
# TODO write examples
# TODO maybe use base R pipe operator |>
# TODO add header entry to the list of lists, called "raw". To then be able to
# index like this: me@molecules$raw, and also distinguish other
# transcript options (e.g., me@molecules$raw)
# TODO --> do we even need the technology argument? maybe for printing messages
readMolecules <- function(data_dir,
                          pattern = NULL,
                          n_samples = NULL,
                          keep_cols = "essential",
                          # specify essential cols if they deviate from the
                          # default feature names and x and y locations.
                          essential_cols = NULL,
                          # one can specify the name of the list in which the 
                          # transcripts get stored.
                          # the default is raw.
                          molecules_mode = NULL
                          )
{
    # use browser() and Q for following variable values within local environ
    # use lobstr::tracemem(obj) to see whenever copies are being made

    if (is.null(pattern)) {
        stop("Please specify the character pattern with which to uniquely
        idenfity the transcript files of interest. For example, 
        transcripts.csv.")
    } else if (is.null(n_samples)) {
        stop("Please specify the number of samples being considered.")
    }

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
        if (is.null(essential_cols)) {
            stop("Essential columns have not been specified.
Please specify column names for columns containing gene names, x and y
locations, and in that order, in the essential_cols argument of this function.")
        }

        # Standardise colnames such that, regardless of technology:
        # gene name info is in "feature_name"
        # x location info is in "x_location"
        # y location info is in "y_location"

        standard_cols <- c("feature_name",
                            "x_location",
                            "y_location")

        # essential_cols value is inherited from wrapper functions
        if (!identical(essential_cols, standard_cols)) {
            # get index for essential cols
            for (col in essential_cols) {
                idx <- grep(col, colnames(mol_df))
                colnames(mol_df)[idx] <- standard_cols[which(essential_cols == col)]
            }
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
                stop("Essential columns could not be identified.
Please specify column names for columns containing gene names, x and y 
locations in the keep_cols argument of this function.")
            }
        }

        # standardise data format
        # coerce df to list to reduce redundancy and save storage space
        mol_n[[f]] <- .standardiseToList(mol_df, cols, feature_name)
    }

    # specify sample_ids
    names(mol_n) <- .getSampleID(n_samples, f_paths)

    # add list header to specify location in molecules slot
    # default is raw
    mol_n <- list(mol_n)
    if (is.null(molecules_mode)) {
        names(mol_n) <- "raw"
    } else {
        names(mol_n) <- molecules_mode
    }

    # CONSTRUCT ME OBJECT
    me <- MoleculeExperiment(molecules = mol_n,
                                boundaries = list("empty default list"))

    return(me)

    # guide user
    cat("Detected transcript information can be accessed with molecules(me) or
    molecules(me, \"raw\") ")
}