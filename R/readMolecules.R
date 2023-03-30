#' Read in detected transcripts file/s into a MoleculeExperiment object
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
#' @param feature_col Character string specifying the name of the column with
#' feature names. For example, "feature_name" in xenium transcripts.csv files.
#' @param x_col Character string specifying the name of the column with the x
#' locations of the transcripts.
#' @param y_col Character string specifying the name of the column with the y
#' locations.
#' @param keep_cols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @param molecules_assay Character string specifying the name of the list in
#' which the transcript information is going to be stored in the molecules slot.
#' The default name is "detected", as we envision that a MoleculeExperiment will
#' usually be created with detected transcript information.
#'
#' @return A standardised detected transcripts file across different
#' imaging-based spatial transcriptomics technologies. This file can be used
#' as input for creating a MoleculeExperiment object.
#' @export
#' @examples
# TODO examples


#' @importFrom magrittr %>%

readMolecules <- function(data_dir,
                          pattern = NULL,
                          n_samples = NULL,
                          feature_col = NULL,
                          x_col = NULL,
                          y_col = NULL,
                          keep_cols = "essential",
                          molecules_assay = NULL
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
    # TODO add checks so that user is guided to include a directory with
    # exactly the n_samples

    f_paths <- replace(f_paths, values = fs)

    # DO DATA STANDARDISATION
    mol_n <- vector("list", n_samples)

    for (f in seq_along(mol_n)) {

        # read_csv modifies transcript_id col and is slower than data.table
        mol_df <- data.table::fread(f_paths[[f]])
        # sprintf function shows that values are not actually changed

        # standardise column names
        essential_cols <- .get_essential_cols(factor_col = feature_col,
                                                x_col,
                                                y_col)

        standard_cols <- .get_standard_cols(df_type = "transcripts")

        mol_df <- .standardise_cols(mol_df, standard_cols, essential_cols)

        # choose cols of interest
        cols <- .select_cols(mol_df, keep_cols, standard_cols)

        # standardise data format to ME list
        # goal = reduce redundancy and save storage space
        mol_n[[f]] <- .standardise_to_list(mol_df, cols, feature_name)
    }

    # specify sample_ids
    names(mol_n) <- .get_sample_id(n_samples, f_paths)

    # add list header to specify location in molecules slot
    # default is detected
    mol_n <- list(mol_n)
    if (is.null(molecules_assay)) {
        names(mol_n) <- "detected"
    } else {
        names(mol_n) <- molecules_assay
    }

    # CONSTRUCT SIMPLE ME OBJECT
    me <- MoleculeExperiment(molecules = mol_n)

    # guide user to use getters
    message("\nDetected transcript information can be accessed with molecules(me) or
molecules(me, \"detected\")\n")

    return(me)
}
