#' Read in detected transcripts file/s into a MoleculeExperiment object
#'
#' A function to standardise transcripts.csv files across different molecule-
#' based ST technologies, and store them into an ME object.
#' It is technology agnostic, so it is accompanied with wrappers for the
#' specific technologies (e.g., see readXenium).
#'
#' @param dataDir Character string specifying the directory with the file/s
#' containing detected transcripts for different runs/samples.
#' @param pattern Character string specifying the pattern with which to find
#' the transcripts files. For example, in Xenium data, the pattern would be
#' "transcripts.csv". In contrast, in Cosmx data, the pattern would be
#' "tx_file".
#' @param featureCol Character string specifying the name of the column with
#' feature names. For example, "feature_name" in xenium transcripts.csv files.
#' @param xCol Character string specifying the name of the column with the x
#' locations of the transcripts.
#' @param yCol Character string specifying the name of the column with the y
#' locations of the transcripts.
#' @param keepCols Vector of characters specifying the columns of interest from
#' the transcripts file. "essential" selects columns with gene names, x and y
#' locations. "all" will select all columns. Alternatively, specific colums
#' of interest can be selected by specifying them as characters in a vector.
#' Note that this personalised vector needs to contain the essential columns.
#' @param moleculesAssay Character string specifying the name of the list in
#' which the transcript information is going to be stored in the molecules slot.
#' The default name is "detected", as we envision that a MoleculeExperiment will
#' usually be created with raw detected transcript information.
#' @param scaleFactorVector Vector containing the scale factor/s with which to
#' change the coordinate data from pixel to micron. It can be either a single
#' integer, or multiple scale factors for the different samples. The default
#' value is 1.
#'
#' @return A simple MoleculeExperiment object with a filled molecules slot.
#' @export
#' @examples
#' repoDir <- system.file("extdata", package = "MoleculeExperiment")
#' repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
#' simple_me <- readMolecules(repoDir,
#'     pattern = "transcripts.csv",
#'     featureCol = "feature_name",
#'     xCol = "x_location",
#'     yCol = "y_location",
#'     keepCols = "essential"
#' )
#' simple_me
#' @importFrom magrittr %>%
readMolecules <- function(dataDir,
                          pattern = NULL,
                          featureCol = NULL,
                          xCol = NULL,
                          yCol = NULL,
                          keepCols = "essential",
                          moleculesAssay = NULL,
                          scaleFactorVector = 1) {
    # check arg validity
    .stop_if_null(pattern, featureCol, xCol, yCol, keepCols)
    .check_if_character(
        dataDir, pattern, featureCol,
        xCol, yCol, keepCols, moleculesAssay
    )

    # locate paths for all transcripts files
    f_paths <- list.files(dataDir,
        pattern = pattern,
        # store full path names
        full.names = TRUE,
        # look into subdirectories too
        recursive = TRUE
    )
    nSamples <- length(f_paths)

    # get vector of scale factors for all samples
    if (length(scaleFactorVector) == 1) {
        # if all samples have same scale factor, create vector with rep numbers
        scaleFactorVector <- rep(scaleFactorVector, nSamples)
    } else if (!identical(length(scaleFactorVector), nSamples)) {
        stop("The vector of scale factors should be either one value for all
        samples, or a vector of the length of the number of samples, specifying
        a scale factor for each sample")
    }

    # DO DATA STANDARDISATION
    mol_n <- vector("list", nSamples)

    for (f in seq_along(mol_n)) {
        # read_csv modifies transcript_id col and is slower than data.table
        mol_df <- data.table::fread(f_paths[[f]])
        # sprintf function shows that values are not actually changed
        # standardise column names
        essential_cols <- .get_essential_cols(
            factor_col = featureCol,
            x_col = xCol,
            y_col = yCol
        )

        standard_cols <- .get_standard_cols(df_type = "molecules")

        mol_df <- .standardise_cols(mol_df, standard_cols, essential_cols)

        # choose cols of interest
        cols <- .select_cols(mol_df, keep_cols = keepCols, standard_cols)

        # scale coordinates if needed
        if (scaleFactorVector[[f]] != 1) {
            mol_df <- .scale_locations(mol_df,
                scale_factor = scaleFactorVector[[f]]
            )
        }

        # standardise data format to ME list
        # goal = reduce redundancy and save storage space
        mol_n[[f]] <- .standardise_to_list(mol_df, cols, "feature_name")
    }

    # specify sample_ids
    names(mol_n) <- .get_sample_id(n_samples = nSamples, f_paths)

    # add list header to specify location in molecules slot
    # default is detected
    mol_n <- list(mol_n)
    if (is.null(moleculesAssay)) {
        names(mol_n) <- "detected"
    } else {
        names(mol_n) <- moleculesAssay
    }

    # CONSTRUCT SIMPLE ME OBJECT
    me <- MoleculeExperiment(molecules = mol_n)

    return(me)
}
