#' Read and standardise detected transcripts file
#'
#' @param data_dir String specifying directory with the file/s with detected
#' transcripts for different runs/samples.
#' @param technology String specifying whether input data was generated with
#' "xenium" (10X Genomics), "cosmx smi" (Nanostring), or "merscope" (Vizgen).
#' @param n_samples Integer specifying number of samples to be read.
#' @param cols Vector of characters specifying the names of the columns
#' containing the gene names, x locations and y locations.
#'
#' @return A standardised detected transcripts file across different
#' imaging-based spatial transcriptomics technologies. This file can be used
#' as input for creating a MoleculeExperiment object.
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
#' TODO write examples 

readMolecules <- function(data_dir, 
                          technology = "xenium",
                          n_samples = 1,
                          cols = c("feature_name", "x_location", "y_location")
                          )
{
    # use browser() and Q for following variable values within local environment
    # browser()
    # use lobstr::tracemem(obj) to see whenever copies are being made

    if(technology == "xenium"){
        # locate paths for all transcripts files
        f_paths <- vector("list", n_samples)

        fs <- list.files(data_dir, 
                         pattern = "transcripts.csv", 
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

            # check for cols of interest
            for (c in cols){
                if (!c %in% colnames(mol_df)) {
                    stop("Default required columns could not be identified.
Please specify column names for gene names, x and y locations in the arguments 
to this function.")
                }
            }
            
            # standardise data
            # coerce df to list to reduce copies being made by modifications

            mol_df %<>% dplyr::select(dplyr::all_of(cols)) %>%
                dplyr::group_by(feature_name)

            mol_ls <- mol_df %>%
                dplyr::group_split(.keep = FALSE) %>%
                purrr::set_names(unlist(dplyr::group_keys(mol_df))) %>%
                as.list()

            mol_n[[f]] <- mol_ls

            ######################################################
           # # USING DATA.TABLE
           # data.table::split(mdf, )
            ######################################################
        }

        # specify sample_ids
        ids <- vector("character", length = n_samples)

        # take the name of the upper directory as the sample_id
        for(f in seq_along(f_paths)) {
            id <- base::strsplit(f_paths[[f]], "/transcripts.*") %>%
                unlist() %>%
                base::strsplit("/") %>%
                unlist() %>%
                tail(1)

            ids <- replace(ids, f, values = id)
        }

        names(mol_n) <- ids

        # specify a summarized printed output? 
        # user might get annoyed by long printed output
        return(mol_n)
    }

   # if(technology == "vizgen"){

   #    # check that there are no rownames
   #    # better to avoid rownames in large datasets
   #    if(length(rownames(input))!=0){
   #        # remove rownames and keep info in new col
   #    }
        # TODO change colnames to be similar to xenium format 

        # check classes of cols?
        # feature_name should be character
        # x_location and y_location should be numeric
   # }

   # if(technology == "cosmx smi"){

   #     # check that there are no rownames
   #     if(length(rownames(input))!=0){
   #         # remove rownames and keep info in new col
   #     }

         # TODO change colnames to be similar to xenium format
 
         # check classes of cols?
         # feature_name should be character
         # x_location and y_location should be numeric

   # }

}

###############################################################################
# ALTERNATIVE 
# maybe have this function as a private .readMolecules() helper function for
# the MoleculeExperiment() constructor function.

#.readMolecules(data_dir, technology){
#    # find detected transcripts file
#    f <- list.files(path = data_dir, 
#        pattern = "^transcripts.csv")
#
#    # Check if file exists. If not, print error message. 
#    if(length(f) == 0){
#        stop("transcripts.csv was not found in the specified directory.")
#    }
#    f_path = paste0(data_dir, sep = "/", f)
#    test_fread <- data.table::fread(input = f_path)
#}
