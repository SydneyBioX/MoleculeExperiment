#' Read and standardise detected transcripts file
#'
#' @param path_transcripts String specifying path for file with detected 
#' transcripts and their 3D locations.
#' @param technology String specifying whether input data was generated with
#' "xenium" (10X Genomics), "cosmx smi" (Nanostring), or "merscope" (Vizgen).
#'
#' @return A standardised detected transcripts file across different
#' imaging-based spatial transcriptomics technologies. This file can be used
#' as input for creating a MoleculeExperiment object.
#' @export
#'
#' @examples
#' write example code here


readMolecules <- function(path_transcripts, technology){
   # # detect technology input
    if(technology == "xenium"){
        # read in detected transcripts file
        # do not use read.csv or read.table from base R, these take too long
        # for GB-sized files with millions of rows.
        # also don't use read_csv from readr, as this modifies table data
        # use data.table package, and fread() function instead
        # this creates obj with class data.frame & data.table
        # mdf = molecule data frame
        mdf <- data.table::fread(path_transcripts)

        # TODO fread also modifies decimals --> should we ignore this? 

        # continue with dplyr for easy data manipulation
        # select minimum columns of interest required for ME object later on
        mdf %<>% dplyr::select(feature_name, x_location, y_location)

        # check that there are no rownames
        # nullify rownames in case there are any
        rownames(mdf) <- NULL

        # TODO change colnames ? 

        # will be used as input for MoleculeExperiment object constructor 
        return(as.data.frame(mdf))
    }

   # if(technology == "vizgen"){
   #    # TODO 

   #    # check that there are no rownames
   #    # better to avoid rownames in large datasets
   #    if(length(rownames(input))!=0){
   #        # remove rownames and keep info in new col
   #        input %<>%
   #    }
   # }
   # if(technology == "cosmx smi"){
   #     # TODO

   #     # check that there are no rownames
   #     if(length(rownames(input))!=0){
   #         # remove rownames and keep info in new col
   #         input %<>%
   #     }

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
