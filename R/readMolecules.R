#' Read and standardise detected transcripts file
#'
#' @param path_transcripts String specifying path for a directory with the
#' file or files with detected transcripts.
#' transcripts and their 3D locations.
#' @param technology String specifying whether input data was generated with
#' "xenium" (10X Genomics), "cosmx smi" (Nanostring), or "merscope" (Vizgen).
#' @param n_samples Integer specifying number of samples to be read.
#'
#' @return A standardised detected transcripts file across different
#' imaging-based spatial transcriptomics technologies. This file can be used
#' as input for creating a MoleculeExperiment object.
#' @export
#'
#' @examples
#' TODO write examples 

###############################################################################
# example data 
path_transcripts <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"
# subset them
mouse_1 <- 
mouse_2 <-


###############################################################################

readMolecules <- function(path_transcripts, 
                          technology = 'xenium', n_samples = 1){
    if(technology == "xenium"){
        # store paths for all transcripts files
        f_paths <- vector("list", n_samples)

        ## is there any file/s containing transcripts.csv in their name?
        ## if yes, add path/s to list 
        ## if not, look into sub-directories, and store paths for files there


        # read in detected transcripts file/s
        # do not use read.csv or read.table from base R, these take too long
        # for GB-sized files with millions of rows.

        # t0 <- Sys.time()
        # test1 <- data.table::fread(path_transcripts)
        # tf <- Sys.time()
        # runtime <- c(tf - t0)
        # runtime # 41.09469 secs


        # t0 <- Sys.time()
        # test2 <- readr::read_csv(path_transcripts)
        # tf <- Sys.time()
        # runtime <- c(tf - t0)
        # runtime # 50.03625 secs

        # also don't use read_csv from readr, as it modifies transcript_id col
        # and Sys.time() shows it is slower than data.table

        # use data.table package, and fread() function instead
        # this creates obj with class data.frame & data.table
        # mdf = molecule data frame

        mdf <- data.table::fread(path_transcripts)

        # sanity check: does fread modify decimals? no, just for visualisation
        # sprintf("%f", head(mdf[,y_location]))

        # continue with dplyr for easy data manipulation

        # check classes of columns are correct

        # TODO check presence of columns required for ME object later on

        # re-order columns but keep all data
        mdf %<>% dplyr::relocate(feature_name, x_location, y_location)

        # TODO check that there are no rownames
        # add rownames to column if they hold useful info 

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
