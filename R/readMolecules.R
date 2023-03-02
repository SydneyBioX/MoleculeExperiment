#' Read and standardise detected transcripts file  
#'
#' @param data_dir String specifying directory where spatial transcriptomics
#' data is stored.
#' @param technology String specifying whether input data was generated with
#' Xenium (10X Genomics), CosMx SMI (Nanostring), or MERSCOPE (Vizgen).
#'
#' @return A standardised detected transcript file across different
#' imaging-based spatial transcriptomics technologies.
#' @export
#'
#' @examples
#' write example code here
#'
#' @importFrom ... ???

# maybe have this function as a private .readMolecules() helper function for 
# the MoleculeExperiment() constructor function.
    # move params to MoleculeExperiment()


###############################################################################
# use usethis::use_package("package") to add packages needed for this function

###############################################################################
# example arguments
# data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"
# technology <- "vizgen"

###############################################################################
.readMolecules(data_dir, technology){

    # identify technology input (10x xenium/nanostring cosmx/vizgen merscope)
    # for now just standardise 10x xenium

    # find detected transcripts file
    f <- list.files(path = data_dir, 
        pattern = "^transcripts.csv")

    # Check if file exists. If not, print error message. 
    if(length(f) == 0){
        stop("transcripts.csv was not found in the specified directory.")
    }

    # read in detected transcripts file
    # do not use read.csv or read.table from base R, these take too long for
    # GB large files due to millions of rows.
    # also don't use read_csv from readr, as this modifies table data
    test_readr <- read_csv(f_path, show_col_types = FALSE)
    # use data.table package, and fread() function instead

    f_path = paste0(data_dir, sep = "/", f)
    test_fread <- data.table::fread(input = f_path)

    # continue with dplyr for easy data manipulation
    
    # check that there are no rownames
    # better to avoid rownames in large datasets
    if(rownames(test_fread)!=NULL){
        # (later) remove rownames and keep info in col
    }
    
    # select minimum columns of interest required for the object later on
    mod <- test_fread %>% dplyr::select(feature_name, x_location, y_location)
    
    # (later) change colnames ? 
    
    # return data in optimal format for SummarizedExperiment obj constructor

    # mdf = molecule data frame standardised  
    # will be used as input for MoleculeExperiment object constructor 
    return(mdf)
}
