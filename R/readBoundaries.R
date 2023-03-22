# =============================================================================
# read in boundary information
# =============================================================================

#' read boundary files for molecule-based ST experiments
#' 
#' @param me A MoleculeExperiment object.
#' 

readBoundaries <- function(me, data_dir)
        # only for xenium for now

        # make this function work with either CELL data or NUCLEI data 
        # in arguments? or where?

        # locate files in directory
        data_dir

        # read in files
        bds <- data.table::fread() 
        # standardise csv to same list of lists format as readMolecules
        # structure should be: me@boundaries$cells$sample1$cellID$vertex_df
        bds_ls <-

        # specify id names like in readMolecules

        # add standardised boundaries list to an already existing ME object
        me@boundaries <- bds_ls


