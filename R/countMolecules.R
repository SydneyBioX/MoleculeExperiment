#' Count molecules per region of interest (e.g., cell)
#' 
#' Function should be flexible to different segmentation information
#' @param me MoleculeExperiment object containing both the transcript data as
#' well as the boundaries data. I.e., the "molecules" and "boundaries" slots
#' need to be filled.
#' See MoleculeExperiment() for more information.
#' @param segmentation_info Character string specifying the type of segmentation
#' information available. Can be either "boundaries" or "masks". Currently,
#' only the "boundaries" information is supported.
#' @param molecules_mode Character string naming the list of the molecules slot
#' from which transcript information should be retrieved from.
#' The default is the raw transcript data that is read in when creating a
#' MoleculeExperiment object. It is possible to change it to another mode, e.g.,
#' "high_threshold" will access the transcript information that has been stored
#' in the "high_threshold" element of the list in the molecules slot.
#' @param boundaries_mode Character string naming the list of the boundaries
#' slot form which boundary information should be retrieved from.
#' For example, for counting transcripts per cell, the list containing the cell
#' boundaries (e.g., "cells") should be selected. 
#' @export
#'
#'
#' @examples

# TODO enable selection of different subslots (raw/high_threshold, or
# cells/nuclei)

countMolecules <- function(me,
                            segmentation_info = "boundaries",
                            molecules_mode = "raw",
                            boundaries_mode) {

    # Function should be flexible to different segmentation information
    # priority for boundaries as 10x and vizgen have this info, but not masks
    if (is(segmentation_info, "boundaries")) {
        return(.countMoleculesBoundaries(me, "raw", "cells"))
    }
    #if (is(segmentation_info, "masks")) {
    #    return(.countMoleculesMasks(me, segmentation))
    #}
}

#######################################
# test ME object
# create molecules_ls from molecules_df
sample_col = "sample_id"
factor_col = "feature_name"
x_coord_col = "x_location"
y_coord_col = "y_location"
cols <- c(sample_col, factor_col, x_coord_col, y_coord_col)

# from mol_df to mol_n should be its own function
mol_tmp <- .standardiseToList(molecules_df, cols, sample_id)
molecules_ls <- lapply(mol_tmp, .standardiseToList, 
                setdiff(cols, "sample_id"), feature_name)

# create boundaries_ls
sample_col = "sample_id"
factor_col = "cell_id"
x_coord_col = "x_location"
y_coord_col = "y_location"
cols <- c(sample_col, factor_col, x_coord_col, y_coord_col)
boundaries_tmp <- .standardiseToList(boundaries_df, cols, sample_id)
boundaries_ls <- lapply(boundaries_tmp, .standardiseToList,
                setdiff(cols, "sample_id"), cell_id)

me <- MoleculeExperiment(molecules = molecules_ls,
                          boundaries = boundaries_ls)

######################################

.countMoleculesBoundaries <- function(me,
                          molecules_mode = "raw",
                          boundaries_mode) {

    # get boundaries 
    x <- get(boundaries_mode)
    me@molecules$x

    # get transcripts
    y <- get(molecules_mode)
    me@molecules$y

    # match transcripts to each boundary 
    # maybe change colnames so that vertex_x matches x_location col from the
    # transcripts tibbles
    sp::over()
    sp::coordinates()

    matrix

    # return counts matrix inside the counts slot of a SpatialExperiment object

    # mean of x and y --> centroid
    centroid <- 

    spe <- SpatialExperiment(assays = matrix1, )

    return(spe)
}

#.countMoleculesMasks(){
#    # should recognise an array
#}
#
