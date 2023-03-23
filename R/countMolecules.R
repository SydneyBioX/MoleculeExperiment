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
#' @param molecule_mode Character string naming the list of the molecules slot
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


countMolecules(me,
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

.countMoleculesBoundaries(me,
                          molecules_mode = "raw",
                          boundaries_mode) {

    # get boundaries 
    x <- get(boundaries_mode)
    me@molecules$x

    # get transcripts
    y <- get(molecules_mode)
    me@molecules$y

    # match transcripts to each boundary 
    sp::over()
    sp::coordinates()

    # return counts matrix inside the counts slot of a SpatialExperiment object
    spe <- SpatialExperiment(assays = )
    return(spe)
}

#.countMoleculesMasks(){
#    # should recognise an array
#}
#
