#' Count molecules per region of interest (e.g., cell)
#' 
#' Function should be flexible to different segmentation information
#' @param me MoleculeExperiment object containing both the transcript data as
#' well as the boundaries data. I.e., the "molecules" and "boundaries" slots
#' need to be filled.
#' See MoleculeExperiment() for more information.
#' @param segmentation Character string specifying whether cell-level data
#' is from a cell boundaries file or a segmentation mask file.
#' @export
#'
#'
#' @examples

# TODO enable selection of different subslots (raw/high_threshold, or
# cells/nuclei)


countMolecules(me, "raw", "cells"){
    # Function should be flexible to different segmentation information
    # priority for boundaries as 10x and vizgen have this info, but not masks
    if (is(segmentation, "boundaries")) {
        return(.countMoleculesBoundaries(me, "raw", "cells"))
    }
    #if (is(segmentation, "masks")) {
    #    return(.countMoleculesMasks(me, segmentation))
    #}
}

.countMoleculesBoundaries(me,
                          molecules_mode = "raw",
                          boundaries_mode = "cells") {

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
