#' Count molecules per region of interest (e.g., cell) 
#' 
#' Function should be flexible to different segmentation information 
#' @param me MoleculeExperiment object constructed with MoleculeExperiment(). 
#' See MoleculeExperiment() for more information.
#' @param segmentation Character string specifying whether cell-level data
#' is from a cell boundaries file or a segmentation mask file.
#' @export
#'
#'
#' @examples


countMolecules(me, segmentation, ...){
    # Function should be flexible to different segmentation information 
    # priority for boundaries as 10x and vizgen have this info, but not masks
    if (is(segmentation, "boundaries")) {
        return(.countMoleculesBoundaries(me, segmentation, ...))
    }
    if (is(segmentation, "masks")) {
        return(.countMoleculesMasks(me, segmentation, ...))
    }
}
