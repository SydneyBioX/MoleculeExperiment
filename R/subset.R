#' @title Subset functions for MoleculeExperiment objects
#' 
#' @description A set of functions to subset MoleculeExperiment objects by
#'    different factors
#' 
#' @param me MoleculeExperiment object.
#' @param extent The extent in micrometers to subset the me object.
#'    This must be of the form c(xmin, xmax, ymin, ymax).
#'
#' @aliases 
#' subset_by_extent
#' 
#' @return A subsetted MoleculeExperiment object
#' 
#' @export
subset_by_extent <- function(me, extent) {
  
  subset_me <- me
  
  # subset the feature slot
  mols_assay_names <- names(me@molecules)
  for (assay in mols_assay_names) {
    
    samples_names <- names(me@molecules[[assay]])
    for (sample in samples_names) {
      
      features_names <- names(me@molecules[[assay]][[sample]])
      
      # loop over each feature
      for (feature in features_names) {
        
        # filter by x and y locations
        subset_me@molecules[[assay]][[sample]][[feature]] <- 
          subset_me@molecules[[assay]][[sample]][[feature]] %>% filter (
            between(x_location, subset_extent[[1]], subset_extent[[2]]) &
              between(y_location, subset_extent[[3]], subset_extent[[4]]) 
          )
      }
    }
  }
  
  # subset the boundary slot
  bds_assay_names <- names(me@boundaries)
  for (assay in bds_assay_names) {
    
    samples_names <- names(me@boundaries[[assay]])
    for (sample in samples_names) {
      
      slicings <- lapply(subset_me@boundaries[[assay]][[sample]], function(sample) {
        x_loc = sample$x_location
        y_loc = sample$y_location
        for (i in 1:nrow(sample)) {
          # out of extent
          if (!between(x_loc[i], subset_extent[[1]], subset_extent[[2]]) |
              !between(y_loc[i], subset_extent[[3]], subset_extent[[4]])) {
            return(FALSE)
          }
        }
        return(TRUE)
      })
      
      subset_me@boundaries[[assay]][[sample]] <- subset_me@boundaries[[assay]][[sample]][unlist(slicings)]
    }
  }
  
  return(subset_me)
  
}