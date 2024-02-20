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
#' @examples
#' data(small_me)
#' 
#' subset_extent <- c(xmin = 3000, xman = 4000, ymin = 2000, ymax = 3000)
#' subset_small_me <- subset_by_extent(small_me, subset_extent)
#' 
#' # check the extent after subsetting
#' extent(subset_small_me, assayName = "detected")
#' 
#' @export
#' @importFrom terra ext 
#' @importFrom cli cli_abort
#' @importFrom methods is
#' @importFrom dplyr filter between
subset_by_extent <- function(me, extent) {
  
  # Input validation
  # me
  if (!methods::is(me, "MoleculeExperiment")) {
    cli::cli_abort(c(
      "x" = "{.var me} is not a MoleculeExperiment object."
    ))
  }
  
  # extent
  e <- tryCatch(
    terra::ext(extent),
    error = function(err) {
      cli::cli_abort(c(
        "Invalid extent.",
        "i" = paste0(
          "{.var extent} must be of the form",
          " c(xmin, xmax, ymin, ymax)."
        ),
        "x" = "From `terra::ext`:",
        " " = "{err$message}"
      ))
    }
  )
  
  subset_me <- me
  
  # 1. subset the feature slot
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
            between(x_location, e$xmin, e$xmax) &
              between(y_location, e$ymin, e$ymax) 
          )
      }
    }
  }
  
  # 2. subset the boundary slot
  bds_assay_names <- names(me@boundaries)
  for (assay in bds_assay_names) {
    
    samples_names <- names(me@boundaries[[assay]])
    for (sample in samples_names) {
      
      slicings <- lapply(subset_me@boundaries[[assay]][[sample]], 
                         function(sample) {
        x_loc = sample$x_location
        y_loc = sample$y_location
        for (i in 1:nrow(sample)) {
         
           # out of extent
          if (!between(x_loc[i], e$xmin, e$xmax) |
              !between(y_loc[i], e$ymin, e$ymax)) {
            return(FALSE)
          }
        }
        return(TRUE)
      })
      
      subset_me@boundaries[[assay]][[sample]] <- 
        subset_me@boundaries[[assay]][[sample]][unlist(slicings)]
    }
  }
  
  return(subset_me)
  
}