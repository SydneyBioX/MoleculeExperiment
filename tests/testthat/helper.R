# =============================================================================
# helper functions to reduce redundancy in unit tests
# =============================================================================

# constructor-like function to create me object with example xenium data
# it's not compute intensive, so can be run anew each time
.new_me_obj <- function() {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    me <- readXenium(repoDir,
                        keepCols = "essential")
    return(me)
}
