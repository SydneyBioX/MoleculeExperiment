# =============================================================================
# helper functions to reduce input redundancy in unit tests
# =============================================================================

# constructor-like function to create me object with example xenium data
# it's not compute intensive, so can be run anew each time
.new_me_obj <- function() {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
    suppressMessages(
        me <- readXenium(repoDir,
                            keepCols = "essential", 
                            addBoundaries = "cell")
    )
    return(me)
}

# create toy transcripts df
.new_mol_df <- function() {
    moleculesDf <- data.frame(
        sample_id = rep(c("sample1", "sample2"), times = c(30, 20)),
        features = rep(c("gene1", "gene2"), times = c(20, 30)),
        x_coords = runif(50),
        y_coords = runif(50)
        )
    return(moleculesDf)
}

# create toy boundaries df
.new_bound_df <- function() {
    boundariesDf <- data.frame(
        sample_id = rep(c("sample1", "sample2"), times = c(16, 6)),
        cell_id = rep(c("cell1", "cell2", "cell3", "cell4",
                        "cell1", "cell2"),
                        times = c(4, 4, 4, 4, 3, 3)),
        vertex_x = c(0, 0.5, 0.5, 0,
                    0.5, 1, 1, 0.5,
                    0, 0.5, 0.5, 0,
                    0.5, 1, 1, 0.5,
                    0, 1, 0, 0, 1, 1),
        vertex_y = c(0, 0, 0.5, 0.5,
                    0, 0, 0.5, 0.5,
                    0.5, 0.5, 1, 1,
                    0.5, 0.5, 1, 1,
                    0, 1, 1, 0, 0, 1)
        )
    return(boundariesDf)
}