test_that("missing or invalid arguments raise an error", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")

    expect_error(readBoundaries(dataDir = repoDir,
                                pattern = NULL,
                                segmentIDCol = "cell_id",
                                xCol = "vertex_x",
                                yCol = "vertex_y",
                                keepCols = "essential",
                                boundariesAssay = "nucleus",
                                scaleFactorVector = 1),
                regexp = "not be NULL")
    
    expect_error(readBoundaries(dataDir = repoDir,
                                pattern = "nucleus_boundaries.csv",
                                segmentIDCol = 1,
                                xCol = 2,
                                yCol = 3,
                                keepCols = "essential",
                                boundariesAssay = "nucleus",
                                scaleFactorVector = 1),
                regexp = "should be a character")
    
    expect_no_error(readBoundaries(dataDir = repoDir,
                                pattern = "nucleus_boundaries.csv",
                                segmentIDCol = "cell_id",
                                xCol = "vertex_x",
                                yCol = "vertex_y",
                                keepCols = "essential",
                                boundariesAssay = "nucleus",
                                scaleFactorVector = 1))
}
)

test_that("scaling works", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")

    bdsLs_1 <- readBoundaries(dataDir = repoDir,
                            pattern = "nucleus_boundaries.csv",
                            segmentIDCol = "cell_id",
                            xCol = "vertex_x",
                            yCol = "vertex_y",
                            keepCols = "essential",
                            boundariesAssay = "nucleus",
                            scaleFactorVector = 1)

    bdsLs_2 <- readBoundaries(dataDir = repoDir,
                            pattern = "nucleus_boundaries.csv",
                            segmentIDCol = "cell_id",
                            xCol = "vertex_x",
                            yCol = "vertex_y",
                            keepCols = "essential",
                            boundariesAssay = "nucleus",
                            scaleFactorVector = 2)
    
    # check out first cell as example 
    x_1 <- bdsLs_1[["nucleus"]][[1]][[1]][["x_location"]]
    y_1 <- bdsLs_1[["nucleus"]][[1]][[1]][["y_location"]]
    x_2 <- bdsLs_2[["nucleus"]][[1]][[1]][["x_location"]]
    y_2 <- bdsLs_2[["nucleus"]][[1]][[1]][["y_location"]]

    expect_equal(x_1*2, x_2)
    expect_equal(y_1*2, y_2)
}
)

test_that("expected sample directories are identified", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    cell_paths <- list.files(repoDir,
                     pattern = "cell_boundaries.csv",
                     full.names = TRUE,
                     recursive = TRUE
    )
    nuclei_paths <- list.files(repoDir,
                     pattern = "nucleus_boundaries.csv",
                     full.names = TRUE,
                     recursive = TRUE
    )

    expect_equal(length(cell_paths), 2)
    expect_equal(length(nuclei_paths), 2)
    expect_false(isTRUE(all.equal(nuclei_paths, cell_paths)))
}
)

test_that("standardised column names are returned in the output", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")

    bdsLs <- readBoundaries(dataDir = repoDir,
                            pattern = "nucleus_boundaries.csv",
                            segmentIDCol = "cell_id",
                            xCol = "vertex_x",
                            yCol = "vertex_y",
                            keepCols = "essential",
                            boundariesAssay = "nucleus",
                            scaleFactorVector = 1)

    # take first cell as example
    nested_df <- bdsLs[["nucleus"]][[1]][[1]]
    expected <- c("x_location", "y_location")

    expect_equal(colnames(nested_df), expected)
}
)

test_that("values read by fread are those we expect from known ext datasets", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")

    bdsLs <- readBoundaries(dataDir = repoDir,
                            pattern = "cell_boundaries.csv",
                            segmentIDCol = "cell_id",
                            xCol = "vertex_x",
                            yCol = "vertex_y",
                            keepCols = "essential",
                            boundariesAssay = "cell",
                            scaleFactorVector = 1)

    # take first cell as example
    nested_df <- bdsLs[["cell"]][[1]][["67500"]]

    # prior knowledge
    x_coords_cell_67500_sample_1 <- sort(c(4904.7124, 4898.7627, 4894.3, 4889.8374,
                                    4886.8623, 4887.075, 4889.8374, 4890.6875,
                                    4894.0874, 4907.6875, 4905.775, 4904.7124,
                                    4904.7124))

    expect_equal(sort(nested_df[["x_location"]]), x_coords_cell_67500_sample_1)
}
)

test_that("returned nested list has expected structure", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    samples <- c("Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs",
        "Xenium_V1_FF_Mouse_Brain_MultiSection_2_outs")

    cellLs <- readBoundaries(dataDir = repoDir,
                            pattern = "cell_boundaries.csv",
                            segmentIDCol = "cell_id",
                            xCol = "vertex_x",
                            yCol = "vertex_y",
                            keepCols = "essential",
                            boundariesAssay = "cell",
                            scaleFactorVector = 1)

    nucleiLs <- readBoundaries(dataDir = repoDir,
                            pattern = "nucleus_boundaries.csv",
                            segmentIDCol = "cell_id",
                            xCol = "vertex_x",
                            yCol = "vertex_y",
                            keepCols = "essential",
                            boundariesAssay = "nuclei",
                            scaleFactorVector = 1)

    expect_equal(names(cellLs), "cell")
    expect_equal(names(nucleiLs), "nuclei")
    expect_equal(names(cellLs[["cell"]]), samples)
    expect_equal(names(nucleiLs[["nuclei"]]), samples)
    # take first cell as example
    cell_df <- cellLs[["cell"]][[1]][[1]]
    nuclei_df <- nucleiLs[["nuclei"]][[1]][[1]]
    expect_equal(colnames(cell_df), c("x_location", "y_location"))
    expect_equal(colnames(nuclei_df), c("x_location", "y_location"))
}
)