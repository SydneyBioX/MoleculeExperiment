test_that("readXenium wrapper does not change readMolecules behaviour", {
    # use readXenium
    me <- .new_me_obj()
    # use readMolecules
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")

    simple_me <- readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = "feature_name",
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "essential")

    # compare
    expect_equal(suppressMessages(molecules(me, assayName = "detected")),
                    suppressMessages(molecules(simple_me, assayName = "detected")))
}
)

test_that("readXenium wrapper does not change readBoundaries behaviour", {
    # use readXenium
    me <- .new_me_obj()
    # use readBoundaries for cells
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")

    cell_bds_ls <- readBoundaries(dataDir = repoDir,
                                pattern = "cell_boundaries.csv",
                                segmentIDCol = "cell_id",
                                xCol = "vertex_x",
                                yCol = "vertex_y",
                                keepCols = "essential",
                                boundariesAssay = "cell",
                                scaleFactorVector = 1)

    expect_equal(boundaries(me, assayName = "cell"), cell_bds_ls)
}
)

test_that("output is of MoleculeExperiment S4 class", {
    me <- .new_me_obj()
    expect_s4_class(me, "MoleculeExperiment")
}
)