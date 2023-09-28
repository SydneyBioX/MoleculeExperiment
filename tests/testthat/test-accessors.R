test_that("accessor functions only work when correct assayName is specified", {
    me <- .new_me_obj()
    mol_df <- .new_mol_df()
    bound_df <- .new_bound_df()
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
    nucleiMEList <- readBoundaries(dataDir = repoDir,
                                pattern = "nucleus_boundaries.csv",
                                segmentIDCol = "cell_id",
                                xCol = "vertex_x",
                                yCol = "vertex_y",
                                keepCols = "essential",
                                boundariesAssay = "nucleus",
                                scaleFactorVector = 1)

    expect_error(molecules(me))
    expect_error(boundaries(me))
    expect_error(molecules(me, assayName = "invented"))
    expect_error(boundaries(me, assayName = "invented"))
    expect_error(molecules(me, assayName = 1))
    expect_error(boundaries(me, assayName = 1))
    expect_error(borndaries(me) <- nucleiMEList)
    expect_error(borndaries(me, 1) <- nucleiMEList)

    expect_no_error(molecules(me, assayName = "detected"))
    expect_no_error(boundaries(me, assayName = "cell"))
    expect_no_error(boundaries(me, assayName = "nucleus") <- nucleiMEList)
}
)

test_that("extent function only works when correct assayName is specified", {
    me <- .new_me_obj()
    expect_error(extent(me))
    expect_error(extent(me, assayName = 1))
    expect_error(extent(me, assayName = "invented"))

    expect_no_error(extent(me, assayName = "detected"))
}
)

test_that("segmentIDs function works with appropriate input", {
    me <- .new_me_obj()
    expect_error(segmentIDs(me))
    expect_error(segmentIDs(me, assayName = 1))
    expect_error(segmentIDs(me, assayName = "invented"))

    expect_no_error(segmentIDs(me, assayName = "cell"))
}
)

test_that("features() works as expected", {
    me <- .new_me_obj()
    expect_error(features(me))
    expect_error(features(me, assayName = 1))
    expect_error(features(me, assayName = "invented"))

    expect_no_error(features(me, assayName = "detected"))
}
)