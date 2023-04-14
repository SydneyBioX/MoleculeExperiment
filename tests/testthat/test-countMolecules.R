test_that("check missing or invalid arguments raise an error", {
    me <- .new_me_obj()

    expect_error(countMolecules(me,
                                moleculesAssay = NULL,
                                segmentationInfo = "boundaries",
                                boundariesAssay = "cell",
                                matrixOnly = FALSE),
                regexp = "not be NULL")

    expect_error(countMolecules(me,
                                moleculesAssay = 1,
                                segmentationInfo = "boundaries",
                                boundariesAssay = "cell",
                                matrixOnly = FALSE),
                regexp = "should be a character")
    expect_no_error(countMolecules(me,
                                moleculesAssay = "detected",
                                segmentationInfo = "boundaries",
                                boundariesAssay = "cell",
                                matrixOnly = FALSE))

}
)

test_that("output is of SpatialExperiment S4 class", {
    me <- .new_me_obj()

    spe <- suppressMessages(countMolecules(me,
                            moleculesAssay = "detected",
                            segmentationInfo = "boundaries",
                            boundariesAssay = "cell",
                            matrixOnly = FALSE))
    expect_s4_class(me, "MoleculeExperiment")
    expect_s4_class(spe, "SpatialExperiment")
    expect_false(isTRUE(is(spe, "MoleculeExperiment")))
}
)

test_that("terra implementation leads to same results as sp implementation", {
    me <- .new_me_obj()

    spe_terra <- suppressMessages(countMolecules(me,
                            moleculesAssay = "detected",
                            segmentationInfo = "boundaries",
                            boundariesAssay = "cell",
                            matrixOnly = FALSE))

    spe_sp <- suppressMessages(countMolecules_sp(me,
                            moleculesAssay = "detected",
                            segmentationInfo = "boundaries",
                            boundariesAssay = "cell",
                            matrixOnly = FALSE))

    counts_terra <- spe_terra@assays@data@listData$counts
    counts_sp <- spe_sp@assays@data@listData$counts

    expect_equal(as.matrix(counts_terra), as.matrix(counts_sp))
}
)