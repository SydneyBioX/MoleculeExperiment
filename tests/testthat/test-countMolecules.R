test_that("check missing or invalid arguments raise an error", {
    me <- .new_me_obj()

    expect_error(countMolecules(me,
                                moleculesAssay = NULL,
                                boundariesAssay = "cell",
                                matrixOnly = FALSE),
                regexp = "not be NULL")

    expect_error(countMolecules(me,
                                moleculesAssay = 1,
                                boundariesAssay = "cell",
                                matrixOnly = FALSE),
                regexp = "should be a character")
    expect_no_error(countMolecules(me,
                                moleculesAssay = "detected",
                                boundariesAssay = "cell",
                                matrixOnly = FALSE))

}
)

test_that("output is of SpatialExperiment S4 class", {
    me <- .new_me_obj()

    spe <- suppressMessages(countMolecules(me,
                            moleculesAssay = "detected",
                            boundariesAssay = "cell",
                            matrixOnly = FALSE))
    expect_s4_class(me, "MoleculeExperiment")
    expect_s4_class(spe, "SpatialExperiment")
    expect_false(isTRUE(is(spe, "MoleculeExperiment")))
}
)
