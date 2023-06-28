# ==============================================================================
# focus on testing external interface of functions

test_that("check missing or invalid arguments raise an error", {
    molsDf <- .new_mol_df()
    # factorCol should NOT be NULL
    expect_error(dataframeToMEList(molsDf,
                                  dfType = "molecules",
                                  assayName = "detected",
                                  sampleCol = "sample_id",
                                  factorCol = NULL,
                                  xCol = "x_coords",
                                  yCol = "y_coords"),
                regexp = "not be NULL"
    )

    # factorCol should be a character string to be valid
    expect_error(dataframeToMEList(molsDf,
                                  dfType = "molecules",
                                  assayName = "detected",
                                  sampleCol = "sample_id",
                                  factorCol = features,
                                  xCol = "x_coords",
                                  yCol = "y_coords"),
                regexp = "should be a character"
    )

    # the following should yield NO errors
    expect_no_error(dataframeToMEList(molsDf,
                                  dfType = "molecules",
                                  assayName = "detected",
                                  sampleCol = "sample_id",
                                  factorCol = "features",
                                  xCol = "x_coords",
                                  yCol = "y_coords")
    )
}
)

test_that("scaling works", {
    bdsDf <- .new_bound_df()
    essential_cols <- .get_essential_cols(factor_col = "cell_id",
                                            x_col = "vertex_x",
                                            y_col = "vertex_y")
    standard_cols <- .get_standard_cols(df_type = "boundaries")
    df <- .standardise_cols(bdsDf, standard_cols, essential_cols)
    x1_df <- .scale_locations(df, scale_factor = 1)
    x2_df <- .scale_locations(df, scale_factor = 2)

    expect_equal(x1_df, df)
    expect_false(isTRUE(all.equal(x2_df, df)))
}
)

test_that("standardised column names are returned in the output", {
    molsDf <- .new_mol_df()

    essential_cols <- .get_essential_cols(factor_col = "features",
                                                x_col = "x_coords",
                                                y_col = "y_coords")

    standard_cols <- .get_standard_cols(df_type = "molecules")

    new_df <- .standardise_cols(molsDf, standard_cols, essential_cols)
    
    expected <- c("feature_name", "x_location", "y_location")

    expect_equal(colnames(new_df[-1]), expected)
    expect_false(isTRUE(all.equal(colnames(new_df), colnames(molsDf))))
}
)

test_that("only columns of interest to the user are kept", {
    molsDf <- .new_mol_df()

    # add extra column
    molsDf["test"] <- NA

    essentialMolsLs <- dataframeToMEList(molsDf,
                                  dfType = "molecules",
                                  assayName = "detected",
                                  sampleCol = "sample_id",
                                  factorCol = "features",
                                  xCol = "x_coords",
                                  yCol = "y_coords",
                                  keepCols = "essential")

    allMolsLs <- dataframeToMEList(molsDf,
                                  dfType = "molecules",
                                  assayName = "detected",
                                  sampleCol = "sample_id",
                                  factorCol = "features",
                                  xCol = "x_coords",
                                  yCol = "y_coords",
                                  keepCols = "all")
    # choose first feature as an example
    essential_df <- essentialMolsLs[["detected"]][[1]][[1]]
    all_df <- allMolsLs[["detected"]][[1]][[1]]

    expect_equal(base::ncol(essential_df), 2)
    expect_equal(base::ncol(all_df), 3)
}
)

test_that("returned nested list has expected structure", {
    molsDf <- .new_mol_df()
    genes_sample1 <- c("gene1", "gene2")

    molsLs <- dataframeToMEList(molsDf,
                                  dfType = "molecules",
                                  assayName = "detected",
                                  sampleCol = "sample_id",
                                  factorCol = "features",
                                  xCol = "x_coords",
                                  yCol = "y_coords")

    expect_equal(names(molsLs), "detected")
    expect_equal(length(molsLs[["detected"]]), 2)
    expect_equal(names(molsLs[["detected"]][[1]]), genes_sample1)

}
)