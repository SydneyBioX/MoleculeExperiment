test_that("missing or invalid arguments raise an error", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")

    expect_error(readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = NULL,
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "essential"),
                regexp = "not be NULL")

    expect_error(readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = "feature_name",
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = 2),
                regexp = "should be a character")

    expect_no_error(suppressMessages(readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = "feature_name",
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "essential")))
}
)

test_that("expected sample directories are identified", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")

    f_paths <- list.files(repoDir,
                     pattern = "transcripts.csv",
                     full.names = TRUE,
                     recursive = TRUE
    )

    expect_equal(length(f_paths), 2)

    suppressMessages(
        simple_me <- readMolecules(repoDir,
                                    pattern = "transcripts.csv",
                                    featureCol = "feature_name",
                                    xCol = "x_location",
                                    yCol = "y_location",
                                    keepCols = "essential")
    )

    expect_equal(
            length(
                MoleculeExperiment::molecules(
                    simple_me,
                    assayName = "detected")[["detected"]]),
        2)
}
)

test_that("only columns of interest to the user are kept", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")

    essential_me <- readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = "feature_name",
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "essential")

    all_me <- readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = "feature_name",
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "all")

    # choose first feature as an example
    essential_mol_ls <- MoleculeExperiment::molecules(essential_me,
                                                        assayName = "detected")

    all_mol_ls <- MoleculeExperiment::molecules(all_me, assayName = "detected")

    essential_df <- essential_mol_ls[["detected"]][[1]][[1]]
    all_df <- all_mol_ls[["detected"]][[1]][[1]]

    expect_equal(base::ncol(essential_df), 2)
    # we know that xenium example data has 9 cols
    # out of 9 cols, one col is used whilst list indexing
    # so 8 cols should remain
    expect_equal(base::ncol(all_df), 8)
}
)

test_that("output looks like expected", {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
    samples <- c("sample1",
        "sample2")

    simple_me <- readMolecules(repoDir,
                                pattern = "transcripts.csv",
                                featureCol = "feature_name",
                                xCol = "x_location",
                                yCol = "y_location",
                                keepCols = "essential")

    nested_ME_ls <- suppressMessages(
        MoleculeExperiment::molecules(simple_me, assayName = "detected"))

    # should be of ME class
    expect_s4_class(simple_me, "MoleculeExperiment")

    # should have expected nested list structure
    expect_equal(names(nested_ME_ls), "detected")
    expect_equal(names(nested_ME_ls$detected), samples)
}
)