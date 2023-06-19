library(rhdf5)
devtools::load_all()

repoDir <- system.file("extdata", package = "MoleculeExperiment")
repoDir <- paste0(repoDir, "/xenium_V1_FF_Mouse_Brain")
simple_me <- readMolecules(repoDir,
    pattern = "transcripts.csv",
    featureCol = "feature_name",
    xCol = "x_location",
    yCol = "y_location",
    keepCols = "essential"
)

simple_me


h5dir <- tempfile(fileext = ".h5")
h5createFile(h5dir)
h5createGroup(h5dir, "molecules")

for (assay_name in names(simple_me@molecules)) {
    h5createGroup(h5dir, paste("molecules", assay_name, sep = "/"))
    for (sample_name in names(simple_me@molecules[[assay_name]])) {
        h5createGroup(
            h5dir, paste("molecules", assay_name, sample_name, sep = "/")
        )
        for (feature_name in names(
            simple_me@molecules[[assay_name]][[sample_name]]
        )) {
            h5createGroup(
                h5dir,
                paste(
                    "molecules", assay_name, sample_name, feature_name,
                    sep = "/"
                )
            )
        }
    }
}
