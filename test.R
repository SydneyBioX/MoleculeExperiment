library(microbenchmark)
devtools::load_all()

big <- FALSE

if (big) {
    path <- "/dski/nobackup/biostat/datasets/spatial/XENIUM_mouse_brain/"
    file <- "me_xenium_full.rds"

    me <- readRDS(paste0(path, file))
} else {
    repoDir <- system.file("extdata", package = "MoleculeExperiment")
    me <- readXenium(repoDir,
        keepCols = "essential"
    )
}

bench <- suppressMessages(microbenchmark(
    v1 = .count_molecules_boundaries(me,
        molecules_assay = "detected",
        boundaries_assay = "cell",
        matrix_only = TRUE
    ),
    v2 = .count_molecules_boundaries_v2(me,
        molecules_assay = "detected",
        boundaries_assay = "cell",
        matrix_only = TRUE
    ),
    times = 30L
))

profvis::profvis({
    .count_molecules_boundaries_v2(me,
        molecules_assay = "detected",
        boundaries_assay = "cell",
        matrix_only = FALSE
    )
})


samples <- names(me@molecules$detected)
features <- sort(unique(unlist(MoleculeExperiment::features(me))))

