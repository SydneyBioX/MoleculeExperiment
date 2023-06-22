library(terra)
library(cli)
library(data.table)
library(tidyverse)
# 1. infer image topology ----

data_dir <- "/albona/nobackup2/biostat/datasets/spatial/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images"

cell_mask_dir <- paste(data_dir, "CellLabels", sep = "/")
file <- "Lung9_Rep1_fov_positions_file.csv"

topology <- data.table::fread(paste(data_dir, file, sep = "/"))
mask_names <- list.files(cell_mask_dir, pattern = "*.tif", full.names = TRUE)

# check if right number of images
if (!nrow(topology) == length(mask_names)) {
    stop(
        "fov_positions and CellLabels have a different number of images.\n",
        "\tCheck if you have valid CosMX data."
    )
}

# convert each image to polygons
poly_list <- lapply(
    cli::cli_progress_along(
        mask_names,
        name = "1/2 Transforming masks into polygons:"
    ),
    function(i) {
        mask <- terra::rast(mask_names[[i]])

        xmin <- topology[i, 2][[1]]
        xmax <- topology[i, 2][[1]] + ncol(mask)

        ymin <- topology[i, 3][[1]]
        ymax <- topology[i, 3][[1]] + nrow(mask)

        terra::ext(mask) <- c(xmin, xmax, ymin, ymax)
        poly <- terra::as.polygons(mask, round = FALSE)
    }
)

merged_vector <- terra::vect()
for (i in cli::cli_progress_along(poly_list, name = "2/2 Joining patches:")) {
    merged_vector <- rbind(merged_vector, poly_list[[i]])
}

values(merged_vector) <- values(merged_vector) %>%
    tidyr::gather(from, cell_id, na.rm = TRUE) %>%
    dplyr::mutate(unique_cell_id = dplyr::row_number() - 1)


transcripts <- data.table::fread(paste0(
    data_dir,
    "/Lung9_Rep1_tx_file.csv"
))
nuclear <- transcripts[CellComp == "Nuclear"][, .(x_global_px, y_global_px)]
setnames(nuclear, c("x_global_px", "y_global_px"), c("x", "y"))
idx <- sample(seq_len(nrow(nuclear)), 150000)
mols <- terra::vect(as.matrix(nuclear[idx]))

# plot the merged vector (VERY LARGE EDITION)
png("merged.png", width = 8, height = 6, units = "in", res = 1200)
plot(merged_vector)
points(mols, col = "pink", alpha = 0.1)
dev.off()
