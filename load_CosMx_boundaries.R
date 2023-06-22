library(terra)
library(cli)
library(data.table)
library(tidyverse)

# 1. infer image topology ----

data_dir <- "/albona/nobackup2/biostat/datasets/spatial/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images"

cell_mask_dir <- paste(data_dir, "CellLabels", sep = "/")
topology_file <- "Lung9_Rep1_fov_positions_file.csv"

topology <- data.table::fread(paste(data_dir, topology_file, sep = "/"))
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

# 2. Sanitiy check ----

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


# 3. Create a *representative* baby dataset ----

dataDir <- "/albona/nobackup2/biostat/datasets/spatial/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images"
pkg_data_dir <- "inst/extdata/nanostring_Lung9_Rep1"

cell_mask_dir <- paste(dataDir, "CellLabels", sep = "/")
topology_file <- "Lung9_Rep1_fov_positions_file.csv"

topology <- data.table::fread(paste(dataDir, topology_file, sep = "/"))
mask_names <- list.files(cell_mask_dir, pattern = "*.tif", full.names = TRUE)

# Sample 1

image_idxs <- 1:4
sample_idxs <- c(1, 1, 2, 2)
names <- list(
    "CellLabels_F001.tif", "CellLabels_F002.tif",
    "CellLabels_F001.tif", "CellLabels_F002.tif"
)

# square patch taken from the images will be size by size
size <- 400

# save extents for trascript filtering
extents <- list()
# create cropped masks and save them
for (i in image_idxs) {
    image <- terra::rast(mask_names[[i]])
    xmin <- topology[i, 2][[1]]
    xmax <- topology[i, 2][[1]] + ncol(image)
    ymin <- topology[i, 3][[1]]
    ymax <- topology[i, 3][[1]] + nrow(image)
    terra::ext(image) <- c(xmin, xmax, ymin, ymax)

    extent <- terra::ext(image)
    # left image
    if (i %% 2 == 1) {
        crop_extent <- terra::ext(
            c(
                extent$xmax - size / 2, # xmin
                extent$xmax, # xmax
                extent$ymax - size, # ymin
                extent$ymax # ymax
            )
        )
    } else { # right image
        crop_extent <- terra::ext(
            c(
                extent$xmin, # xmin
                extent$xmin + size / 2, # xmax
                extent$ymax - size, # ymin
                extent$ymax # ymax
            )
        )
    }
    extents[[i]] <- crop_extent
    cropped_image <- terra::crop(image, crop_extent)

    terra::writeRaster(
        cropped_image, paste(
            pkg_data_dir,
            paste0("sample_", sample_idxs[i]), "CellLabels",
            names[[i]],
            sep = "/"
        ),
        overwrite = TRUE
    )
}

# compute joined extent for each set of patches.
joined_extents <- list(
    terra::union(extents[[1]], extents[[2]]),
    terra::union(extents[[3]], extents[[4]])
)

# load transcripts
transcripts <- data.table::fread(paste0(
    dataDir,
    "/Lung9_Rep1_tx_file.csv"
))


# TODO: use new extents in fov file
# create filtered transcripts and fov file
for (i in seq_along(joined_extents)) {
    e <- joined_extents[[i]]

    # filter for patch
    mod_transcripts <- transcripts %>%
        filter(
            x_global_px > e$xmin,
            x_global_px < e$xmax,
            y_global_px > e$ymin,
            y_global_px < e$ymax
        )

    utils::write.csv(mod_transcripts,
        file = paste(
            pkg_data_dir,
            paste0("sample_", i),
            "tx_file.csv",
            sep = "/"
        ),
        row.names = FALSE
    )
    utils::write.csv(
        topology %>%
            filter(
                fov == (i - 1) * 2 + 1 | fov == (i - 1) * 2 + 2
            ) %>%
            mutate(fov = row_number()),
        file = paste(
            pkg_data_dir,
            paste0("sample_", i),
            "fov_positions_file.csv",
            sep = "/"
        ),
        row.names = FALSE
    )
}
