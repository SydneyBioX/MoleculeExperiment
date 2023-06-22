Data in this directory is used by the vignette and the examples of functions
in the package. It corresponds to Xenium by 10x genomics (mouse brain FF
tissue), CosMx by Nanostring (human lung NSCLC) and Merscope by Vizgen (human
ovarian cancer).

# XENIUM
- Downloaded from [here](https://www.10xgenomics.com/resources/datasets/fresh-frozen-mouse-brain-replicates-1-standard)
- Download date: 8th Feb 2023
- Modifications: Raw csv.gz files were uncompressed and only the transcripts
and boundaries from a patch of 20µm x 20µm were used. 
    - x_min <- 4900, x_max <- 4920
    - y_min <- 6400, y_max <- 6420

# MERSCOPE
- Downloaded from [here](https://console.cloud.google.com/storage/browser/vz-ffpe-showcase/HumanOvarianCancerPatient2Slice2?pageState=(%22StorageObjectListTable%22:(%22f%22:%22%255B%255D%22))&prefix=&forceOnObjectsSortingFiltering=false)
- Download date: 27th Feb 2023
- Modifications: raw transcripts file was filtered to only contain molecules
present in a 80µm x 80µm patch.
    - x_min <- 8000, x_max <- 8080
    - y_min <- 8000, y_max <- 8080 

# COSMX
- Downloaded from [here](https://nanostring.com/products/cosmx-spatial-molecular-imager/nsclc-ffpe-dataset/)
- Download date: 27th Feb 2023
- Modifications: Raw transcript file has coordinates in pixels. CosMx specifies that 1 pixel is equal to 0.18 µm. Data included in `inst/extdata/nanostring_Lung9_Rep1` were created using the following R script.

```r
library(terra)
library(data.table)
library(tidyverse)

# vvv REPLACE THIS vvv
data_dir <- "DATA_DIR"
pkg_data_dir <- "inst/extdata/nanostring_Lung9_Rep1"

cell_mask_dir <- paste(data_dir, "CellLabels", sep = "/")
topology_file <- "Lung9_Rep1_fov_positions_file.csv"

topology <- data.table::fread(paste(data_dir, topology_file, sep = "/"))
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
    data_dir,
    "/Lung9_Rep1_tx_file.csv"
))

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
        )
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
        )
    )
}
```