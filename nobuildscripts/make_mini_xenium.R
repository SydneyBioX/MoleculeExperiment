# GOAL: select a mini patch of xenium data
# should be small data but also encompass cell & nuclei

library(dplyr)
library(magrittr)

# define 20µmx20µm patch
x_min <- 4900
x_max <- 4920
y_min <- 6400
y_max <- 6420

# take raw data
sample_1_raw <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"
sample_2_raw <- "/dski/nobackup/bpeters/cellCommData_2023/mouse_brain/Xenium_V1_FF_Mouse_Brain_MultiSection_2_outs"


# specify target dirs
target_dir_1 <- "/dski/nobackup/bpeters/SpatialUtils/inst/extdata/Xenium_V1_FF_Mouse_Brain_MultiSection_1_outs"

target_dir_2 <- "/dski/nobackup/bpeters/SpatialUtils/inst/extdata/Xenium_V1_FF_Mouse_Brain_MultiSection_2_outs"

# =============================================================================
# SAMPLE 1

# select transcripts
transcripts_1 <- data.table::fread(paste0(sample_1_raw, "/transcripts.csv.gz"))
mod_transcripts_1 <- transcripts_1 %>% filter(x_location > x_min &
                                              x_location < x_max &
                                              y_location > y_min &
                                              y_location < y_max)
lobstr::obj_size(mod_transcripts_1)
# 60 kb? should be fine for bioconductor

# save to csv
utils::write.csv(mod_transcripts_1,
                file = paste0(target_dir_1, "/transcripts.csv"))

# -----------------------------------------------------------------------------
# select matching cell boundaries
cell_boundaries_1 <- data.table::fread(paste0(sample_1_raw,
                                            "/cell_boundaries.csv.gz"))
mod_cell_boundaries_1 <- cell_boundaries_1 %>% filter(vertex_x > x_min &
                                            vertex_x < x_max &
                                            vertex_y > y_min &
                                            vertex_y < y_max)

# save to csv
utils::write.csv(mod_cell_boundaries_1,
                file = paste0(target_dir_1, "/cell_boundaries.csv"))


# -----------------------------------------------------------------------------
# select nucleus boundaries
nuc_boundaries_1 <- data.table::fread(paste0(sample_1_raw,
                                            "/nucleus_boundaries.csv.gz"))
mod_nuc_boundaries_1 <- nuc_boundaries_1 %>% filter(vertex_x > x_min &
                                            vertex_x < x_max &
                                            vertex_y > y_min &
                                            vertex_y < y_max)

# save to csv
utils::write.csv(mod_nuc_boundaries_1,
                file = paste0(target_dir_1, "/nucleus_boundaries.csv"))

# =============================================================================
# SAMPLE 2

# select transcripts
transcripts_2 <- data.table::fread(paste0(sample_2_raw,
                                          "/transcripts.csv.gz"))
mod_transcripts_2 <- transcripts_2 %>% filter(x_location > x_min &
                                              x_location < x_max &
                                              y_location > y_min &
                                              y_location < y_max)

# save to csv
utils::write.csv(mod_transcripts_2,
                file = paste0(target_dir_2, "/transcripts.csv"))

# -----------------------------------------------------------------------------
# select matching cell boundaries
cell_boundaries_2 <- data.table::fread(paste0(sample_2_raw,
                                                "/cell_boundaries.csv.gz"))

mod_cell_boundaries_2 <- cell_boundaries_2 %>% filter(vertex_x > x_min &
                                                      vertex_x < x_max &
                                                      vertex_y > y_min &
                                                      vertex_y < y_max)


# save to csv
utils::write.csv(mod_cell_boundaries_2,
                file = paste0(target_dir_2, "/cell_boundaries.csv"))

# -----------------------------------------------------------------------------

# select nucleus boundaries
nuc_boundaries_2 <- data.table::fread(paste0(sample_2_raw,
                                                "/nucleus_boundaries.csv.gz"))

mod_nuc_boundaries_2 <- nuc_boundaries_2 %>% filter(vertex_x > x_min &
                                                     vertex_x < x_max &
                                                     vertex_y > y_min &
                                                     vertex_y < y_max)


# save to csv
utils::write.csv(mod_nuc_boundaries_2,
                file = paste0(target_dir_2, "/nucleus_boundaries.csv"))

