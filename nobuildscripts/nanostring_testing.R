# ==============================================================================
# testing ME functions with cosmx (nanostring) data

# test readCosmx()
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/nanostring_NSCLC_lung9_rep1/modified/Lung9_Rep1/Lung9_Rep1-Flat_files_and_images"
me_cosmx <- readCosmx(data_dir,
                      n_samples = 1,
                      keep_cols = "essential")