# ==============================================================================
# testing ME function with merscope (vizgen) data
data_dir <- "/dski/nobackup/bpeters/cellCommData_2023/vizgen_HumanOvarianCancerPatient2Slice2"
me_vizgen <- readMerscope(data_dir,
                          n_samples = 1,
                          keep_cols = "essential")