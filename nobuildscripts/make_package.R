# GOAL: create SpatialUtils R package

library(devtools)
# this should also load usethis package

packageVersion("devtools")

usethis::create_package("/dski/nobackup/bpeters/SpatialUtils")

# specify which files to be ignored during package build
usethis::use_build_ignore("nobuildscripts")
# nobuildscripts will contain scripts where I test my functions

# run build sanity check
devtools::check()
# license is missing

# specify MIT license for package
usethis::use_mit_license()

# sync R's README.Rmd and github's README.md files
usethis::use_readme_rmd()
# REGULARLY render README file
devtools::build_readme()

# remember to update documentation
devtools::document()

# run final check
devtools::check()

# install package
devtools::install()
# package should now be able to be loaded to other scripts via library()
