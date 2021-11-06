## ######################################################################### ##
## Load data
## ######################################################################### ##

#rstudioapi::restartSession()

#' NOTE:
#' This script assumes that the setup-scripts have been already run, e.g.:
#' source("./R/setup.R")

## ========================================================================= ##
## load additional packages
## ========================================================================= ##


## ========================================================================= ##
## load data from file
## ========================================================================= ##

## define filename of data file:
filename_dat <- "211104_HADS_Psychometrie_v4_Diagnosen.sav"

## read data and sanitize names:
dat_raw <- read_sav(file.path(path_dat, filename_dat))
names(dat_raw) <- tolower(names(dat_raw))
