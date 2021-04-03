## ######################################################################### ##
## Setup script
## ######################################################################### ##

#rstudioapi::restartSession()

## ========================================================================= ##
## restore renv environment from existing snapshot (renv.lock file)
## ========================================================================= ##

## restore renv environment from existing snapshot (renv.lock file):
renv::restore()

#' NOTE:
#' This assumes that an renv environment exists. This is created in a 
#' specific R file.
#' Sometimes, renv::restore() failed because of missing packages (dependencies).
#' Just re-running renv::restore() again worked, though.


## ========================================================================= ##
## global variables and options
## ========================================================================= ##

options(tibble.width = Inf)

path_raw <- "."
path_dat <- file.path(path_raw, "data")
path_r <- file.path(path_raw, "R")
path_tmp <- file.path(path_raw, "tmp")
path_plot <- file.path(path_raw, "plot")


## ========================================================================= ##
## load packages
## ========================================================================= ##

library(haven)
library(tibble)
library(magrittr)
library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)

## ========================================================================= ##
## load function definitions specific for this project
## ========================================================================= ##

source(file.path(path_r, "function-library.R"))

## ========================================================================= ##
## fonts using showtext
## ========================================================================= ##

# devtools::install_github("yixuan/showtext")
# ## see https://github.com/yixuan/showtext

# library(showtext)
# ## Loading Google fonts (http://www.google.com/fonts)
# 
# ## add font (showtext):
# font_add(family = "Calibri", regular = "Calibri.otf")
# showtext_auto()
# 
# ## Tell showtext the resolution of the device,
# ## only needed for bitmap graphics. Default is 96
# showtext_opts(dpi = 300)
# 
# ## select font for plots below:
font_base <- "Calibri"


