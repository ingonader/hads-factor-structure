## ######################################################################### ##
## Sample description
## ######################################################################### ##

# rm(list = ls())
# rstudioapi::restartSession()

#' NOTE:
#' This script assumes that the following scripts have been run:
#' * setup scripts
#' * data load scripts
#' * data preparation scripts

source("./R/setup.R")
source(file.path(path_r, "01-data-load-raw.R"))
source(file.path(path_r, "02-data-prep.R"))

## ========================================================================= ##
## load additional packages
## ========================================================================= ##


## ========================================================================= ##
## sample description
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## sample size
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## number of subjects that were presented the HADS:
nrow(dat_all)

## number of responders with no missing items:
nrow(dat_fa)

## number of participantes for each study year:
dat_all %>%
  group_by(yr) %>%
  count()
dat_fa %>%
  group_by(yr) %>%
  count()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check Eingabedatum
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

range(dat_fa$t1_datum, na.rm = TRUE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check participant's age
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

range(dat_fa$t1_alter, na.rm = TRUE)
range(dat_fa$t1_alter_calc, na.rm = TRUE)
range(dat_fa$t1_alter_both, na.rm = TRUE)


## ========================================================================= ##
## test assumptions
## ========================================================================= ##

library(MVN)

## univariate normality:
dat_fa[varnames_fa] %>% psych::multi.hist()
dat_fa[varnames_fa] %>% lapply(hist, nclass = 4)

## multivariate normality:
dat_fa[varnames_fa] %>% mvn()
