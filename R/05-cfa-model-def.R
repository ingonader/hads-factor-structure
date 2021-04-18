## ######################################################################### ##
## Confirmatory Factor Analysis: Model Definitions
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

library(lavaan)


## ========================================================================= ##
## confirmatory factor analysis
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## define model(s)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

models_cfa <- list(
  "zigmond_2f_cor" = "
      ## Zigmond & Snaith, 1983
      f1 =~ i_01 + i_03 + i_05 + i_07 + i_09 + i_11 + i_13  ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_08 + i_10 + i_12 + i_14  ## depression
  ",
  "razavi_1f" = paste0(
     "## Razavi et al., 1990
      f1 =~ ", paste(varnames_fa, collapse = " + ")
  ),
  "moorey_2f_cor" = "
      ## Moorey et al., 1991
      f1 =~ i_01 + i_03 + i_05 + i_09 + i_11 + i_13                ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## depression
  ",
  "zigmond_mod01_2f_cor" = "
      ## 2 correlated factors, item 7 excluded
      f1 =~ i_01 + i_03 + i_05 + i_09 + i_11 + i_13          ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_08 + i_10 + i_12 + i_14   ## depression
  ",
  "zigmond_mod02_2f_cor" = "
      ## 2 correlated factors, item 7 excluded
      f1 =~ i_01 + i_03 + i_05 + i_09 + i_11 + i_13          ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_08 + i_12 + i_14          ## depression
  ",
  "dunbar_3f_cor" = "
      ## Dunbar et al., 2000, correlated factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
  ",
  "dunbar_3f_hier" = "
      ## Dunbar et al., 2000, hierarchical factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
      f2 =~ f1 + f3     ## NA explains AA and AD
      f1 ~~ 0*f3        ## but AA and AD by themselves are uncorrelated
  ",
  "friedman_3f_cor" = "
      ## Friedman et al., 2001, 3 correlated factors
      f1 =~ i_03 + i_05 + i_09 + i_13                       ## psychic anxiety
      f2 =~ i_01 + i_07 + i_11                              ## psychomotor agitation
      f3 =~ i_02 + i_04 + i_06 + i_08 + i_10 + i_12 + i_14  ## depression
  ",
  "friedman_3f_ortho" = "
      ## Friedman et al., 2001, 3 uncorrelated factors
      f1 =~ i_03 + i_05 + i_09 + i_13                       ## psychic anxiety
      f2 =~ i_01 + i_07 + i_11                              ## psychomotor agitation
      f3 =~ i_02 + i_04 + i_06 + i_08 + i_10 + i_12 + i_14  ## depression
      f1 ~~ 0*f2
      f1 ~~ 0*f3
      f2 ~~ 0*f3
  ",
  "caci_3f_cor" = "
      ## Caci et al., 2003, 3 factors
      f1 =~ i_01 + i_03 + i_05 + i_09 + i_13  ## anxiety
      f2 =~ i_07 + i_11 + i_14                ## restlessness
      f3 =~ i_02 + i_04 + i_06 + i_08 + i_12  ## depression
  ",
  "emons_2f_cor" = "
      ## Emons et al., 2012, 3 factors (shortened)
      f1 =~ i_01 + i_03 + i_05 + i_09 + i_13  ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_08 + i_12  ## depression
  "
)

#' TODO: CHECK:
#' * check if model specification for dunbar's hierarchical model is correct!
#' * check if model specification for dunbar's correlated model is correct (warning msg!)

# ## estimate one model of the list above:
# wch_model <- "dunbar_3f_cor"  ## covariance matrix is not positive definite (due to sampling error), but model estimation runs fine
# fit_cfa <- cfa(models_cfa[[wch_model]], data = dat_fa)
# fit_cfa_summary <- summary(fit_cfa, standardized = TRUE, fit.measures = TRUE)
# fit_cfa_summary$FIT
# bind_rows(fit_cfa_summary$FIT)
# 
# lavInspect(fit_cfa, "cov.lv")
# det(lavInspect(fit_cfa, "cov.lv"))
# eigen(lavInspect(fit_cfa, "cov.lv"))$values


