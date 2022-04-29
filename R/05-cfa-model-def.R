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
  "zigmond_man_2f_cor" = "
      ## Zigmond & Snaith, 1983, but with mandatory items only
      f1 =~ i_01 + i_03 + i_05 + i_09  ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_12  ## depression
  ",
  "razavi_1f" = paste0(
     "## Razavi et al., 1990
      f1 =~ ", paste(varnames_fa, collapse = " + ")
  ),
  "smith_1f" = "
      ## Smith, 2006, Excluded items 10, 11, 14 from one-factor solution
      f1 =~ i_01 + i_02 + i_03 + i_04 + i_05 + i_06 + i_07 + i_08 + i_09 + i_12 + i_13
  ",
  "smith_2f" = "
      ## Smith, 2006, Excluded items 10 and 11 from two-factor solution
      f1 =~ i_01 + i_03 + i_05 + i_07 + i_09 + i_13  ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_08 + i_12 + i_14  ## depression
  ",
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
      ## 2 correlated factors, items 7 and 10 excluded
      f1 =~ i_01 + i_03 + i_05 + i_09 + i_11 + i_13          ## anxiety
      f2 =~ i_02 + i_04 + i_06 + i_08 + i_12 + i_14          ## depression
  ",
  "dunbar_3f_cor" = "
      ## Dunbar et al., 2000, correlated factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
  ",
  "friedman_3f_cor" = "
      ## Friedman et al., 2001, 3 correlated factors
      f1 =~ i_03 + i_05 + i_09 + i_13                       ## psychic anxiety
      f2 =~ i_01 + i_07 + i_11                              ## psychomotor agitation
      f3 =~ i_02 + i_04 + i_06 + i_08 + i_10 + i_12 + i_14  ## depression
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

## add the same models again (with different names) to later define constraints:
models_cfa[["dunbar_3f_cor_constr"]] <- models_cfa[["dunbar_3f_cor"]]
models_cfa[["caci_3f_cor_constr"]] <- models_cfa[["caci_3f_cor"]]


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## define constraints for base models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## create list of empty strings with same names as models_cfa:
models_cfa_constraints_base <- rep(list(""), 
                                   length = length(models_cfa)) %>% 
  set_names(names(models_cfa))

## set constraints in semtools syntax for specific base models:
models_cfa_constraints_base[["dunbar_3f_cor_constr"]] <- "
      psi.2_1 < sqrt(abs(psi.1_1)) * sqrt(abs(psi.2_2)) * .990   ## constrain cor(f1, f2) to remain < 1
  "
models_cfa_constraints_base[["caci_3f_cor_constr"]] <- "
      psi.3_2 < sqrt(abs(psi.3_3)) * sqrt(abs(psi.2_2)) * .94    ## constraint cor(f3, f2) to remain < .95 (necessary for smaller groups, e.g. tumorart)
  "

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## define constraints for multigroup CFA models (measurement invariance)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## create list of empty strings with same names as models_cfa:
models_cfa_constraints_mi <- rep(list(""), 
                                 length = length(models_cfa)) %>% 
  set_names(names(models_cfa))

## set constraints in semtools syntax for specific multigroup CFAs:
models_cfa_constraints_mi[["dunbar_3f_cor_constr"]] <- "
      psi.2_1.g1 < sqrt(abs(psi.1_1.g1)) * sqrt(abs(psi.2_2.g1)) * .990   ## constrain cor(f1, f2) to remain < 1 (in group 1)
      psi.2_1.g2 < sqrt(abs(psi.1_1.g2)) * sqrt(abs(psi.2_2.g2)) * .990   ## constrain cor(f1, f2) to remain < 1 (in group 2)
  "
models_cfa_constraints_mi[["caci_3f_cor_constr"]] <- "
      psi.3_2.g1 < sqrt(abs(psi.3_3.g1)) * sqrt(abs(psi.2_2.g1)) * .94   ## constrain cor(f3, f2) to remain < .95 (in group 1)
      psi.3_2.g2 < sqrt(abs(psi.3_3.g2)) * sqrt(abs(psi.2_2.g2)) * .94   ## constrain cor(f3, f2) to remain < .95 (in group 2)
  "

