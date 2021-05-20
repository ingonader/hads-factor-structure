## ######################################################################### ##
## Main file
## ######################################################################### ##

## clear environment:
# rm(list = ls())
# rstudioapi::restartSession()

## ========================================================================= ##
## run full analysis
## ========================================================================= ##

## setup R environment:
source("./R/setup.R")
# source(file.path(path_r, "01-data-load-raw.R"))
# source(file.path(path_r, "02-data-prep.R"))

## source CFA model definitions:
## (which currently also sources data load and dat aprep)
source(file.path(path_r, "05-cfa-model-def.R"))  # currently also sources data load and data prep scripts

## run EFA:
source(file.path(path_r, "04-efa.R"))

## run CFA:
source(file.path(path_r, "06b-cfa-mlr-est.R"))

## run model summary checks:
source(file.path(path_r, "09-result-checks.R"))

## dump results to disk:
save.image(construct_filename(path_tmp, "results-all", "Rdata"))


