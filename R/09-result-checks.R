## ######################################################################### ##
## Automated Checks of Results
## ######################################################################### ##

# rm(list = ls())
# rstudioapi::restartSession()

#' NOTE:
#' This script assumes that the following scripts have been run:
#' * setup scripts
#' * data load scripts
#' * data preparation scripts
#' * EFA
#' * all CFA scripts

source("./R/setup.R")

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

library(lavaan)

## ========================================================================= ##
## load data dump from disk
## ========================================================================= ##

load(file = file.path(path_tmp, "results-all_2021-04-26--20-24-12---f25fcea3.Rdata"))

## ========================================================================= ##
## CFA models
## ========================================================================= ##

get_loadings_summary <- function(fit) {
  loadings_with_zero <- fit %>% inspect(what = "std") %>% .$lambda
  loadings_range <- loadings_with_zero[loadings_with_zero != 0] %>% range(na.rm = TRUE)
  return(loadings_range)
}

#fit <- res_cfa_mlr$fit[[7]]
#fit <- res_cfa_mlr$fit[[2]]
fit <- res_mi_mlr$fit[[3]]
get_factor_var_summary <- function(fit) {
  fact_var <- fit %>% inspect(what = "cov.lv") %>% diag()
  return(range(fact_var))
}

get_factor_covar_summary <- function(fit) {
  fact_covar <- fit %>% inspect(what = "cov.lv")
  ## check if only one factor:
  if (nrow(fact_covar) == 1)
    return(NA)
  ## else, return lower triangle range:
  return(range(fact_covar[lower.tri(fact_covar)], na.rm = TRUE))
}
#get_factor_covar_summary(fit)

get_error_var_summary <- function(fit) {
  fit %>% parameterEstimates() %>% filter(lhs == rhs) %>%
    filter(!grepl("f[0-9]+", lhs)) %>%
    {range(.$est, na.rm = TRUE)}
}

get_cor_resid_summary <- function(fit) {
  fit %>% resid(type = "cor") %>% .$cov %>% range(na.rm = TRUE)
}

extend_cfa_parameter_summary <- function(res_cfa) {
  res_cfa %>% mutate(
    load_min = purrr::map_dbl(fit, ~ get_loadings_summary(.x)[1]),
    load_max = purrr::map_dbl(fit, ~ get_loadings_summary(.x)[2]),
    fact_var_min = purrr::map_dbl(fit, ~ get_factor_var_summary(.x)[1]),
    fact_var_max = purrr::map_dbl(fit, ~ get_factor_var_summary(.x)[2]),
    fact_covar_min = purrr::map_dbl(fit, ~ get_factor_covar_summary(.x)[1]),
    fact_covar_max = purrr::map_dbl(fit, ~ get_factor_covar_summary(.x)[2]),
    error_var_min = purrr::map_dbl(fit, ~ get_error_var_summary(.x)[1]),
    error_var_max = purrr::map_dbl(fit, ~ get_error_var_summary(.x)[2]),
    cor_resid_min = purrr::map_dbl(fit, ~ get_cor_resid_summary(.x)[1]),
    cor_resid_max = purrr::map_dbl(fit, ~ get_cor_resid_summary(.x)[2])
  )
}

## extend CFA results for MLR estimation:
res_cfa_mlr_ext <- res_cfa_mlr %>% 
  extend_cfa_parameter_summary()
res_cfa_mlr_ext %>% select(model, npar, cfi.robust, rmsea.robust, status, 
                           contains("load_"), contains("fact_"),
                           contains("error_"), contains("cor_resid_"))

## extend CFA results for WLSMV estimation:
res_cfa_ordinal_ext <- res_cfa_ordinal %>% 
  extend_cfa_parameter_summary()
res_cfa_ordinal_ext %>% select(model, npar, cfi.scaled, rmsea.scaled, status, 
                           contains("load_"), contains("fact_"),
                           contains("error_"), contains("cor_resid_"))

## TODO:
## extend MGCFA (MI) models


