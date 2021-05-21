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
## CFA models
## ========================================================================= ##

# fit1 <- res_cfa_mlr$fit[[7]]
# fit <- res_cfa_mlr$fit[[2]]
# fit2 <- res_mi_mlr$fit[[3]]
get_loadings_summary <- function(fit) {
  ## get list of loadings, theta, psi (or list of lists):
  loadings_and_stuff <- fit %>% inspect(what = "std") 
  
  ## check if result is from CFA (depth 2) or MGCFA (depth 3):
  if (purrr::vec_depth(loadings_and_stuff) == 2) {
    loadings_with_zero <- loadings_and_stuff$lambda
  }
  else {
    loadings_with_zero <- loadings_and_stuff %>% 
      lapply(., function(i) i$lambda)    
  }
  ## get rid of zero loadings in this matrix / list of matrices:
  loadings_range <- lapplyiflist(loadings_with_zero, function(x) x[x != 0]) %>% 
    range(na.rm = TRUE)
  ## return range of loadings (or range of list of loadings):
  return(loadings_range)
}
# get_loadings_summary(fit1)
# get_loadings_summary(fit2)

get_factor_var_summary <- function(fit) {
  ## get factor variances (in covariance matrix diagonal), 
  ## for one group of (list of) multiple groups (via lapply):
  fact_var <- fit %>% inspect(what = "cov.lv") %>% lapplyiflist(., diag)
  ## extract range (of vector or list):
  return(range(fact_var))
}
# get_factor_var_summary(fit1)
# get_factor_var_summary(fit2)

get_factor_covar_summary <- function(fit) {
  ## get factor variance-covariance matrix (or list, if MGCFA):
  fact_covar <- fit %>% inspect(what = "cov.lv")
  ## check if only one factor:
  if (firstiflist(fact_covar, nrow) == 1)
    return(NA)
  ## else, extract lower triangle:
  lowtri <- lapplyiflist(fact_covar, 
                         function(x) x[lower.tri(x)])
  ## and return range of (list of) lower triangles:
  return(range(lowtri, na.rm = TRUE))
}
# get_factor_covar_summary(fit1)
# get_factor_covar_summary(fit2)

get_error_var_summary <- function(fit) {
  ## get all parameter estimates:
  fit %>% parameterEstimates() %>% 
    ## keep only error variances (of items) and variances of factors:
    filter(lhs == rhs) %>%
    ## remove factor estimates:
    filter(!grepl("f[0-9]+", lhs)) %>%
    ## return range:
    {range(.$est, na.rm = TRUE)}
}
# get_error_var_summary(fit1)
# get_error_var_summary(fit2)

# fit <- res_cfa_mlr$fit[[7]]
# fit <- res_cfa_mlr$fit[[2]]
# fit <- res_mi_mlr$fit[[3]]
get_cor_resid_summary <- function(fit) {
  ## get correlation residuals (list, or list of lists):
  cor_resid_summary <- fit %>% resid(type = "cor") 
  
  ## check if result is from CFA (depth 2) or MGCFA (depth 3):
  if (purrr::vec_depth(cor_resid_summary) == 2) {
    ## get covariances:
    cor_resid_cov <- cor_resid_summary$cov
  }
  else {
    ## get covariances from list of summaries:
    cor_resid_cov <- lapplyiflist(cor_resid_summary, 
                                  function(i) i$cov) 
  }
  ## and return range (of list or single cov matrix):
  return(range(cor_resid_cov, na.rm = TRUE))
}
# get_cor_resid_summary(fit1)
# get_cor_resid_summary(fit2)

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
# res_cfa_mlr_ext %>% select(model, npar, cfi.robust, rmsea.robust, status, 
#                            contains("load_"), contains("fact_"),
#                            contains("error_"), contains("cor_resid_"))

## extend MGCFA (MI) models for MLR estimation:
res_mi_mlr_ext <- res_mi_mlr %>%
  extend_cfa_parameter_summary()
# res_mi_mlr_ext %>% select(1:5, status, 
#                           contains("load_"), contains("fact_"),
#                           contains("error_"), contains("cor_resid_"))

# save.image(file = file.path(path_tmp, "results-all_2021-05-04--21-03-00---f25fcea3---extended.Rdata"))


  