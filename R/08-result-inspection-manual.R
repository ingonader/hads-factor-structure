## ######################################################################### ##
## Manual Inspection of Results
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check warnings
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ------------------------------------------------------------------------- ##
## MLR estimation
## ------------------------------------------------------------------------- ##

## models and their status:
res_cfa_mlr %>% 
  select(model, npar, cfi.robust, rmsea.robust, status) %>%
  mutate_at(c("cfi.robust", "rmsea.robust"), ~ round(.x, 3))

## Warning messages:
res_cfa_mlr %>% filter(status != "success") %>% pull(status_msg) %>% cat()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## inspect a specific model
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## inspect the dunbar model:
wch_model <- 6; print(names(models_cfa)[wch_model])

## summary with all most parameter estimates:
res_cfa_mlr$fit[[wch_model]] %>% summary()

## factor covariances and correlations for MLR:
res_cfa_mlr$fit[[wch_model]] %>% inspect(what = "cov.lv")
res_cfa_mlr$fit[[wch_model]] %>% inspect(what = "cov.lv") %>% det()
res_cfa_mlr$fit[[wch_model]] %>% inspect(what = "cor.lv")

## inspect loadings:
loadings_mlr <- res_cfa_mlr$fit[[wch_model]] %>% inspect(what = "std") %>% .$lambda
range(loadings_mlr[loadings_mlr > 0]) %>% round(2)

## inspect error variances:
res_cfa_mlr$fit[[wch_model]] %>% summary()
res_cfa_mlr$fit[[wch_model]] %>% parameterEstimates() %>% filter(lhs == rhs)

## correlation residuals:
## inspect correlation residuals to see whether model fails to 
## capture some observed relationships:
res_cfa_mlr$fit[[wch_model]] %>% resid(type = "cor")
res_cfa_mlr$fit[[wch_model]] %>% resid(type = "cor") %>% .$cov %>% max()

# ## residual covariances:
# res_cfa_mlr$fit[[wch_model]] %>% resid() %>% .$cov

## inspect modification indices:
res_cfa_mlr$fit[[wch_model]] %>%
  modificationIndices() %>% 
  arrange(desc(mi))

## ========================================================================= ##
## Measurement Invariance
## ========================================================================= ##

## warning messages:
res_mi_mlr %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()

## overview:
res_mi_mlr %>% filter(status != "success") %>%
  group_by(status, model, group) %>%
  count() %>%
  select(n, everything())

## inspect Caci's model:
wch_model_name <- "caci_3f_cor"
wch_model <- which(names(models_cfa) == wch_model_name)
tmp <- res_mi_mlr %>% 
  filter(status != "success",
         model == wch_model_name)

## covariance matrix:
wch_tmp <- 2
tmp$fit[[wch_tmp]] %>% inspect(what = "cov.lv")
tmp$fit[[wch_tmp]] %>% inspect(what = "cov.lv") %>% lapplyiflist(., as.vector)

## loadings:
tmp$fit[[wch_tmp]] %>% inspect(what = "std") %>% lapplyiflist(., function(i) i$lambda)
bind_cols(
  tmp$fit[[wch_tmp]] %>% inspect(what = "std") %>% lapplyiflist(., function(i) i$lambda),
  res_cfa_mlr$fit[[wch_model]] %>% inspect(what = "std") %>% .$lambda
)

## correlation residuals:
tmp$fit[[wch_tmp]] %>% resid(type = "cor") %>% lapplyiflist(., function(i) i$cov)
tmp$fit[[wch_tmp]] %>% resid(type = "cor") %>% lapplyiflist(., function(i) i$cov) %>% lapplyiflist(max)

## error variances:
tmp$fit[[wch_tmp]] %>% parameterEstimates() %>% filter(lhs == rhs)
tmp$fit[[wch_tmp]] %>% parameterEstimates() %>% filter(lhs == rhs) %>% {range(.$est)}

## modifcation indices:
tmp$fit[[wch_tmp]] %>% modificationIndices() %>% arrange(desc(mi)) %>% head(n = 10)


# ## non-warning model for comparison:
# wch_model_success <- 0
# wch_model_success <- wch_model_success + 1
# res_mi_mlr %>% filter(status == "success") %>% 
#   pull(fit) %>% 
#   .[[wch_model_success]] %>% 
#   inspect(what = "cov.lv")






