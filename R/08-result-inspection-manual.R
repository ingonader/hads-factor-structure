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

# load(file = file.path(path_tmp, "results-all_2021-05-21--21-37-38---e91be763.Rdata"))

## ========================================================================= ##
## CFA models
## ========================================================================= ##

## inspect results:
res_cfa_mlr %>% select(model, npar, cfi, cfi.scaled, cfi.robust, rmsea, rmsea.scaled, rmsea.robust, status, status_msg)
res_cfa_mlr %>% select(model, npar, cfi.robust, rmsea.robust, status)
res_cfa_mlr %>% select(model, npar, cfi.robust, rmsea.robust, status) %>% arrange(model)

#' robust RMSEA and CFI values are computed following 
#' Brosseau-Liard, P. E., Savalei, V., and Li, L. (2012), and 
#' Brosseau-Liard, P. E. and Savalei, V. (2014); 
#' in the output of fitMeasures(), the 'new' ones are called cfi.robust and rmsea.robust, 
#' (while the 'old' ones are called cfi.scaled and rmsea.scaled)
#' -- Yves Rosseel, Sept 2016

## check warnings:
res_cfa_mlr %>% filter(status != "success") %>% pull(status_msg) %>% cat()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## re-estimate individual models via basic lavaan estimation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## estimate one model of the list above, but without adding constraints 
## and without semtools syntax:
wch_model <- "dunbar_3f_cor"
fit_cfa <- cfa(models_cfa[[wch_model]], data = dat_fa, estimator = "MLR")
fit_cfa_summary <- summary(fit_cfa, standardized = TRUE, fit.measures = TRUE)
fit_cfa_summary$FIT
fit_cfa %>% inspect(what = "cov.lv")
fit_cfa %>% inspect(what = "cov.lv") %>% det()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## re-estimate individual models as done in code (semtools syntax with constraints)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

wch_model <- "dunbar_3f_cor"
#wch_model <- "caci_3f_cor"
#wch_model <- "dunbar_3f_hier_constr"

tmp <- get_fitindices_of_model_list(
  models_cfa[wch_model], 
  model_constraints = models_cfa_constraints_base[wch_model], 
  data = dat_fa, 
  estimator = "MLR", 
  ID.fac = "auto.fix.first")
tmp
summary(tmp$fit[[1]])

## inspect this model:
fit_cfa <- tmp$fit[[1]]
fit_cfa %>% inspect(what = "cov.lv")
fit_cfa %>% inspect(what = "cov.lv") %>% det()
fit_cfa %>% inspect(what = "cov.lv") %>% eigen() %>% .$values

## check factor correlations based on estimated covariance matrix:
## re-calculate correlation from estimated covariance matrix:
covmat <- fit_cfa %>% inspect(what = "cov.lv")
covmat["f1", "f2"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f2", "f2"]))
covmat["f1", "f3"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f3", "f3"]))
covmat["f2", "f3"] / (sqrt(covmat["f2", "f2"]) * sqrt(covmat["f3", "f3"]))


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

## check results:
res_mi_mlr %>% select(model, group, grps, grps_n, constraint, npar, contains("cfi"), status) %>%
  mutate(across(contains("cfi"), ~ round(.x, 3)))

## check warnings:
res_mi_mlr %>% filter(status != "success") %>%
  group_by(status, model, group) %>%
  count() %>%
  select(n, everything())
res_mi_mlr %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()
res_mi_mlr %>% filter(status != "success") %>%
  mutate(status_msg = status_msg %>%
           stringr::str_replace_all(".*lavaan WARNING: ", "") %>%
           stringr::str_replace_all('use lavInspect.*$', "") %>%
           stringr::str_replace_all("[[:blank:][:cntrl:]]+", " ")) %>%
  group_by(model, group, status_msg) %>% count()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## inspect one of the fitted models:
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

res_mi_mlr %>% 
  filter(
    model == "dunbar_3f_hier_constr",
    group == "t1_geschlecht"
  ) %>%
  select(model, group, grps, grps_n, constraint, npar, contains("cfi"), status)

fit_cfa <- res_mi_mlr %>% 
  filter(
    model == "dunbar_3f_hier_constr",
    group == "t1_geschlecht",
    constraint == "strict"
  ) %>%
  pull(fit) %>% .[[1]]

summary(fit_cfa)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## re-estimate individual MG-CFA models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

wch_model <- "dunbar_3f_cor_constr"
#wch_model <- "dunbar_3f_cor"
#wch_model <- "dunbar_3f_hier_constr"

tmp <- fit_constrained_mlr(
  models_cfa[[wch_model]],
  model_constraints_base = models_cfa_constraints_base[[wch_model]],
  model_constraints_mi = models_cfa_constraints_mi[[wch_model]],
  data = dat_fa, group = "tumorart")
tmp %>% select(group, grps, grps_n, constraint, npar, status)
tmp %>% select(group, grps, grps_n, constraint, npar, status, status_msg)

## select only models with warnings:
tmp_warn <- tmp %>% filter(status != "success")

## of those, select a specific model for inspection:
wch_warn <- 1

## inspect this model:
fit_cfa <- tmp_warn$fit[[wch_warn]]
fit_cfa %>% inspect(what = "cov.lv")
fit_cfa %>% inspect(what = "cov.lv") %>% det()
fit_cfa %>% inspect(what = "cov.lv") %>% eigen() %>% .$values

## check factor correlations based on estimated covariance matrix:
## re-calculate correlation from estimated covariance matrix:
covmat_raw <- tmp_warn$fit[[wch_warn]] %>% inspect(what = "cov.lv")
covmat <- covmat_raw
#covmat <- covmat_raw[[1]]
covmat["f1", "f2"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f2", "f2"]))
covmat["f1", "f3"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f3", "f3"]))
covmat["f2", "f3"] / (sqrt(covmat["f2", "f2"]) * sqrt(covmat["f3", "f3"]))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## re-estimate individual MG-CFA models with different constraints
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## select model:
wch_model <- "dunbar_3f_hier"

## add semtools syntax:
models_cfa[[wch_model]] %>%
  measEq.syntax(data = dat_fa, ID.fac = "auto.fix.first") %>%
  as.character() %>% cat()

## estimate model with different constraints (for experimentation):
tmp <- fit_constrained_mlr(
  models_cfa[[wch_model]],
  model_constraints_base = "
  ",
  model_constraints_mi = "
      psi.1_1.g1 > 0.0000001   ## constrain factor variance: needs to be > 1 (in grp 1)
      psi.1_1.g2 > 0.0000001   ## constrain factor variance: needs to be > 1 (in grp 2)
  ",
  data = dat_fa, group = "t1_geschlecht"
)
tmp$status
tmp$status_msg
fit_cfa <- tmp$fit[[6]]
fit_cfa %>% summary()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check warnings
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## warning messages:
res_mi_mlr %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()

## overview of warnings messages:
res_mi_mlr %>% filter(status != "success") %>%
  group_by(status, model, group) %>%
  count() %>%
  select(n, everything())

## inspect Caci's model:
wch_model_name <- "caci_3f_cor"
## insepct Dunbar's hierarchical model:
wch_model_name <- "dunbar_3f_hier"
wch_model <- which(names(models_cfa) == wch_model_name)
tmp <- res_mi_mlr %>% 
  filter(status != "success",
         model == wch_model_name)

## select a model with warnings:
wch_tmp <- 1

## model summary:
tmp$fit[[wch_tmp]] %>% summary()

## covariance matrix:
tmp$fit[[wch_tmp]] %>% inspect(what = "cov.lv")
tmp$fit[[wch_tmp]] %>% inspect(what = "cov.lv") %>% det()
tmp$fit[[wch_tmp]] %>% inspect(what = "cov.lv") %>% lapplyiflist(., as.vector)

## check factor correlations based on estimated covariance matrix:
## re-calculate correlation from estimated covariance matrix:
covmat_raw <- tmp$fit[[wch_tmp]] %>% inspect(what = "cov.lv")
covmat <- covmat_raw
#covmat <- covmat_raw[[1]]
covmat["f1", "f2"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f2", "f2"]))
covmat["f1", "f3"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f3", "f3"]))
covmat["f2", "f3"] / (sqrt(covmat["f2", "f2"]) * sqrt(covmat["f3", "f3"]))


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


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## re-fit specific models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## re-fit specific model for all groups:
mod_refit <- "
      ## Dunbar et al., 2000, correlated factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
      f1 ~~ var_f1 * f1  ## introduce parameter for variance of factor 1
      f2 ~~ var_f2 * f2  ## introduce parameter for variance of factor 2
      f1 ~~ c01 * f2                  ## constrain correlation of f1 and f2...
      c01 < sqrt(var_f1) * sqrt(var_f2) * .990    ## .. to remain smaller than one, to avoid heywood case
"
# mod_refit_grp <- "
#       ## Dunbar et al., 2000, correlated factors, item 7 loads to 2 factors
#       f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
#       f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
#       f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
#       f1 ~~ c(g1_var_f1, g2_var_f1) * f1  ## introduce parameter for variance of factor 1
#       f2 ~~ c(g1_var_f2, g2_var_f2) * f2  ## introduce parameter for variance of factor 2
#       f1 ~~ c(g1_c01, g2_c01) * f2                  ## constrain correlation of f1 and f2...
#       g1_c01 < sqrt(g1_var_f1) * sqrt(g1_var_f2) * .990    ## .. to remain smaller than one, to avoid heywood case
#       g2_c01 < sqrt(g2_var_f1) * sqrt(g2_var_f2) * .990    ## .. to remain smaller than one, to avoid heywood case
# "
# mod_constraints_grp <- "
#       f1 ~~ c(g1_var_f1, g2_var_f1) * f1  ## introduce parameter for variance of factor 1
#       f2 ~~ c(g1_var_f2, g2_var_f2) * f2  ## introduce parameter for variance of factor 2
#       f1 ~~ c(g1_c01, g2_c01) * f2                  ## constrain correlation of f1 and f2...
#       g1_c01 < sqrt(g1_var_f1) * sqrt(g1_var_f2) * .990    ## .. to remain smaller than one, to avoid heywood case
#       g2_c01 < sqrt(g2_var_f1) * sqrt(g2_var_f2) * .990    ## .. to remain smaller than one, to avoid heywood case
# "
group <- groups_cfa[1]
## raw model without constraints:
mod_refit_raw <- "
      ## Dunbar et al., 2000, correlated factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
"
## constraints in semTools nomenclature:
mod_refit_constraints_grp_semtools <- "
      psi.2_1.g1 < sqrt(psi.1_1.g1) * sqrt(psi.2_2.g1) * .990   ## constrain cor(f1, f2) to remain < 1 (in group 1)
      psi.2_1.g2 < sqrt(psi.1_1.g2) * sqrt(psi.2_2.g2) * .990   ## constrain cor(f1, f2) to remain < 1 (in group 2)
"
## add semtools syntax to raw model:
mod_refit_grp <- mod_refit_raw %>%
  measEq.syntax(data = dat_fa, group = group,
                ID.fac = "auto.fix.first",
                group.equal = c("configurational")) %>%
  as.character()
## ad constraints:
mod_refit_grp <- paste0(mod_refit_grp,
                        mod_refit_constraints_grp_semtools)
cat(mod_refit_grp)

#' NOTE: c01 < .990 seems to be enough for groupwise models

## fit one model:
fit_cfa <- cfa(mod_refit, data = dat_fa, estimator = "MLR", std.lv = FALSE)
fit_cfa <- cfa(mod_refit_grp, data = dat_fa, estimator = "MLR", group = group, std.lv = FALSE)
summary(fit_cfa, fit.measures = TRUE)
fit_cfa %>% inspect(what = "cov.lv")
## re-calculate correlation from estimated covariance matrix:
covmat_raw <- fit_cfa %>% inspect(what = "cov.lv")
covmat <- covmat_raw[[1]]
covmat["f1", "f2"] / (sqrt(covmat["f1", "f1"]) * sqrt(covmat["f2", "f2"]))

## fit all group models:
#dat_refit <- fit_groups_mlr(mod_refit_grp, data = dat_fa, group_cfa = group_cfa, ID.fac = "auto.fix.first")
dat_refit <- fit_groups_mlr(mod_refit, data = dat_fa, group_cfa = group_cfa, ID.fac = "auto.fix.first")
dat_refit$status
dat_refit %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()

dat_refit %>% filter(status != "success") %>% pull(fit) %>% .[[3]] %>% summary()
dat_refit %>% filter(status != "success") %>% pull(fit) %>% .[[3]] %>% inspect("cov.lv")

#' New Warning: (16 may 2021):
#' Warning messages:
#' 1: In lavaanify(model = FLAT, constraints = constraints,  ... :
#' lavaan WARNING: using a single label per parameter in a multiple group
#' setting implies imposing equality constraints across all the groups;
#' If this is not intended, either remove the label(s), or use a vector
#' of labels (one for each group);
#' See the Multiple groups section in the man page of model.syntax.

## overview of warnings messages::
dat_refit %>% filter(status != "success") %>%
  group_by(status, group, constraint) %>%
  count() %>%
  select(n, everything())

## original warning messages:
res_mi_mlr %>% 
  filter(status != "success",
         model == "dunbar_3f_cor") %>%
  group_by(status, model, group) %>%
  count() %>%
  select(n, everything())



## ========================================================================= ##
## experimentation
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## try different model specifications
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# mod_tmp <- "## Dunbar et al., 2000, hierarchical factors, item 7 loads to 2 factors
#       f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
#       f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
#       f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
#       f2 =~ f1 + f3     ## NA explains AA and AD
#       f1 ~~ 0*f3  
# "
# mod_tmp <- "## Dunbar et al., 2000, hierarchical factors, item 7 loads to 2 factors
#       f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
#       f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
#       f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
#       f1 ~ f2
#       f3 ~ f2     ## NA explains AA and AD
#       f1 ~~ 0*f3  
# "
# fit_cfa <- cfa(mod_tmp, data = dat_fa, estimator = "MLR", std.lv = TRUE)
# fit_cfa %>% inspect(what = "cov.lv")
# fit_cfa %>% summary()
# 
# ## estimate one model with constraints:
# tmp_model_def <- models_cfa[["dunbar_3f_cor"]]
# tmp_model_def <- paste0(
#   models_cfa[["dunbar_3f_cor"]],
#   "f1 ~~ c01 * f2",  "\n",
#   "c01 < .99", "\n"
# )
# cat(tmp_model_def)
# #fit_cfa <- cfa(tmp_model_def, data = dat_fa, estimator = "MLR")
# fit_ind <- get_fit_indices(tmp_model_def, data = dat_fa, estimator = "MLR")
# fit_ind
# fit_ind$status
# fit_cfa <- fit_ind$fit[[1]]
# fit_cfa %>% inspect(what = "cov.lv")
# fit_cfa %>% inspect(what = "cov.lv") %>% det()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## export 3-factor model with constraint but no additional specification
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

mod_exp <- "
      ## Dunbar et al., 2000, correlated factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
      f1 ~~ c01 * f2     ## constrain covariance (== correlation, if std.lv = TRUE) of f1 and f2...
      c01 < .995          ## .. to remain smaller than one, to avoid heywood case
"
fit_cfa <- cfa(mod_exp, data = dat_fa, estimator = "MLR", std.lv = TRUE)
# summary(fit_cfa, fit.measures = TRUE)
# fit_cfa %>% inspect(what = "cov.lv")

## construct dataset for export:
dat_export <- as_tibble(lavPredict(fit_cfa)) %>%
  bind_cols(
    dat_fa[groups_cfa]
  )

save(mod_exp, fit_cfa, groups_cfa, dat_export,
     file = file.path(path_tmp, "dunbar_3f_cor_constrained.Rdata"))

