## ######################################################################### ##
## Confirmatory Factor Analysis with robust MLR estimation
## ######################################################################### ##

# rm(list = ls())
# rstudioapi::restartSession()

#' NOTE:
#' This script assumes that the following scripts have been run:
#' * setup scripts
#' * data load scripts
#' * data preparation scripts

source("./R/setup.R")
source(file.path(path_r, "05-cfa-model-def.R"))  # currently also sources data load and data prep scripts

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

library(lavaan)


## ========================================================================= ##
## confirmatory factor analysis
## ========================================================================= ##

# ## estimate one model of the list above:
# wch_model <- "dunbar_3f_cor"  ## covariance matrix is not positive definite (due to sampling error), but model estimation runs fine
# wch_model <- 1  ## covariance matrix is not positive definite (due to sampling error), but model estimation runs fine
# fit_cfa <- cfa(models_cfa[[wch_model]], data = dat_fa, estimator = "MLR")
# fit_cfa_summary <- summary(fit_cfa, standardized = TRUE, fit.measures = TRUE)
# fit_cfa_summary$FIT
# fit_cfa %>% inspect(what = "cov.lv")
# bind_rows(fit_cfa_summary$FIT)

## get fit indices of all models:
res_cfa_mlr <- models_cfa %>% {
  bind_cols(
    model = names(.),
    purrr::map_dfr(., get_fit_indices, dat_fa, estimator = "MLR")
    )
}
# res_cfa_mlr %>% select(model, npar, cfi, cfi.scaled, cfi.robust, rmsea, rmsea.scaled, rmsea.robust, status, status_msg)
# res_cfa_mlr %>% select(model, npar, cfi.robust, rmsea.robust, status)

#' robust RMSEA and CFI values are computed following 
#' Brosseau-Liard, P. E., Savalei, V., and Li, L. (2012), and 
#' Brosseau-Liard, P. E. and Savalei, V. (2014); 
#' in the output of fitMeasures(), the 'new' ones are called cfi.robust and rmsea.robust, 
#' (while the 'old' ones are called cfi.scaled and rmsea.scaled)
#' -- Yves Rosseel, Sept 2016

# ## check warnings:
# res_cfa_mlr %>% filter(status != "success") %>% pull(status_msg) %>% cat()


## ========================================================================= ##
## measurement invariance
## ========================================================================= ##

library(semTools)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## measurement invariance for specified variables: function definition
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# ## for debugging:
# lavaan_str <- models_cfa[[1]]
# data <- dat_fa
# group <- "t1_alter_grp2"

## define function to estimate a grouped model with all levels of constraints:
fit_constrained_mlr <- function(lavaan_str, data, group) {
  ## remove missings in grouping variable:
  group_miss <- is.na(data[[group]])
  data <- data[!group_miss, ]
  ## get number of groups from data:
  grps <- data[[group]] %>% unique()
  n_grps <- grps %>% length()
  ## get sample size of each group as string:
  grps_n <- data[[group]] %>% 
    table(useNA = "if") %>%
    paste(collapse = ", ") %>%
    paste0("[", ., "]")
  ## define list of all model definitions:
  models_constrained <- list(
    ## configurational invariance: items load on same factors:
    "configurational" = lavaan_str %>%
      measEq.syntax(data = data, group = group,
                    ID.fac = "auto.fix.first",
                    group.equal = c("configurational")) %>%
      as.character(),
    ## weak (metric) invariance: factor laodings identical
    "metric" = lavaan_str %>% 
      measEq.syntax(data = data, group = group,
                    ID.fac = "auto.fix.first",
                    group.equal = c("loadings")) %>%
      as.character(),
    ## strong (scalar) invariance: factor loadings + item intercepts
    "scalar" = lavaan_str %>%
      measEq.syntax(data = data, group = group,
                    ID.fac = "auto.fix.first",
                    group.equal = c("loadings", "intercepts")) %>%
      as.character(),
    ## strict invariance: factor loadings + item intercepts + residual variances
    "strict" = lavaan_str %>%
      measEq.syntax(data = data, group = group,
                    ID.fac = "auto.fix.first",
                    group.equal = c("loadings", "intercepts", "residuals")) %>%
      as.character()
  )
  ## first, fit model in each group separately:
  fit_cfa_group <- seq_along(grps) %>% {
    bind_cols(
      group = group,
      grps = n_grps,
      grps_n = grps_n,
      constraint = paste0("group ", ., ": ", grps[.]),
      purrr::map_dfr(.,
        ~ get_fit_indices(lavaan_str, 
                          data = data %>% filter((!!as.name(group)) == grps[.x]),
                          group = NULL, 
                          estimator = "MLR")
      )
    )
  }
  ## fit models and get fit indices:
  ## NOTE: std.lv will be ignored if ID.fac is specified above
  fit_cfa_constrained <- models_constrained %>% 
    {
      bind_cols(
        group = group,
        grps = n_grps,
        grps_n = grps_n,
        constraint = names(.),
        purrr::map_dfr(., get_fit_indices, data, group = group, estimator = "MLR")
      )
    }
  ## add differences of constrained models:
  fit_cfa_constrained <- fit_cfa_constrained %>%
    mutate(
      anova_diff = purrr::map2(fit, lag(fit), anova),
      anova_chisq = purrr::map_dbl(anova_diff, ~ .x$`Chisq diff`[2]),
      anova_df = purrr::map_dbl(anova_diff, ~ .x$`Df diff`[2]),
      anova_chisq_per_df = anova_chisq / anova_df,
      anova_p = purrr::map_dbl(anova_diff, ~ .x$`Pr(>Chisq)`[2]),
      chisq_diff = chisq - lag(chisq),
      df_diff = df - lag(df),
      chisq_scaled_diff = chisq.scaled - lag(chisq.scaled),
      df_scaled_diff = df.scaled - lag(df.scaled),
      cfi_diff = cfi - lag(cfi),
      cfi_robust_diff = cfi.robust - lag(cfi.robust),
      rmsea_diff = rmsea - lag(rmsea),
      rmsea_robust_diff = rmsea.robust - lag(rmsea.robust),
      srmr_diff = srmr - lag(srmr)
    )
  ## combine group fits and contrained fits:
  varnames_diffvars_nonconvert <- "anova_diff"
  varnames_diffvars <- setdiff(names(fit_cfa_constrained), c(names(fit_cfa_group), varnames_diffvars_nonconvert))
  fit_cfa_constrained <- fit_cfa_constrained %>% 
    mutate_at(vars(varnames_diffvars), as.numeric)
  fit_cfa_group[, c(varnames_diffvars, varnames_diffvars_nonconvert)] <- NA
  fit_cfa_all <- bind_rows(
    fit_cfa_group,
    fit_cfa_constrained
  )
  return(fit_cfa_all)
}
# debug(fit_constrained_mlr)
# undebug(fit_constrained_mlr)
# fit_constrained_mlr(models_cfa[[1]], data = dat_fa, group = "t1_geschlecht")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## measurement invariance for specified variables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## level 1: models
## level 2: groups
## level 3: constraints

## define variables that should be used for 
groups_cfa <- c(
  "t1_geschlecht", 
  "t1_alter_grp2", 
  "t1_datum_grp2", 
  "tumorart"
)

## define function to fit constrained models for all grouping variables in one specific model:
fit_groups_mlr <- function(lavaan_str, data, group_cfa) {
  purrr::map_dfr(
    groups_cfa, ~ fit_constrained_mlr(lavaan_str, data = data, group = .x)
  )
}
# tmp3 <- fit_groups_mlr(models_cfa[[4]], data = dat_fa, group_cfa = group_cfa)
# tmp3

## fit all constrained models for all grouping variables:
res_mi_mlr <- NULL
for (i in seq_along(models_cfa)) {
  ## estimate constrained models for determining measurement invariance for a specific factor structure:
  res_this <- purrr::map_dfr(
    models_cfa[[i]], ~ fit_groups_mlr(.x, data = dat_fa, group_cfa = group_cfa)
  )
  ## add name of model to results data:
  res_this <- tibble(model = names(models_cfa)[i], res_this)
  ## store results:
  res_mi_mlr <- bind_rows(
    res_mi_mlr,
    res_this
  )
}
# res_mi_mlr %>% print(n = 50)

# ## check warnings:
# res_mi_mlr %>% filter(status != "success") %>%
#   group_by(status, model, group) %>%
#   count() %>%
#   select(n, everything())
# res_mi_mlr %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()

## ------------------------------------------------------------------------- ##
## stacked bar plot of delta-CFIs
## ------------------------------------------------------------------------- ##

## create plotting data:
dat_plot <- res_mi_mlr %>%
  mutate(
    invariance_level = paste0(lag(constraint), "\nto ", constraint)
  ) %>% 
  filter(constraint %in% c("metric", "scalar", "strict"))

## plot delta CFI:
plot_mi_mlr <- dat_plot %>%
  ggplot(aes(x = forcats::fct_relevel(invariance_level,
                                      "configurational\nto metric", 
                                      "metric\nto scalar", 
                                      "scalar\nto strict"),
             y = cfi_robust_diff, 
             color = group,
             fill = group, 
             group = group)) + 
  geom_bar(position="dodge", stat="identity", width = .4) +
  geom_hline(yintercept = -0.01, linetype = "dashed", color = "darkgrey", alpha = .8) + 
  scale_fill_discrete(
    name = "Grouping Variable",
    breaks = c("t1_alter_grp2", "t1_datum_grp2", "t1_geschlecht", "tumorart"),
    labels = c("Age", "Time of reponse", "Sex", "Tumor type")
  ) +
  scale_color_discrete(
    name = "Grouping Variable",
    breaks = c("t1_alter_grp2", "t1_datum_grp2", "t1_geschlecht", "tumorart"),
    labels = c("Age", "Time of reponse", "Sex", "Tumor type")
  ) +
  facet_wrap(vars(model)) +
  labs(
    x = "",
    y = expression(Delta*"CFI (robust)")
  ) +
  theme_light()
plot_mi_mlr

ggsave(filename = file.path(path_plot, "fig-mi-02-mlr.jpg"), width = 8, height = 5, scale = 1.5, dpi = 600)


## ========================================================================= ##
## experimentation
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## try different model specifications
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

mod_tmp <- "## Dunbar et al., 2000, hierarchical factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
      f2 =~ f1 + f3     ## NA explains AA and AD
      f1 ~~ 0*f3  
"
mod_tmp <- "## Dunbar et al., 2000, hierarchical factors, item 7 loads to 2 factors
      f1 =~ i_03 + i_09 + i_13                                     ## autonomic anxiety
      f2 =~ i_01 + i_05 + i_07 + i_11                              ## neg. affectivigy (NA)
      f3 =~ i_02 + i_04 + i_06 + i_07 + i_08 + i_10 + i_12 + i_14  ## anhedonicstic depression
      f1 ~ f2
      f3 ~ f2     ## NA explains AA and AD
      f1 ~~ 0*f3  
"
fit_cfa <- cfa(mod_tmp, data = dat_fa, estimator = "MLR", std.lv = TRUE)
fit_cfa %>% inspect(what = "cov.lv")
fit_cfa %>% summary()

## estimate one model with constraints:
tmp_model_def <- models_cfa[["dunbar_3f_cor"]]
tmp_model_def <- paste0(
  models_cfa[["dunbar_3f_cor"]],
  "f1 ~~ c01 * f2",  "\n",
  "c01 < .99", "\n"
)
cat(tmp_model_def)
#fit_cfa <- cfa(tmp_model_def, data = dat_fa, estimator = "MLR")
fit_ind <- get_fit_indices(tmp_model_def, data = dat_fa, estimator = "MLR")
fit_ind
fit_ind$status
fit_cfa <- fit_ind$fit[[1]]
fit_cfa %>% inspect(what = "cov.lv")
fit_cfa %>% inspect(what = "cov.lv") %>% det()

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
summary(fit_cfa, fit.measures = TRUE)
fit_cfa %>% inspect(what = "cov.lv")

## construct dataset for export:
dat_export <- as_tibble(lavPredict(fit_cfa)) %>%
  bind_cols(
    dat_fa[groups_cfa]
  )

save(mod_exp, fit_cfa, groups_cfa, dat_export,
     file = file.path(path_tmp, "dunbar_3f_cor_constrained.Rdata"))

