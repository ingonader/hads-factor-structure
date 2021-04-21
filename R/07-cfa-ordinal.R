## ######################################################################### ##
## Confirmatory Factor Analysis for Ordinal Data
## ######################################################################### ##

#' TODO:
#' * use "ridge" option? tried, but to no avail...
#'   https://stats.stackexchange.com/questions/360212/fitting-issues-when-including-certain-combination-of-variables-with-lavaancfa
#'   https://rdrr.io/cran/lavaan/man/lavOptions.html

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

## estimate one model of the list above:
wch_model <- "dunbar_3f_cor"  ## covariance matrix is not positive definite (due to sampling error), but model estimation runs fine
fit_cfa <- cfa(models_cfa[[wch_model]], data = dat_fa, estimator = "WLSMV", ordered = TRUE)
fit_cfa_summary <- summary(fit_cfa, standardized = TRUE, fit.measures = TRUE)
fit_cfa_summary$FIT
bind_rows(fit_cfa_summary$FIT)

cfa(models_cfa[[wch_model]], data = dat_fa, estimator = "WLSMV", ordered = TRUE, group = "t1_geschlecht", group.equal = c("loadings")) #%>% summary()
cfa(models_cfa[[wch_model]], data = dat_fa, estimator = "WLSMV", ordered = TRUE, group = "t1_geschlecht", group.equal = c("loadings", "thresholds")) #%>% summary()

#' relevant indicators:
#' * chisq.scaled
#' * df.scaled
#' * cfi.scaled
#' * tli.scaled
#' * rmsea.scaled

## get fit indices of all models:
res_cfa_ordinal <- models_cfa %>% {
  bind_cols(
    model = names(.),
    purrr::map_dfr(., get_fit_indices, dat_fa, estimator = "WLSMV", ordered = TRUE)
    )
}
res_cfa_ordinal %>% select(model, npar, cfi, cfi.scaled, rmsea, rmsea.scaled, srmr, status, status_msg)
res_cfa_ordinal %>% select(model, npar, cfi.scaled, rmsea.scaled, status)

## check warnings:
res_cfa_ordinal %>% filter(status != "success") %>% pull(status_msg) %>% cat()

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
fit_constrained_ordinal <- function(lavaan_str, data, group) {
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
                    ID.fac = "marker",
                    parameterization = "theta",
                    ID.cat = "millsap.tein.2004",
                    ID.thr = c(1L, 1L),
                    estimator = "WLSMV", ordered = TRUE,
                    group.equal = c("configurational")) %>%
      as.character(),
    ## weak (metric) invariance: factor laodings identical
    "metric" = lavaan_str %>% 
      measEq.syntax(data = data, group = group,
                    ID.fac = "marker",
                    parameterization = "theta",
                    ID.cat = "millsap.tein.2004",
                    ID.thr = c(1L, 1L),
                    estimator = "WLSMV", ordered = TRUE,
                    group.equal = c("loadings")) %>%
      as.character(),
    ## strong (scalar) invariance: factor loadings + item intercepts
    "scalar" = lavaan_str %>%
      measEq.syntax(data = data, group = group,
                    ID.fac = "marker",
                    parameterization = "theta",
                    ID.cat = "millsap.tein.2004",
                    ID.thr = c(1L, 1L),
                    estimator = "WLSMV", ordered = TRUE,
                    group.equal = c("loadings", "thresholds")) %>%
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
                                       estimator = "WLSMV", ordered = TRUE)
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
        purrr::map_dfr(., get_fit_indices, data, group = group, 
                       estimator = "WLSMV", ordered = TRUE)
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
      cfi_scaled_diff = cfi.scaled - lag(cfi.scaled),
      rmsea_scaled_diff = rmsea.scaled - lag(rmsea.scaled),
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
# debug(fit_constrained_ordinal)
# undebug(fit_constrained_ordinal)
# tmp <- fit_constrained_ordinal(models_cfa[[1]], data = dat_fa, group = "t1_geschlecht")
# tmp$status_msg

# ## add chisquare per df (Reduced chi-squared statistic):
# tmp <- tmp %>%
#   mutate(
#     anova_chisq_per_df = anova_chisq / anova_df
#   )

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

# ## check datum grouping:
# dat_fa$t1_datum_grp2 %>% table()

# ## test all grouping variables for specific model:
# purrr::map_dfr(
#   groups_cfa, ~ fit_constrained(models_cfa[[1]], data = dat_fa, group = .x)
# )


## define function to fit constrained models for all grouping variables in one specific model:
fit_groups_ordinal <- function(lavaan_str, data, group_cfa) {
  purrr::map_dfr(
    groups_cfa, ~ fit_constrained_ordinal(lavaan_str, data = data, group = .x)
  )
}
# tmp3 <- fit_groups_ordinal(models_cfa[[4]], data = dat_fa, group_cfa = group_cfa)
# tmp3

## fit all constrained models for all grouping variables:
res_mi_ordinal <- NULL
for (i in seq_along(models_cfa)) {
  ## estimate constrained models for determining measurement invariance for a specific factor structure:
  res_this <- purrr::map_dfr(
    models_cfa[[i]], ~ fit_groups_ordinal(.x, data = dat_fa, group_cfa = group_cfa)
  )
  ## add name of model to results data:
  res_this <- tibble(model = names(models_cfa)[i], res_this)
  ## store results:
  res_mi_ordinal <- bind_rows(
    res_mi_ordinal,
    res_this
  )
}
res_mi_ordinal %>% print(n = 50)

## check warnings:
res_mi_ordinal %>% filter(status != "success") %>% 
  group_by(status, model, group) %>% 
  count() %>% 
  select(n, everything())
res_mi_ordinal %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()

## remove p value for configurational invariance and add chisq per df (reduced chi squared statistic):
res_mi_ordinal <- res_mi_ordinal %>%
  mutate(
    anova_p = ifelse(constraint == "configurational", NA, round(anova_p, 3)),
    anova_chisq_per_df = anova_chisq / anova_df,
    chisq_diff_per_df = chisq_diff / df_diff,
    chisq_scaled_diff_per_df = chisq_scaled_diff / df_scaled_diff
  )

res_mi_ordinal %>% 
  select(model, group, grps_n, constraint, anova_chisq, anova_df, anova_chisq_per_df, anova_p) %>% 
  print(n = 200)


## ------------------------------------------------------------------------- ##
## stacked bar plot of delta-CFIs
## ------------------------------------------------------------------------- ##

## create plotting data:
dat_plot <- res_mi_ordinal %>%
  mutate(
    invariance_level = paste0(lag(constraint), "\nto ", constraint)
  ) %>% 
  filter(constraint %in% c("metric", "scalar", "strict"))

## plot delta CFI:
plot_mi_ordinal <- dat_plot %>%
  ggplot(aes(x = forcats::fct_relevel(invariance_level,
                                      "configurational\nto metric", 
                                      "metric\nto scalar"),
             #y = chisq_diff_per_df,
             #y = chisq_scaled_diff_per_df,
             y = anova_chisq_per_df,
             color = group,
             fill = group, 
             group = group)) + 
  geom_bar(position="dodge", stat="identity", width = .4) +
  #geom_hline(yintercept = 3.841, linetype = "dashed", color = "darkgrey", alpha = .8) + 
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
    y = expression(Delta*chi*"^2 per df")
  ) +
  theme_light()
plot_mi_ordinal

ggsave(filename = file.path(path_plot, "fig-mi-03-ordinal.jpg"), width = 8, height = 5, scale = 1.5, dpi = 600)


