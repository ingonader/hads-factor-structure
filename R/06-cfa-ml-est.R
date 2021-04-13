## ######################################################################### ##
## Confirmatory Factor Analysis with standard ML estimation
## ######################################################################### ##

#' TODO:
#' * also add chisq-test between nested models, e.g. with semTools:::difftest()

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


## get fit indices of all models:
res <- models_cfa %>% {
  bind_cols(
    model = names(.),
    purrr::map_dfr(., get_fit_indices, dat_fa)
    )
}
res %>% select(model, npar, cfi, aic, bic, rmsea, status, status_msg)
res %>% select(model, npar, cfi, rmsea, status)

## check warnings:
res %>% filter(status != "success") %>% pull(status_msg) %>% cat()


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
fit_constrained <- function(lavaan_str, data, group) {
  ## remove missings in grouping variable:
  group_miss <- is.na(data[[group]])
  data <- data[!group_miss, ]
  ## get number of groups from data:
  n_grps <- data[[group]] %>% unique() %>% length()
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
  ## fit models and get fit indices:
  ## NOTE: std.lv will be ignored if ID.fac is specified above
  fit_cfa_constrained <- models_constrained %>% 
    {
      bind_cols(
        group = group,
        grps = n_grps,
        grps_n = grps_n,
        constraint = names(.),
        purrr::map_dfr(., get_fit_indices, data, group = group)
      )
    }
  ## add differences of constrained models:
  fit_cfa_constrained <- fit_cfa_constrained %>%
    mutate(
      chisq_diff_0 = chisq - chisq[1],
      chisq_diff = chisq - lag(chisq),
      df_diff_0 = df - df[1],
      df_diff = df - lag(df),
      cfi_diff_0 = cfi - cfi[1],
      cfi_diff = cfi - lag(cfi),
      rmsea_diff = rmsea - lag(rmsea),
      srmr_diff = srmr - lag(srmr)
    )
  return(fit_cfa_constrained)
}
# debug(fit_constrained)
# undebug(fit_constrained)
fit_constrained(models_cfa[[1]], data = dat_fa, group = "t1_geschlecht")

# ## check getting estimated models out of data.frame:
# tmp <- fit_constrained(models_cfa[[1]], data = dat_fa, group = "t1_geschlecht", std.lv = TRUE)
# tmp2 <- fit_constrained(models_cfa[[1]], data = dat_fa, group = "t1_geschlecht", std.lv = FALSE)
# summary(tmp$fit[1][[1]], fit.measures = TRUE, standardized = TRUE)
# check_identical <- setdiff(names(tmp), c("fit"))
# identical(tmp$cfi, tmp2$cfi)


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
fit_groups <- function(lavaan_str, data, group_cfa) {
  purrr::map_dfr(
    groups_cfa, ~ fit_constrained(lavaan_str, data = data, group = .x)
  )
}
tmp3 <- fit_groups(models_cfa[[4]], data = dat_fa, group_cfa = group_cfa)
tmp3

## define how many constrained models are fitted for each model: 
n_mi_models <- 4
## fit all constrained models for all grouping variables:
res <- models_cfa %>% {
  bind_cols(
    ## get model names from list, repeated for the number of results from fit_groups:
    model = rep(names(.), each = length(groups_cfa) * n_mi_models),
    ## get results data:
    purrr::map_dfr(
      ., ~ fit_groups(.x, data = dat_fa, group_cfa = group_cfa)
    )
  )
}
res %>% print(n = 50)

## check warnings:
res %>% filter(status != "success") %>% 
  group_by(status, model, group) %>% 
  count() %>% 
  select(n, everything())
res %>% filter(status != "success") %>% pull(status_msg) %>% unique() %>% cat()

## ------------------------------------------------------------------------- ##
## stacked bar plot of delta-CFIs
## ------------------------------------------------------------------------- ##

## create plotting data:
dat_plot <- res %>%
  mutate(
    invariance_level = paste0(lag(constraint), "\nto ", constraint)
  ) %>% 
  filter(constraint != "configurational")

## plot delta CFI:
dat_plot %>%
  ggplot(aes(x = forcats::fct_relevel(invariance_level,
                                      "configurational\nto metric", 
                                      "metric\nto scalar", 
                                      "scalar\nto strict"),
             y = cfi_diff, 
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
    y = expression(Delta*"CFI")
  ) +
  theme_light()


