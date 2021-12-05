## ######################################################################### ##
## Results for manuscript
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
#' * all model check scripts

source("./R/setup.R")

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

library(flextable)
library(officer)


## ========================================================================= ##
## global table options
## ========================================================================= ##

#get_flextable_defaults()
set_flextable_defaults(
  font.family = "Times New Roman",
  big.mark = ""
)

## ========================================================================= ##
## function definitions
## ========================================================================= ##


construct_modelname <- function(x) {
  plyr::revalue(x,
                c(
                  "zigmond_2f_cor" = "Zigmond & Snaith (1983)",
                  "zigmond_man_2f_cor" = "Zigmond & Snaith (1983; 8 items)",
                  "razavi_1f" = "Razavi et al. (1990)",
                  "moorey_2f_cor" = "Moorey et al. (1991)",
                  "dunbar_3f_cor" = "Dunbar et al. (2000)",
                  "friedman_3f_cor" = "Friedman et al. (2001)",
                  "caci_3f_cor" = "Caci et al. (2003)",
                  "smith_1f" = "Smith (2006; one-factor solution)",
                  "smith_2f" = "Smith (2006; two-factor solution)",
                  "emons_2f_cor" = "Emons et al. (2012)",
                  "dunbar_3f_cor_constr" = "Dunbar et al. (2000; constr.)",
                  "caci_3f_cor_constr" = "Caci et al. (2003; constr.)",
                  "zigmond_mod01_2f_cor" = "13-item model",
                  "zigmond_mod02_2f_cor" = "12-item model"
                )
  )
}

## ========================================================================= ##
## global variables
## ========================================================================= ##

## model order in ms tables:

dat_order <- tribble(
  ~sort_order, ~model,
  10, "zigmond_2f_cor",
  11, "zigmond_man_2f_cor",
  20, "razavi_1f",
  30, "moorey_2f_cor",
  40, "dunbar_3f_cor",
  41, "dunbar_3f_cor_constr",
  50, "friedman_3f_cor",
  60, "caci_3f_cor",
  61, "caci_3f_cor_constr",
  65, "smith_1f",
  66, "smith_2f",
  70, "emons_2f_cor",
  80, "zigmond_mod01_2f_cor",
  90, "zigmond_mod02_2f_cor"
)


## ========================================================================= ##
## CFA model results
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## overview table of CFA models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## create tibble:
res_cfa_ms <- res_cfa_mlr %>%
  left_join(dat_order, by = "model") %>%
  arrange(sort_order) %>%
  select(
    model, 
    #npar, 
    #chisq, 
    chisq.scaled,
    df.scaled,
    cfi.robust, 
    rmsea.robust) %>% #, status)
  filter(!(model %in% c("dunbar_3f_cor", "caci_3f_cor"))) %>%
  mutate(model = construct_modelname(model)) %>%
  mutate_at(vars(contains("chisq")), ~ round(.x, 1)) %>%
  mutate_at(vars(contains("cfi"), contains("rmsea")), ~ round(.x, 3))
res_cfa_ms

## check tibble:
#res_cfa_mlr %>% select(contains("df"))


## create flextable and save to file:
ft_res_cfa <- res_cfa_ms %>% 
  rename(
    "Model" = "model"
  ) %>%
  flextable() %>%
  # compose(
  #   i = 1, j = "npar", part = "header",
  #   value = as_paragraph(
  #     as_i("N"),
  #     as_sub("Param")
  #   )
  # ) %>%
  # compose(
  #   i = 1, j = "chisq", part = "header",
  #   value = as_paragraph(
  #     as_chunk("c", props = fp_text(font.family = "Symbol")),  ## Chi
  #     as_sup("2")
  #   )
  # ) %>%
  compose(
    i = 1, j = "chisq.scaled", part = "header",
    value = as_paragraph(
      as_i("\u03C7"), # as_chunk("c", props = fp_text(font.family = "Symbol")),  ## Chi  ## Segoe UI Symbol
      as_sup("2"),
      as_sub("scaled")
    )
  ) %>%
  compose(
    i = 1, j = "df.scaled", part = "header",
    value = as_paragraph(
      as_i("df")
      #as_sub("scaled")
    )
  ) %>% 
  compose(
    i = 1, j = "cfi.robust", part = "header",
    value = as_paragraph(
      "CFI",
      as_sub("robust")
    )
  ) %>% 
  compose(
    i = 1, j = "rmsea.robust", part = "header",
    value = as_paragraph(
      "RMSEA",
      as_sub("robust")
    )
  ) %>% 
  autofit()
save_as_docx(ft_res_cfa, path = file.path(path_tmp, "table-cfa-model-fit.docx"))
save_as_docx(ft_res_cfa, path = file.path(path_ms, "table-cfa-model-fit.docx"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## estimation problems for some 3-factor models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## dunbar_3f_cor
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## CFA estimation in full sample:
res_cfa_mlr %>% 
  filter(model == "dunbar_3f_cor") %>%
  pull(status_msg)

res_cfa_mlr %>% 
  filter(model == "dunbar_3f_cor") %>%
  pull(fit) %>%
  .[[1]] %>%
  summary()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## caci_3f_cor
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## CFA estimation in full sample:

res_mi_mlr %>% 
  filter(model == "caci_3f_cor") %>%
  select(model, group, grps, grps_n, constraint, status, status_msg) %>%
  print(n = 30)

res_cfa_mlr %>% 
  filter(model == "caci_3f_cor") %>%
  pull(fit) %>%
  .[[1]] %>%
  summary()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## correlations between factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## correlation matrix of a single model:
lavPredict(res_cfa_mlr$fit[[1]]) %>% cor()

## correlation matrix of all models:
purrr::map(res_cfa_mlr$fit, ~ round(cor(lavPredict(.x)), 3)) %>% 
  setNames(res_cfa_mlr$model)

## max correlation of all models:
purrr::map(res_cfa_mlr$fit, function(.x) {
  cormat <- cor(lavPredict(.x))
  ret <- max(cormat[lower.tri(cormat)])
  return(ret)
}) %>% 
  setNames(res_cfa_mlr$model) %>%
  as_tibble()

## ========================================================================= ##
## measurement invariance
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## MI tables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

add_constraint_order <- function(dat, n_mi_models = 6) {
  n_models <- length(unique(dat$model))
  n_grps <- length(unique(dat$group))
  n_constraints <- nrow(dat) / (n_models * n_grps)
  mutate(dat,
         constraint_order = rep(1:n_mi_models, (n_models * n_grps))
         )
}

## TODO: arrange might not work, yet; 
## needs to be arranged by group and constraint, too

## create tibble with relevant results:
res_mi_ms <- res_mi_mlr %>% 
  filter(constraint != "strict") %>%  ## TODO: remove "strict" invariance
  add_constraint_order(n_mi_models = 5) %>%
  left_join(dat_order, by = "model") %>%
  arrange(sort_order, group, constraint_order) %>%
  select(model, 
         group, 
         constraint, 
         #npar, 
         chisq.scaled, 
         df.scaled, 
         cfi.robust, 
         cfi_robust_diff) %>%
  filter(!(model %in% c("dunbar_3f_cor", "caci_3f_cor"))) %>%
  mutate(model = construct_modelname(model)) %>%
  mutate_at(vars(contains("chisq")), ~ round(.x, 1)) %>%
  mutate_at(vars(contains("cfi")), ~ round(.x, 3))

## create flextable and save to file:
ft_res_mi <- res_mi_ms %>%
  ## change contents of columns: ------------------------------------------- ##
  mutate(
    ## remove duplicate entries within columns:
    model = if_else(model != lag(model), model, "", model),
    group = if_else(group != lag(group), group, "", group),
    ## change group column:
    group = group %>% stringr::str_replace_all(
      "t1_geschlecht", "Sex"
      ) %>%
      stringr::str_replace_all(
        "t1_alter_grp2", "Age"
      ) %>%
      stringr::str_replace_all(
        "t1_datum_grp2", "Time of response"
      ) %>%
      stringr::str_replace_all(
        "tumorart", "Tumor type"
      ),
    ## change constraint column:
    constraint = constraint %>% stringr::str_replace_all(
        "^grp ", "G"
      ) %>%
      stringr::str_replace_all(
        "männlich", "men"
      ) %>%
      stringr::str_replace_all(
        "weiblich", "women"
      ) %>%
      stringr::str_replace_all(
        "solider Tumor", "solid"
      ) %>%
      stringr::str_replace_all(
        "hämatologischer Tumor", "haematological"
      ) %>%
      stringr::str_to_sentence()
  ) %>%
  ## rename columns (part 1): ---------------------------------------------- ##
  rename(
    "Model" = "model",
    "Group" = "group",
    "Constraint" = "constraint"
  ) %>%
  flextable() %>%
  ## rename columns (part 2): ---------------------------------------------- ##
  compose(
    i = 1, j = "chisq.scaled", part = "header",
    value = as_paragraph(
      as_i("\u03C7"), ## Chi
      as_sup("2"),
      as_sub("scaled")
    )
  ) %>%
  compose(
    i = 1, j = "df.scaled", part = "header",
    value = as_paragraph(
      as_i("df")
      #as_sub("scaled")
    )
  ) %>%   compose(
    i = 1, j = "cfi_robust_diff", part = "header",
    value = as_paragraph(
      "\u0394",  ## delta
      "CFI",
      as_sub("robust")
    )
  ) %>%   compose(
    i = 1, j = "cfi.robust", part = "header",
    value = as_paragraph(
      "CFI",
      as_sub("robust")
    )
  ) %>% 
  autofit()
save_as_docx(ft_res_mi, path = file.path(path_tmp, "table-measurement-invariance.docx"))
save_as_docx(ft_res_mi, path = file.path(path_ms, "table-measurement-invariance.docx"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## stacked bar plot of delta-CFIs
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## define colors:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")[1:4]

## create plotting data:
dat_plot <- res_mi_mlr %>%
  left_join(dat_order, by = "model") %>%
  filter(constraint %in% c("metric", "scalar")) %>%
  filter(!(model %in% c("dunbar_3f_cor", "caci_3f_cor"))) %>%
  mutate(
    model = construct_modelname(model),
    model = forcats::fct_reorder(model, sort_order)
  )

## plot delta CFI:
plot_mi_mlr <- dat_plot %>%
  ggplot(aes(x = forcats::fct_relevel(constraint,
                                      "metric", 
                                      "scalar"),
             y = cfi_robust_diff, 
             color = group,
             fill = group, 
             group = group)) + 
  geom_bar(position="dodge", stat="identity", width = .4) +
  #geom_hline(yintercept = -0.003, linetype = "dashed", color = "darkgrey", alpha = .6) + 
  geom_hline(yintercept = -0.01, linetype = "dashed", color = "darkgrey", alpha = .8) + 
  scale_fill_manual(
    name = "Grouping Variable",
    breaks = c("t1_alter_grp2", "t1_datum_grp2", "t1_geschlecht", "tumorart"),
    labels = c("Age", "Time of reponse", "Gender", "Cancer type"),
    values = cbp1
  ) +
  scale_color_manual(
    name = "Grouping Variable",
    breaks = c("t1_alter_grp2", "t1_datum_grp2", "t1_geschlecht", "tumorart"),
    labels = c("Age", "Time of reponse", "Gender", "Cancer type"),
    values = cbp1
    
  ) +
  facet_wrap(vars(model)) +
  labs(
    x = "",
    y = expression(Delta*"CFI (robust)")
  ) +
  theme_classic()
plot_mi_mlr

ggsave(filename = file.path(path_plot, "fig-mi-02-mlr.jpg"), width = 8, height = 5, scale = 1.5, dpi = 600)
ggsave(filename = file.path(path_ms, "fig-mi-02-mlr.jpg"), width = 8, height = 5, scale = 1.5, dpi = 600)

