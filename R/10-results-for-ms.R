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

construct_modelname <- function(x) {
  plyr::revalue(x,
                c(
                  "zigmond_2f_cor" = "Zigmond & Snaith (1983)",
                  "razavi_1f" = "Razavi et al. (1990)",
                  "moorey_2f_cor" = "Moorey et al. (1991)",
                  "zigmond_mod01_2f_cor" = "Zigmond & Snaith (1983; 13 items)",
                  "zigmond_mod02_2f_cor" = "Zigmond & Snaith (1983; 12 items)",
                  "dunbar_3f_cor" = "Dunbar et al. (2000)",
                  "friedman_3f_cor" = "Friedman et al. (2001)",
                  "caci_3f_cor" = "Caci et al. (2003)",
                  "emons_2f_cor" = "Emons et al. (2012)",
                  "dunbar_3f_cor_constr" = "Dunbar et al. (2000; constr.)",
                  "caci_3f_cor_constr" = "Caci et al. (2003; constr.)"
                )
  )
}

## ========================================================================= ##
## CFA model results
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## overview table of CFA models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## create tibble:
res_cfa_ms <- res_cfa_mlr %>%
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
    "Model" = "model",
    "CFI" = "cfi.robust",
    "RMSEA" = "rmsea.robust"
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
  autofit()
save_as_docx(ft_res_cfa, path = file.path(path_tmp, "flextable.docx"))
save_as_docx(ft_res_cfa, path = file.path(path_ms, "flextable.docx"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## correlations between factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## correlation matrix of a single model:
lavPredict(res_cfa_mlr$fit[[1]]) %>% cor()

## correlation matrix of all models:
purrr::map(res_cfa_mlr$fit, ~ cor(lavPredict(.x))) %>% 
  setNames(res_cfa_mlr$model)

## max correlation of all models:
purrr::map(res_cfa_mlr$fit, function(.x) {
  cormat <- cor(lavPredict(.x))
  ret <- max(cormat[lower.tri(cormat)])
  return(ret)
}) %>% 
  setNames(res_cfa_mlr$model) %>%
  as_tibble()
