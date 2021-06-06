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
      as_chunk("c", props = fp_text(font.family = "Symbol")),  ## Chi
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
  )
save_as_docx(ft_res_cfa, path = file.path(path_tmp, "flextable.docx"))

