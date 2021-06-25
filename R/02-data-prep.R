## ######################################################################### ##
## Load preparation
## ######################################################################### ##

# rm(list = ls())
# rstudioapi::restartSession()

#' NOTE:
#' This script assumes that the following scripts have been run:
#' * setup scripts
#' * data load scripts

# source("./R/setup.R")
# source(file.path(path_r, "01-data-load-raw.R"))

## ========================================================================= ##
## load additional packages
## ========================================================================= ##


## ========================================================================= ##
## copy data for data prep
## ========================================================================= ##

dat_all <- dat_raw

## ========================================================================= ##
## recode 99, 999 to missing values where appropriate
## ========================================================================= ##

# ## check for 99 in data:
# summarize_all(dat_raw, ~ sum(as.character(.x) == "99", na.rm = TRUE)) %>% 
#   tidyr::pivot_longer(cols = everything()) %>%
#   filter(value > 0)
# 
# ## check for 999 in data:
# summarize_all(dat_raw, ~ sum(as.character(.x) == "999", na.rm = TRUE)) %>% 
#   tidyr::pivot_longer(cols = everything()) %>%
#   filter(value > 0)
# 
# ## check for 77 in data:
# summarize_all(dat_raw, ~ sum(as.character(.x) == "77", na.rm = TRUE)) %>% 
#   tidyr::pivot_longer(cols = everything()) %>%
#   filter(value > 0)


## ========================================================================= ##
## add and modify variables
## ========================================================================= ##

## add age:
dat_all <- dat_all %>% 
  mutate(
    t1_alter_calc = time_length(dat_all$t1_datum - dat_all$geburtsdatum, "years"),
    t1_alter_both = coalesce(floor(t1_alter_calc), t1_alter)
  )

## check t1_alter with calculated age:
# dat_all %>% select(geburtsdatum, t1_datum, t1_alter_calc, t1_alter) %>% na.omit()

## add study year:
dat_all <- dat_all %>%
  mutate(
    yr = lubridate::year(t1_datum)
  )

## change dbl+lbl variables to factors where appropriate:
varnames_convert_factor <- c(
  "t1_geschlecht", "t1_familienstand", "t1_kinder_unter_ueber_18",
  "t1_schulbildung", "t1_berufstaetigkeit", "t1_einkommen_kat", "t1_wohnort",
  "t1_untersuchungsort", "t1_komplementaermed_dich", 
  "t1_komplementaermed_form", "t1_psychologische_behandlung", 
  "t1_psy_erkrank_dich", "t1_psy_erkrank_label",  "t1_info_erkrankung", "t1_info_behandlung", 
  "t1_info_psychotherapie", "t1_info_selbsthilfegruppen", "t1_info_andere", 
  "diagnosen", "diagnose", "diagnose_final",
  "onkodiagnosen", "tumorklassifikation",  "tumorart", "nodes", 
  "nebendiagnose"
)
dat_all <- dat_all %>% mutate_at(
  varnames_convert_factor,
  as_factor
)

## ========================================================================= ##
## define relevant variables
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## technical variables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

varnames_id <- "laufende_nr"
varnames_tech <- c("t1_datum", "t1_untersuchungsort")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## demographic and info variables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

varnames_info <- grep("^info", names(dat_all), value = TRUE)
varnames_sozio <- c(
  "geschlecht", "t1_alter_both", #"psychologische_behandlung",
  varnames_info
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## HADS
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## variable names of items that have been entered into SPSS file:
varnames_item_anx_orig <- grep("^t1_hads[0-9]+_a$", names(dat_all), value = TRUE)
varnames_item_depr_orig <- grep("^t1_hads[0-9]+_d$", names(dat_all), value = TRUE)

## variable names of items after recoding them to 0-3 
## (high values mean more severe symptoms):
varnames_item_anx_recode <- grep("^t1_hads[0-9]+_a_recoded$", names(dat_all), value = TRUE)
varnames_item_depr_recode <- grep("^t1_hads[0-9]+_d_recoded$", names(dat_all), value = TRUE)

## define names for all HADS items:
varnames_item_hads_orig <- base::union(varnames_item_anx_orig,
                                       varnames_item_depr_orig)
varnames_item_hads_recode <- base::union(varnames_item_anx_recode,
                                         varnames_item_depr_recode)

## define variable names that are used for analysis:
varnames_item_hads <- varnames_item_hads_recode
varnames_item_anx <- varnames_item_anx_recode
varnames_item_depr <- varnames_item_depr_recode

## define Emons items; items to remove: 7, 11, 10, 14
varnames_item_emons <- varnames_item_hads %>% {
  .[!grepl("07|11|10|14", .)]
}

## define simple variable names for hads items and ... 
varnames_fa <- stringr::str_replace_all(
  varnames_item_hads,
  ".*?([0-9]{2}).*",
  "i_\\1"
)
## ... copy relevant items into new variables with simple names:
dat_all[varnames_fa] <- dat_all[varnames_item_hads]


## ========================================================================= ##
## sanity checks
## ========================================================================= ##

## create indicators (counters) of problems per scale:
dat_tmp <- dat_raw %>% 
  {mutate(.,
          probs_hads_orig = check_valrange(.[varnames_item_hads_orig], val_rng = 1:4),
          probs_hads_recode = check_valrange(.[varnames_item_hads_recode], val_rng = 0:3)
  )}
assertthat::are_equal(
  dat_tmp %>% filter((probs_hads_orig > 0) | (probs_hads_recode > 0)) %>% nrow(),
  0
)

## ========================================================================= ##
## missing variables per scale
## ========================================================================= ##

## add variable with percentage of missings per scale:
dat_all <- dat_all %>% {mutate(., 
  miss_item_anx = is.na(.[varnames_item_anx]) %>% rowSums(),
  miss_item_depr = is.na(.[varnames_item_depr]) %>% rowSums(),
  miss_item_hads = is.na(.[varnames_item_hads]) %>% rowSums()
)}

## ========================================================================= ##
## exclude participants
## ========================================================================= ##

## exclude early years with almost no participants (2011, 2012)
dat_all <- dat_all %>%
  filter((yr >= 2013) | is.na(yr))

## exclude all non-responders (participants that have no item scores,
## i.e., keep all with at least one item response):
dat_all <- dat_all %>%
  filter(miss_item_hads < 14)
nrow(dat_all)

#dat_all %>% filter((yr >= 2013) | is.na(yr)) %>% nrow()
#dat_all %>% filter((yr >= 2013)) %>% nrow()

## exclude responders that have missing values:
dat_fa <- dat_all %>%
  filter(miss_item_hads == 0)
nrow(dat_fa)

## ========================================================================= ##
## add grouping variables (after exclusions)
## ========================================================================= ##

## median date:
date_med <- median(dat_all$t1_datum, na.rm = TRUE)

## add grouping variables:
dat_fa <- dat_fa %>%
  mutate(
    t1_alter_grp2 = cut_number(t1_alter_both, n = 2),
    t1_datum_grp2 = ifelse(
      t1_datum <= date_med,
      paste0("<=", date_med),
      paste0(">", date_med)
    ) %>% 
      as.factor()
  )

# check groupings:
# dat_all$t1_alter_grp3
# dat_all$t1_datum_grp2 %>% table(useNA = "if")
# dat_all %>% group_by(t1_datum_grp2) %>%
#   summarize(date_min = min(t1_datum, na.rm = TRUE),
#             date_max = max(t1_datum, na.rm = TRUE),
#             n = n()) %>%
#   ungroup() %>%
#   arrange(1) %>%
#   mutate(lbl = paste0(date_min, "_-_", date_max))
# dat_all %>% group_by(krebstyp_grp3) %>% summarize(paste(unique(krebstyp), collapse = ","))
# dat_all %>% group_by(hads_grp2ext) %>% summarize(ha = max(t1_hads_skalaa_summe_new, na.rm = TRUE),
#                                                  hd = max(t1_hads_skalad_summe_new, na.rm = TRUE))



