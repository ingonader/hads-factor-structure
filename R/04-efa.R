## ######################################################################### ##
## Exploratory Factor Analysis
## ######################################################################### ##

# rm(list = ls())
# rstudioapi::restartSession()

#' NOTE:
#' This script assumes that the following scripts have been run:
#' * setup scripts
#' * data load scripts
#' * data preparation scripts

source("./R/setup.R")
source(file.path(path_r, "01-data-load-raw.R"))
source(file.path(path_r, "02-data-prep.R"))

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

library(nFactors)
library(psych)
library(polycor)

## ========================================================================= ##
## exploratory factor analysis
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## determine number of factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## define dataset for exploratory factor analysis:
dat_efa <- dat_fa[varnames_item_hads]
#dat_efa <- dat_fa[varnames_item_emons]

## check that dataset has no missings:
assertthat::are_equal(
  dim(dat_efa),
  dim(na.omit(dat_efa))
)

## calculate correlation matrix of polychoric correlations:
cormat_efa_hc <- hetcor(dat_efa, ML = TRUE)
cormat_efa <- cormat_efa_hc[["correlations"]]

## calculate eigenvalues (and eigenvectors):
ev <- eigen(cormat_efa)
#ev$values

## data for scree plot with ggplot:
dat_plot_scree <- data.frame(
  "n" = seq_along(ev$values),
  "eigenvalue" = ev$values
)

## scree plot:
dat_plot_scree %>%
  ggplot(aes(x = n, y = eigenvalue)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(from = 0, to = max(dat_plot_scree$n, na.rm = TRUE), by = 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = max(dat_plot_scree$eigenvalue, na.rm = TRUE), by = 1))

## scree plot with parallel analysis:
ap <- parallel(subject = nrow(dat_efa), var = ncol(dat_efa), rep = 100, cent=0.05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## EFA: orthogonal solution
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## orthogonal factors:
fit_efa_ortho <- fa(r = cormat_efa, nfactors = 2, n.obs = nrow(dat_efa),
                    rotate = "varimax", fm = "wls")
#fit_efa_ortho

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## EFA: solution with correlated factors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## correlated factors:
fit_efa_rot <- fa(r = cormat_efa, nfactors = 2, n.obs = nrow(dat_efa),
                  rotate = "oblimin", fm = "wls")
#fit_efa_rot



# ## base factanal:
# 
# ## orthogonal factors:
# fit_efa_ortho <- factanal(dat_efa, factors = 2, rotation = "varimax")
# fit_efa_ortho
# 
# ## unrotated solution:
# fit_efa_norot <- factanal(dat_efa, factors = 2, rotation = "none")
# fit_efa_norot
# 
# ## correlated factors:
# fit_efa <- factanal(dat_efa, factors = 2, rotation = "promax")
# fit_efa



