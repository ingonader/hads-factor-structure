## ######################################################################### ##
## Function definitions
## ######################################################################### ##

check_minmax <- function(df, val_rng) {
  min_val <- min(val_rng)
  max_val <- max(val_rng)
  ## check min and max:
  n_below_min <- rowSums(df < min_val, na.rm = TRUE)
  n_above_max <- rowSums(df > max_val, na.rm = TRUE)
  return(n_below_min + n_above_max)
}

check_valrange <- function(df, val_rng) {
  ## check value range:
  n_not_valrng <- apply(df, 1:2, function(i)
    !(i %in% val_rng) & !is.na(i)) %>%
    rowSums(na.rm = TRUE)
  return(n_not_valrng)
}


