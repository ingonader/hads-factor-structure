## ######################################################################### ##
## Function definitions
## ######################################################################### ##


#' Check value range (min/max) for continuous variables
#' 
#' Check how many values of a given data.frame with continuous variables 
#' are below the minimum or above the maximum of the correct value range.
#'
#' @param df A data.frame with the relevant variables, which all need
#'   to be continuous.
#' @param val_rng The value range: minimum and maximum of acceptable
#'   values.
#'
#' @return A vector with length \code{nrow(df)}, containing the number 
#'   of violations for each row of the given data.frame.
#' @export
#'
#' @examples
check_minmax <- function(df, val_rng) {
  min_val <- min(val_rng)
  max_val <- max(val_rng)
  ## check min and max:
  n_below_min <- rowSums(df < min_val, na.rm = TRUE)
  n_above_max <- rowSums(df > max_val, na.rm = TRUE)
  return(n_below_min + n_above_max)
}


#' Check value range for categorical variables
#' 
#' Check how many values of a given data.frame with categorical or discrete 
#' variables have values that are not in the allowed set of values.
#'
#' @param df A data.frame with the relevant variables, which all need
#'   to be discrete.
#' @param val_rng A vector with allowed values. Missings (NA) are not
#'   counted as violations.
#'
#' @return A vector with length \code{nrow(df)}, containing the number 
#'   of violations for each row of the given data.frame.
#' @export
#'
#' @examples
check_valrange <- function(df, val_rng) {
  ## check value range:
  n_not_valrng <- apply(df, 1:2, function(i)
    !(i %in% val_rng) & !is.na(i)) %>%
    rowSums(na.rm = TRUE)
  return(n_not_valrng)
}


#' Estimate CFA model and return vector of fit indices 
#'
#' @param model_def A string (character vector of length one) containing
#'   the model definition as expected by lavaan.
#' @param data A data.frame with the data used to estimate the lavaan
#'   model.
#'
#' @return A vector with all fit indices as returned by lavaan.
#' @export
#'
#' @examples
get_fit_indices <- function(model_def, data) {
  fit_cfa <- cfa(model_def, data = data)
  fit_cfa_summary <- summary(fit_cfa, standardized = TRUE, fit.measures = TRUE)
  return(fit_cfa_summary$FIT)
}


