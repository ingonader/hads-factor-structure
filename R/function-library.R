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
#' Takes a model definition string in lavaan syntax, estimates the
#' specified model via the \code{cfa()} function and returns the 
#' fit indices.
#'
#' @param model_def A string (character vector of length one) containing
#'   the model definition as expected by lavaan.
#' @param data A data.frame with the data used to estimate the lavaan
#'   model.
#' @param group A grouping variable (character vector of lenght one) 
#'   that is passed on to lavaan's \code{cfa()} function. Defaults to
#'   NULL, which means no groups.
#' @param std.lv Determines if variances are fixed to 1 (if \code{TRUE}) 
#'   or if first factor loading is fixed to 1 (if \code{FALSE}). 
#'   Passed to lavaans \code{cfa()} function, but defaults to 
#'   \code{TRUE} for this funciton (instead of \code{FALSE} as in 
#'   the \code{cfa()} function).
#' @param ... Additional parameters passed to lavaaan's \code{cfa()} 
#'   function
#'
#' @return A vector with all fit indices as returned by lavaan.
#' @export
#'
#' @examples
get_fit_indices <- function(model_def, data, group = NULL, std.lv = TRUE, ...) {
  fit_cfa <- tryCatch(
    withCallingHandlers(
      {
        status <- "success"
        status_msg <- ""
        list(
          "model" = cfa(model_def, data = data, group = group, std.lv = std.lv, ...),
          "status" = status,
          "status_msg" = status_msg
        )
      },
      warning = function(e) {
        ## overwrite status_msg in above environment:
        status <<- "WARNING"
        status_msg <<- paste0(e)
        ## and resume processing:
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      return(list(
        "model" = NA,
        status = "ERROR",
        status_msg = paste0(e)
      ))
    }, 
    finally = {
    }
  )
  ## redirect stdout to file (to avoid cluttering the screen):
  sink("output.txt", type = "output")
  fit_cfa_summary <- summary(fit_cfa$model, standardized = TRUE, fit.measures = TRUE)
  ## output back to screen:
  sink()
  ## convert results to data.frame with one row:
  ret <- bind_rows(fit_cfa_summary$FIT)
  ## add model fit to data.frame:
  ret$fit <- list(fit_cfa$model)
  ## add status message (warning/error) to fit indices:
  ret$status <- fit_cfa$status
  ret$status_msg <- fit_cfa$status_msg
  return(ret)
}


