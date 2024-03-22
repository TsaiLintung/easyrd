#' Generate Parameters for RD Analysis
#'
#' This function generates a list of parameters for regression discontinuity (RD) analysis based on user-defined inputs. It is primarily used to set up the necessary parameters before conducting an RD analysis.
#'
#' @param outcomes A character vector specifying the outcome variables for the RD analysis.
#' @param running A string indicating the running variable used in the RD analysis.
#' @param cutoff A numeric value specifying the cutoff point for the running variable in the RD analysis.
#' @param vce A string specifying the variance-covariance estimator to be used. Default is "hc1".
#' @param est A string indicating the estimation method to be used. Default is "robust".
#' @param order An integer specifying the order of the polynomial to fit in the RD design. Default is 1 (linear).
#' @param bandwidth A numeric specifying the bandwidth around the cutoff. If NULL, it will be calculated automatically based on the data and method. Default is NULL.
#' @param bin_bandwidth A numeric value used to specify the bandwidth for binning data in the graphical representation of the RD design. If NULL, it will be determined automatically. Default is NULL.
#' @param covariate A vector of strings representing any covariates to be included in the RD analysis. Default is NULL.
#'
#' @return A list containing the specified parameters for the RD analysis.
#'
#' @examples
#' params <- get_param(
#'   outcomes = c("test_score", "graduation_rate"),
#'   running = "age",
#'   cutoff = 18,
#'   vce = "hc1",
#'   est = "robust",
#'   order = 2,
#'   bandwidth = 5,
#'   bin_bandwidth = 0.5,
#'   covariate = c("gender", "income")
#' )
#'
#' @export

get_param <- function(outcomes, running, cutoff,
                      vce = "hc1", est = "robust", order = 1,
                      bandwidth = NULL, bin_bandwidth = NULL, covariate = NULL){

  p <- list()
  p$outcomes <- outcomes
  p$running <- running
  p$cutoff <- cutoff
  p$vce <- vce
  p$est <- est
  p$order <- order
  p$bandwidth <- bandwidth
  p$bin_bandwidth <- bin_bandwidth
  p$covariate <- covariate

  return(p)

}
