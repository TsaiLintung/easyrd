#' Run RD Estimation
#'
#' This function performs Regression Discontinuity (RD) estimation on the provided data table based on the parameters specified in the 'p' list. It supports various adjustments including bandwidth, covariates, and estimation methods. The function is capable of processing multiple outcomes specified in the 'p' list.
#'
#' @param dt A data.table object containing the data to be used in the RD estimation.
#' @param p A list containing settings for the RD estimation including 'outcomes', 'running', 'cutoff', 'order', 'bandwidth', 'covariate', 'vce', and 'est'. These settings specify the outcomes to analyze, the running variable, cutoff point, polynomial order, bandwidth, additional covariates, variance estimation method, and estimation method respectively.
#' @param verbose A logical flag indicating whether to print messages about the estimation process and results. Default is TRUE.
#' @param ... Additional arguments passed to the rdrobust function.
#'
#' @return Returns a data.table that includes the RD estimation results for each outcome specified in the 'p' list, including coefficients, standard errors, p-values, and bandwidths.
#' @examples
#'
#' #create example data
#' x <- runif(1000, -1, 1)
#' x1 <- runif(1000, -1, 1)
#' y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)
#' dt <- data.table(x = x, y = y, x1 = x1)
#'
#' # essential parameters
#'
#' params <- list(outcomes = c("y"),
#'                running = "x",
#'                cutoff = 0)
#'
#' # Run RD estimation
#' result <- run_rd(my_data_table, params)
#'
#' # all parameters with default values
#' allparams <- list(outcomes = c("y"),
#'                   running = "x",
#'                   cutoff = 0,
#'                   order = 1,
#'                   bandwidth = 0.5,
#'                   covariate = c("x1"),
#'                   vce = "hc1", #see vce in rdrobust
#'                   est = "conv") #conv, biasc, robust
#'
#' @import data.table rdrobust
#' @export
run_rd <- function(dt, p, verbose = TRUE, ...){

  #get the setting from p
  outcomes <- p$outcomes
  running <- p$running
  cutoff <- p$cutoff
  order <- p$order
  bandwidth <- p$bandwidth
  covariate <- p$covariate
  vce <- p$vce
  est <- p$est

  #default values
  if(is.null(order)){order <- 1}
  if(is.null(vce)){vce <- "hc1"}
  if(is.null(est)){est <- "robust"}

  # some preprocess
  rv <- dt[, get(running)]

  if(!is.null(covariate)){
    cov <- dt[, .SD, .SDcols = covariate]
  } else {cov <- NULL}

  rd_result <- data.table()
  for(outcol in outcomes){

    outcome <- dt[, .SD, .SDcols = outcol] |> unlist()

    #the actual estimation step
    rd_est <- tryCatch(suppressWarnings(rdrobust(y = outcome , x = rv,
                                                 c = cutoff,
                                                 p = order, h = bandwidth, vce = vce, covs = cov, ...)),
                       error = function(e){
                         message("failed for ", outcol, " message: ", e)
                         return(NULL)
                       })

    if(is.null(rd_est)){
      ests <- data.table(outcome = outcol, coef = NA, se = NA, pvalue = NA, bw = NA)
    } else {

      resultindex <- switch(est, conv = 1, biasc = 2, robust = 3)

      coef <- rd_est$coef[resultindex]
      se <- rd_est$se[resultindex]
      pvalue <- (1 - pnorm(abs(coef/se)))*2
      bw <- rd_est$bws[1,1]

      ests <- data.table(outcome = outcol, coef, se, pvalue, bw)
    }

    rd_result <- rbind(rd_result, ests)
    if(verbose){
      message("estimated result for ", outcol)
    }

    gc()

  }

  rd_result[, note := ""]
  return(rd_result)

}
