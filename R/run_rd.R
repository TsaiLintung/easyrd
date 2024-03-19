run_rd <- function()


run_rd_main <- function(dt, p, verbose = TRUE, ...){

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
      ests <- data.table(outcome = outcol, coef = NA, se = NA, pvalue = NA, llim = NA, rlim = NA, bw = NA)
    } else {

      resultindex <- switch(est, conv = 1, biasc = 2, robust = 3)

      coef <- rd_est$coef[resultindex]
      se <- rd_est$se[resultindex]
      pvalue <- (1 - pnorm(abs(coef/se)))*2
      bw <- rd_est$bws[1,1]
      
      if(est == "conv"){
        llim <- rd_est$tau_cl[1]
        rlim <- rd_est$tau_cl[2]
      } else {
        llim <- rd_est$tau_bc[1]
        rlim <- rd_est$tau_bc[2]
      }
      
      ests <- data.table(outcome = outcol, coef, se, pvalue, llim, rlim, bw)
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
