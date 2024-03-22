est_rd <- function(dt, p, outcol, ...){

  # get vectors
  rv <- dt[, get(p$running)]
  outcome <- dt[, .SD, .SDcols = outcol] |> unlist()
  if(!is.null(p$covariate)){
    cov <- dt[, .SD, .SDcols = p$covariate]
  } else {cov <- NULL}

  #estimation
  rd_est <- tryCatch(suppressWarnings(rdrobust(y = outcome , x = rv,
                                               c = p$cutoff,
                                               p = p$order, h = p$bandwidth, vce = p$vce, covs = cov, ...)),
                     error = function(e){
                       message("failed for ", outcol, " message: ", e)
                       return(NULL)
                     })

  #post process
  if(is.null(rd_est)){
    ests <- data.table(coef = NA, se = NA, pvalue = NA, llim = NA, rlim = NA, bw = NA)
  } else {

    resultindex <- switch(p$est, conv = 1, biasc = 2, robust = 3)
    coef <- rd_est$coef[resultindex]
    se <- rd_est$se[resultindex]
    pvalue <- (1 - pnorm(abs(coef/se)))*2
    bw <- rd_est$bws[1,1]

    if(p$est == "conv"){
      llim <- rd_est$tau_cl[1]
      rlim <- rd_est$tau_cl[2]
    } else {
      llim <- rd_est$tau_bc[1]
      rlim <- rd_est$tau_bc[2]
    }

    ests <- data.table(coef, se, pvalue, llim, rlim, bw)
  }

  return(ests)

}
