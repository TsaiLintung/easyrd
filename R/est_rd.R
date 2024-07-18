est_rd <- function(dt, p, outcol){

  # get vectors
  rv <- dt[, get(p$running)]
  outcome <- dt[, .SD, .SDcols = outcol] |> unlist()
  if(!is.null(p$covs)){
    covs <- dt[, .SD, .SDcols = p$covs]
  } else {covs <- NULL}
  if(!is.null(p$cluster)){
    cluster <- dt[, .SD, .SDcols = p$cluster]
  } else {cluster <- NULL}

  #estimation
  rdrobust_param <- c(list(y = outcome, x = rv, c = p$cutoff, h = p$bandwidth, covs = covs, cluster = cluster), p$est_param)
  rd_est <- tryCatch(suppressWarnings(do.call(rdrobust, rdrobust_param)),
                     error = function(e){
                       message("failed for ", outcol, " message: ", e)
                       return(NULL)
                     })

  #post process
  if(is.null(rd_est)){
    ests <- data.table(coef = NA, se = NA, pvalue = NA, llim = NA, rlim = NA, bw = NA)
  } else {
  
    #get the result from the right estimation
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
