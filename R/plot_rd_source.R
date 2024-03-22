plot_rd_source <- function(dt, p, outcol){

  #get cols
  rv <- dt[, get(p$running)]
  outcome <- dt[, get(outcol)]

  #default values
  if(is.null(p$bandwidth)){
    bwresult <- rdbwselect(y = outcome, x = rv, c = p$cutoff, p = p$order, q = p$order+1, vce = p$vce)
    bw <- bwresult$bws[1]
  } else {
    bw <- p$bandwidth
  }
  if(is.null(p$bin_bandwidth)){
    bbw <- bw
  } else {
    bbw <-p$bin_bandwidth
  }

  #get the rd plot
  rdp <- tryCatch(rdplot(y = outcome, x = rv, c = p$cutoff, p = p$order, kernel = "triangular", h = bw, hide = TRUE),
                  error = function(e){
                    message("rd plot failed with: ", e)
                    return(NULL)})

  #extract info from the rdplot
  if(!is.null(rdp)){
    bin <- data.table(x = rdp$vars_bins$rdplot_mean_x,
                      y = rdp$vars_bins$rdplot_mean_y)
    bin <- bin[x > p$cutoff - bbw & x < p$cutoff + bbw]
    bin[, part := "bin"]

    line <- data.table(x = rdp$vars_poly$rdplot_x,
                       y = rdp$vars_poly$rdplot_y)
    rline <- line[x > p$cutoff][x == max(x) | x == min(x)]
    rline[, part := "r"]
    lline <- line[x < p$cutoff][x == max(x) | x == min(x)]
    lline[, part := "l"]

    result <- rbindlist(list(bin, rline, lline))
  } else {
    warning("fails to produce plot for ", outcol)
    return(NULL)
  }

  return(result)

}
