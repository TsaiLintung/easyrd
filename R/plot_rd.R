#' Draw RD
#'
#' draw the RD plot from the RD source
#'
#' @param ps plot source or easy rd plot object or a data.table from result$plot_source
#'
#' @return ggplot
#' @export
plot_rd <- function(ps){

  if("easyrd_result" %in% class(ps)){ps <- ps$plot_source}
  if(!is.data.table(ps)){stop("invalid plot source")}

  head <- ps[1, ]
  subtt <- head[,type]
  if(head[,type] != "main"){subtt <- paste0(subtt, ": ", head[,value])}
  bin <- ps[part == "bin"]
  lline <- ps[part == "l"]
  rline <- ps[part == "r"]
  plot <- ggplot() + geom_point(aes(x = x, y = y), data = bin) +
    geom_line(aes(x = x, y = y), data = lline) +
    geom_line(aes(x = x, y = y), data = rline) +
    labs(y = "estimate", title = "RD plot", subtitle = subtt)  + facet_wrap(~outcome, scales = "free")
  return(plot)
}

#' Draw Alt RD
#'
#' draw the RD plot from the RD source with alternative specification
#'
#' @param dt a data.table from result$estimate
#'
#' @return ggplot
#' @export
plot_alt_rd <- function(dt){
  if(dt[, uniqueN(type)] > 1){stop("more than one alternation type")}
  qn <- qnorm(0.975)
  alt_type <- dt[1,type]
  plot <-  dt |> ggplot() + geom_point(aes(x = value, y = coef)) + geom_errorbar(aes(x = value, ymax = coef + se*qn, ymin = coef - se*qn)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    labs(title = paste0("RD estimate with alternative ", alt_type), xlab = alt_type, ylab = "") + facet_wrap(~outcome)
  return(plot)
}

plot_rd_source <- function(dt, p, outcol){
  
  #get cols
  rv <- dt[, get(p$running)]
  outcome <- dt[, get(outcol)]
  
  #default values
  if(is.null(p$bandwidth)){
    bwresult <- rdbwselect(y = outcome, x = rv, c = p$cutoff, p = p$plot_param$p, q = p$plot_param$q, vce = "hc1")
    bw <- bwresult$bws[1]
  } else {
    bw <- p$bandwidth
  }

  if(!is.null(p$bin_bandidth_ratio)){
    keep <- rv > p$cutoff - bw*p$bin_bandidth_ratio & rv < p$cutoff + bw*p$bin_bandidth_ratio
    outcome <- outcome[keep]
    rv <- rv[keep]
  }
    
  #get the rd plot
  rdplot_param <- c(list(y = outcome, x = rv, c = p$cutoff, hide = TRUE, h = bw), p$plot_param)
  rdp <- tryCatch(do.call(rdplot, rdplot_param),
                  error = function(e){
                    message("rd plot failed with: ", e)
                    return(NULL)})
  
  #extract info from the rdplot
  if(!is.null(rdp)){
    bin <- data.table(x = rdp$vars_bins$rdplot_mean_x,
                      y = rdp$vars_bins$rdplot_mean_y)
    
    bin[, part := "bin"]
    
    line <- data.table(x = rdp$vars_poly$rdplot_x,
                       y = rdp$vars_poly$rdplot_y)
    
    #only need to keep the tips if is linear
    if(p$plot_param$p == 1){
      rline <- line[x > p$cutoff][x == max(x) | x == min(x)]
      rline[, part := "r"]
      lline <- line[x < p$cutoff][x == max(x) | x == min(x)]
      lline[, part := "l"]
    } else {
      rline <- line[x > p$cutoff]
      rline[, part := "r"]
      lline <- line[x < p$cutoff]
      lline[, part := "l"]
    }

    
    result <- rbindlist(list(bin, rline, lline))
  } else {
    warning("fails to produce plot for ", outcol)
    return(NULL)
  }
  
  return(result)
  
}
