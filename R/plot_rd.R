plot_rd <- function(dt, result, bin_bw, p){
  
  rdbwselect
  
}

get_rd_source <- function(dt, outcol, bin_bw, line_bw, p){

  #get the setting from p
  running <- p$running
  cutoff <- p$cutoff
  order <- p$order

  #default values
  if(is.null(order)){order <- 1}

  rv <- dt[, get(running)]
  outcome <- dt[, get(outcol)]

  #get the rd plot
  rdp <- tryCatch(rdplot(outcome, rv, c = cutoff, p = order, kernel = "triangular", h = line_bw, hide = TRUE),
                         error = function(e){
                           message("rd plot failed with: ", e)
                           return(NULL)
                         })

  #extract info from the rdplot
  if(!is.null(rdp)){
    bin <- data.table(x = rdp$vars_bins$rdplot_mean_x,
                       y = rdp$vars_bins$rdplot_mean_y)
    bin <- bin[x > cutoff - bin_bw & x < cutoff + bin_bw]

    line <- data.table(x = rdp$vars_poly$rdplot_x,
                       y = rdp$vars_poly$rdplot_y)
    rline <- line[x > cutoff][x == max(x) | x == min(x)]
    lline <- line[x < cutoff][x == max(x) | x == min(x)]
    result <- list(bin = bin, rline = rline, lline = lline)
  } else {
    result <- list(bin = NA, line = NA)
  }

  return(result)

}

draw_rd <- function(result){
  bin <- result$bin
  lline <- result$lline
  rline <- result$rline
  plot <- ggplot() + geom_point(aes(x = x, y = y), data = bin) + 
    geom_line(aes(x = x, y = y), data = lline) +
    geom_line(aes(x = x, y = y), data = rline) 
  return(plot)
}