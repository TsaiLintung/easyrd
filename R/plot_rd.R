

plot_rd_base <- function(dt, outcol, bin_bw, line_bw, p){

  #get the setting from p
  running <- p$running
  cutoff <- p$cutoff
  order <- p$order


  #default values
  if(is.null(order)){order <- 1}

  rv <- dt[, get(running)]
  outcome <- dt[, get(outcol)]

  #get the rd plot
  rdp <- tryCatch(rdplot(outcome, rv, c = cutoff, p = order, kernel = "triangular", h = line_bw),
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
    line <- rbind(line[x > cutoff][x == max(x) | x == min(x)],
                  line[x < cutoff][x == max(x) | x == min(x)]) #only need the tip si linear
    result <- list(bin = bin, line = line)
  } else {
    result <- list(bin = NA, line = NA)
  }

  return(result)

}
