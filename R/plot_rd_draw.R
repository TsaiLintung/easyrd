plot_rd_draw <- function(ps){
  bin <- ps$bin
  lline <- ps$lline
  rline <- ps$rline
  plot <- ggplot() + geom_point(aes(x = x, y = y), data = bin) +
    geom_line(aes(x = x, y = y), data = lline) +
    geom_line(aes(x = x, y = y), data = rline) +
    labs(x = ps$running, y = ps$outcome,
         title = "RD plot", subtitle = paste0(ps$type, ": ", ps$value))
  return(plot)
}
