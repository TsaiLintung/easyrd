#' Draw RD
#'
#' draw the RD plot from the RD source
#'
#' @param ps plot source object
#'
#' @return ggplot
#' @export
plot_rd <- function(ps){
  head <- ps[1, ]
  bin <- ps[part == "bin"]
  lline <- ps[part == "l"]
  rline <- ps[part == "r"]
  plot <- ggplot() + geom_point(aes(x = x, y = y), data = bin) +
    geom_line(aes(x = x, y = y), data = lline) +
    geom_line(aes(x = x, y = y), data = rline) +
    labs(y = head[,outcome],
         title = "RD plot", subtitle = paste0(head[,type], ": ", head[,value]))
  return(plot)
}
