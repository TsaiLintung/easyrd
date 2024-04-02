#' Draw RD
#'
#' draw the RD plot from the RD source
#'
#' @param ps plot source or easy rd plot object
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
    labs(y = head[,outcome],
         title = "RD plot", subtitle = subtt)
  return(plot)
}
