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
    labs(y = "estimate", title = "RD plot", subtitle = subtt)  + facet_wrap(~outcome)
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
    labs(title = paste0("RD estimate with alternative ", alt_type), xlab = alt_type, ylab = "") + facet_wrap(~outcome)
  return(plot)
}