#' Simple RD Analysis
#'
#' This function simplifies regression discontinuity (RD) analysis results by providing estimates and plots based on the specified alternative specifications and values.
#' It allows for different types of RD analyses including main, subsample, and donut approaches.
#'
#' @param data A data.table containing the data for RD analysis.
#' @param p A list containing parameters for the RD analysis, typically including outcomes and other necessary information.
#' @param alt_type A string that specifies the type of alternative analysis to be conducted. Valid types are NULL (defaults to "main"), "subsample", and "donut". This argument determines how the data is manipulated before analysis.
#' @param values A vector of values corresponding to the alt_type specification. These are used to modify the dataset or parameters based on the type of RD analysis being conducted.
#' @param result_type A character vector indicating the types of results to return. The default is c("estimate", "plot"), meaning both estimates and plots will be generated. Can also include "plot_source" for the plot data.
#' @param verbose A boolean flag indicating whether to print messages for each result produced. Defaults to FALSE
#' @param ... Additional arguments passed to the estimation function est_rd.
#'
#' @return A list containing the analysis results. This includes 'estimate' (data.table of estimates), 'plot_source' (list of data for plots), and 'plot' (list of generated plots), depending on the specified result_type.
#'
#' @import data.table ggplot2 rdrobust dreamerr
#' @export


simplerd <- function(data, p,
                     alt_type = NULL,
                     values = c(),
                     result_type = c("estimate", "plot"),
                     verbose = FALSE, ...){

  #argument validation
  if(class(p) != "rdsimple_param"){stop("please generate the parameter with get_param")}
  if(!is.data.table(data)){data <- as.data.table(data)}

  check_set_arg(alt_type, "NULL | match", .choices = c("cutoff", "vce", "est", "order", "bandwidth", "covariate", "subsample"))
  check_set_arg(result_type, "multi match", .choices = c("estimate", "plot", "plot_source"))

  #if just the main result
  if(is.null(alt_type)){
    alt_type <- "main"
    values <- NA
  }

  #multiple spec
  estimates <- data.table()
  plot_sources <- data.table()
  plots <- list()
  outcomes <- p$outcomes
  for(value in values){

    dts <- data
    ps <- p

    #get the spec
    if(alt_type != "main"){
      if(alt_type == "subsample"){
        dts <- dts[eval(str2lang(value))]
      }
      else if(alt_type == "donut"){
        dts <- dts[get(p$running) > p$cutoff + value | get(p$running) < p$cutoff - value]
      } else {
        ps[[alt_type]] <- value
      }
    }

    #get results for each outcome
    for(outcol in outcomes){

      #main estimate
      if("estimate" %in% result_type){
        estimate <- est_rd(dts, p, outcol, ...)
        estimate[, `:=`(type = alt_type, value = value, outcome = outcol)]
        estimates <- rbind(estimate, estimates)
      }

      if("plot_source" %in% result_type | "plot" %in% result_type){

        #source for rdplot
        plot_source <- plot_rd_source(dts, p, outcol)
        plot_source[, `:=`(type = alt_type, value = value, outcome = outcol)]

        if("plot" %in% result_type){
          plot <- plot_rd(plot_source)
          plots[[outcol]] <- plot
        }

      }

      if(verbose){
        message("produced result for outcome: ", outcol, ", ", alt_type, "=", value)
      }

    }

  }

  #assemble the result
  result <- list(estimate = estimates, plot_source = plot_source, plot = plots)
  class(result) <- "simplerd_result"


  return(result)

}
