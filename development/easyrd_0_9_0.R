#2024-04-19
message('loading easyrd source ver. ver: 0.9.0 (new start), date: 2024-04-19')
require(data.table);require(dreamerr);require(ggplot2);require(rdrobust);
#' Easy RD Analysis
#'
#' This function simplifies regression discontinuity (RD) analysis results by providing estimates and plots based on the specified alternative specifications and values.
#' It allows for different types of RD analyses including main, subsample, and donut approaches.
#'
#' @param data A data.table containing the data for RD analysis.
#' @param p A list containing parameters for the RD analysis, typically including outcomes and other necessary information.
#' @param alt_type A string that specifies the type of alternative analysis to be conducted. Valid types are NULL (defaults to "main"), "subsample", and "donut". This argument determines how the data is manipulated before analysis.
#' @param values A vector of values corresponding to the alt_type specification. These are used to modify the dataset or parameters based on the type of RD analysis being conducted.
#' @param result_type A character vector indicating the types of results to return. The default is c("estimate", "plot_source", "plot") for getting all results. 
#' @param verbose A boolean flag indicating whether to print messages for each result produced. Defaults to FALSE
#' @param ... Additional arguments passed to the estimation function est_rd.
#'
#' @return A list containing the analysis results. This includes 'estimate' (data.table of estimates), 'plot_source' (list of data for plots), and 'plot' (list of generated plots), depending on the specified result_type.
#'
#' @import data.table ggplot2 rdrobust dreamerr
#' @export


easyrd <- function(data, p,
                   alt_type = NULL,
                   values = c(),
                   result_type = c("estimate", "plot_source", "plot"),
                   verbose = FALSE, ...){

  #argument validation
  if(class(p) != "easyrd_param"){stop("please generate the parameter with get_param")}
  if(!is.data.table(data)){data <- as.data.table(data)}

  check_set_arg(alt_type, "NULL | match", .choices = c("cutoff", "vce", "est", "order", "bandwidth", "covariate", "subsample", "donut"))
  check_set_arg(result_type, "multi match", .choices = c("estimate", "plot", "plot_source"))
  
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
        estimate <- est_rd(dts, ps, outcol, ...)
        estimate[, `:=`(type = alt_type, value = value, outcome = outcol)]
        setcolorder(estimate, c("outcome", "type", "value", "coef", "se", "pvalue", "llim", "rlim", "bw"))
        estimates <- rbind(estimate, estimates)
      }

      if("plot_source" %in% result_type | "plot" %in% result_type){

        #source for rdplot
        plot_source <- plot_rd_source(dts, ps, outcol)
        plot_source[, `:=`(type = alt_type, value = value, outcome = outcol)]
        plot_sources <- rbind(plot_source, plot_sources)

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
  result <- list(estimate = estimates,
                 plot_source = plot_sources,
                 plot = plots,
                 params = p,
                 alt_type = alt_type,
                 values = values)
  class(result) <- "easyrd_result"


  return(result)

}

est_rd <- function(dt, p, outcol, ...){

  # get vectors
  rv <- dt[, get(p$running)]
  outcome <- dt[, .SD, .SDcols = outcol] |> unlist()
  if(!is.null(p$covariate)){
    cov <- dt[, .SD, .SDcols = p$covariate]
  } else {cov <- NULL}

  #estimation
  rd_est <- tryCatch(suppressWarnings(rdrobust(y = outcome , x = rv,
                                               c = p$cutoff,
                                               p = p$order, h = p$bandwidth, vce = p$vce, covs = cov, ...)),
                     error = function(e){
                       message("failed for ", outcol, " message: ", e)
                       return(NULL)
                     })

  #post process
  if(is.null(rd_est)){
    ests <- data.table(coef = NA, se = NA, pvalue = NA, llim = NA, rlim = NA, bw = NA)
  } else {

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

#' Generate Parameters for RD Analysis
#'
#' This function generates a list of parameters for regression discontinuity (RD) analysis based on user-defined inputs. It is primarily used to set up the necessary parameters before conducting an RD analysis.
#'
#' @param outcomes A character vector specifying the outcome variables for the RD analysis.
#' @param running A string indicating the running variable used in the RD analysis.
#' @param cutoff A numeric value specifying the cutoff point for the running variable in the RD analysis.
#' @param vce A string specifying the variance-covariance estimator to be used. Default is "hc1".
#' @param est A string indicating the estimation method to be used. Default is "robust".
#' @param order An integer specifying the order of the polynomial to fit in the RD design. Default is 1 (linear).
#' @param bandwidth A numeric specifying the bandwidth around the cutoff. If NULL, it will be calculated automatically based on the data and method. Default is NULL.
#' @param bin_bandwidth A numeric value used to specify the bandwidth for binning data in the graphical representation of the RD design. If NULL, it will be determined automatically. Default is NULL.
#' @param covariate A vector of strings representing any covariates to be included in the RD analysis. Default is NULL.
#'
#' @return A list containing the specified parameters for the RD analysis.
#'
#' @examples
#' params <- get_param(
#'   outcomes = c("test_score", "graduation_rate"),
#'   running = "age",
#'   cutoff = 18,
#'   vce = "hc1",
#'   est = "robust",
#'   order = 2,
#'   bandwidth = 5,
#'   bin_bandwidth = 0.5,
#'   covariate = c("gender", "income")
#' )
#'
#' @export
get_param <- function(outcomes, running, cutoff,
                      vce = "hc1", est = "robust", order = 1, donut = -1,
                      bandwidth = NULL, bin_bandwidth = NULL, covariate = NULL){

  p <- list()
  p$outcomes <- outcomes
  p$running <- running
  p$cutoff <- cutoff
  p$vce <- vce
  p$est <- est
  p$order <- order
  p$bandwidth <- bandwidth
  p$bin_bandwidth <- bin_bandwidth
  p$covariate <- covariate
  p$donut <- donut

  class(p) <- "easyrd_param"

  return(p)

}

# summary function for the simplerd --------------------------------------------

#' The result from easy rd
setClass("easyrd_result")

#' Summarize the result from easyrd
#'
#' @param object A easyrd_result object
#' @export
setMethod("summary", signature(object = "easyrd_result"), function(object){

  getstar <- function(p){ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))}

  dt <- object$estimate
  dt[, spec := paste0(type, ": ", value)]
  dt[, sig := getstar(pvalue)]
  for(sp in dt[, unique(spec)]){
    cat(paste0(sp, "\n"))
    print(dt[spec == sp,.(outcome, coef, se, pvalue, sig)], class = FALSE)
    cat("\n")
  }

})

#' Plot the result from easyrd
#'
#' @param x A easyrd_result object
#' @export
setMethod("plot", signature(x = "easyrd_result"), function(x){
  if(!x$alt_type=="main"){
    return(plot_alt_rd(x$estimate))
  } else {
    return(plot_rd(x$plot_source))
  }
  
})

