#' Generate Parameters for RD Analysis
#'
#' Generates a list of parameters for regression discontinuity (RD) analysis based on user-defined inputs. 
#' It is primarily used to set up the necessary parameters before conducting an RD analysis.
#'
#' @param outcomes A character vector specifying the outcome variables for the RD analysis.
#' @param running A string indicating the running variable used in the RD analysis.
#' @param cutoff A numeric value specifying the cutoff point for the running variable in the RD analysis.
#' @param bandwidth A numeric value specifying the bandwidth for data used, default is "NULL"
#' @param donut A numeric value specifying the size of the donut hole, default is -1
#' @param covs the vector of column names of variable used as covariates, default is NULL
#' @param cluster the name of the column name of variable used for clustering standard error, default is NULL
#' @param est A string indicating the estimator to be used. Default is "robust".
#' @param bin_bandidth_ratio A numeric to adjust the size of the bin relative to the local regression line, NULL value just show all data, default is 1
#' @param est_param A list of parameters to be passed to rdrobust, default is list(vce = "hc1")
#' @param plot_param A list of parameters to be passed to rdplot, default is list(kernel = "triangular", p = 1)
#'
#' @return A list containing the specified parameters for the RD analysis.
#' @export
get_rd_param <- function(outcomes, running, cutoff,
                      donut = -1, bandwidth = NULL, covs = NULL, cluster = NULL,
                      est = "robust", bin_bandidth_ratio = 1,
                      est_param = list(vce = "hc1"), 
                      plot_param = list(kernel = "triangular", p = 1)){
  p <- as.list(environment())
  class(p) <- "easyrd_param"
  return(p)

}

# generics -------------------------------

#' @export
print.easyrd_result <- function(x,...){
  getstar <- function(p){ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))}
  
  dt <- copy(x$estimate)
  dt[, spec := paste0(type, value)]
  dt[, sig := getstar(pvalue)]
  
  if(dt[, uniqueN(spec)] == 1){
    print(dt[,.(outcome, coef, se, pvalue, sig)], class = FALSE)
  } else {
    t <- dt[, first(type)]
    setorder(dt, outcome, value)
    setnames(dt, "value", t)
    cat("result with alternative", t, "\n")
    for(out in dt[, unique(outcome)]){
      cat(paste0(out, "\n"))
      print(dt[outcome == out & type == t,.SD, .SDcols = c(t, "coef", "se", "pvalue", "sig")], class = FALSE)
      cat("\n")
    }
  }

  invisible(x)
}

#' @export
plot.easyrd_result <- function(x,...){
  if(!x$alt_type=="main"){
    return(plot_alt_rd(x$estimate))
  } else {
    return(plot_rd(x$plot_source))
  }
}

# deal with globals ---------

NULL

# quiets concerns of R CMD check re: the .'s that appear in pipelines and data.table variables
utils::globalVariables(c("type", "value", "coef", "se", "part", "x", "y", "spec", "sig", "pvalue", ".", "outcome"))