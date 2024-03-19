#' Alternative RD Estimation Approaches
#'
#' This function extends the RD estimation capabilities by allowing for alternative specifications such as subgroup analysis, donut hole removal, or other specification changes. It dynamically applies different settings based on the 'type' parameter and iterates through the provided 'specs' to conduct RD estimations accordingly. The function leverages the 'run_rd' function for the core RD estimation logic.
#'
#' @param dt A data.table object containing the data to be used for RD estimation.
#' @param p A list containing the base settings for the RD estimation as expected by the 'run_rd' function.
#' @param type A character string indicating the type of alternative RD estimation to perform. Possible values include "subgroup" for subgroup analysis, "donut" for donut RD estimation, or any other parameter name from the 'p' list to modify.
#' @param specs A vector specifying the different specifications to apply for the 'type' of RD estimation. For example, if 'type' is "subgroup", 'specs' should be a list of subgroup definitions; if 'type' is "donut", it should be a list of donut radius values.
#' @param verbose A logical flag indicating whether to print messages about the RD estimation process and results for each specification. Default is TRUE.
#' @param ... Additional arguments passed to the 'run_rd' function or other functions called within.
#'
#' @return Returns a data.table that aggregates the RD estimation results across all specifications applied, with an additional column 'note' indicating the specific setting for each result.
#' @examples
#' # Define base parameters
#' base_params <- list(outcomes = c("outcome1"),
#'                     running = "running_var",
#'                     cutoff = 0.5,
#'                     order = 1,
#'                     bandwidth = 0.75)
#'
#' # Apply subgroup analysis
#' subgroup_specs <- c("group_var == 1", "group_var == 2")
#' results_subgroup <- alt_rd(my_data_table, base_params, "subgroup", subgroup_specs)
#'
#' # Apply donut RD analysis
#' donut_specs <- c(0.1, 0.2)
#' results_donut <- alt_rd(my_data_table, base_params, "donut", donut_specs)
#'
#' @export
alt_rd <- function(dt, p, type, specs, verbose = TRUE, ...){

  if(type == "subgroup"){

    specs <- c("all", specs)
    all <- TRUE
    altfunc <- function(subgroup){
      dts <- dt[eval(str2lang(subgroup))]
      subgroup_result <- run_rd(dts, p, verbose, ...)
      subgroup_result[, note := paste0("subgroup",":",subgroup)]
      return(subgroup_result)
    }

  } else if(type == "donut"){
    altfunc <- function(donut){
      dtd <- dt[get(p$running) > p$cutoff + donut | get(p$running) < p$cutoff - donut]
      donut_result <- run_rd(dtd, p, verbose, ...)
      donut_result[, note := paste0("donut",":",donut)]
      return(donut_result)
    }
  } else {
    altfunc <- function(spec){
      p_alt <- p
      p_alt[[type]] <- spec
      alt_result <- run_rd(dt, p_alt, verbose, ...)
      alt_result[, note := paste0(type,":",spec)]
      return(alt_result)
    }
  }

  alt_results <- rbindlist(lapply(specs, altfunc))

  return(alt_results)

}

