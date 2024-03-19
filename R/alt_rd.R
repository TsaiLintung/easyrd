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

