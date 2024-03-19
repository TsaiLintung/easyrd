

density <- function(){
  #CJM test
  mctest <- rddensity(adult_inv[,CHO], 199.5, vce = "plugin")
  summary(mctest)


  #the data for density plot
  counts <- adult_inv[, .(count = .N), by = CHO]
  counts[count > 5] |> fwrite(paste0(output_dir, "density.csv"))
}

