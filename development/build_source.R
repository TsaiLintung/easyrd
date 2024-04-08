rm(list = ls())
gc()

setwd("~/GitHub/easyrd")

source_files <- list.files("R", include.dirs = FALSE, full.names = TRUE)

ver <- "0.9.0"
vername <- "new start"

sink("development/easyrd_sourcever.R")
cat(paste0("#", as.character(Sys.Date()), "\n"))
cat(paste0("message('loading easyrd source ver. ver: ", ver, " (", vername ,"), date: " , as.character(Sys.Date()), "')\n"))
cat("require(data.table);require(dreamerr);require(ggplot2);require(rdrobust)")
for(file in source_files){
  current_file = readLines(file)
  cat(current_file, sep ="\n")
  cat("\n")
}

sink()
