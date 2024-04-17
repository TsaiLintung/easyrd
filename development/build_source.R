rm(list = ls())
gc()

library(stringr)
library(here)


source_files <- list.files(here("R"), include.dirs = FALSE, full.names = TRUE)

ver <- "0.9.0"
vername <- "new start"
dep <- c("data.table", "dreamerr", "ggplot2", "rdrobust")

sink(here("development/source_head.R"))
cat(paste0("#", as.character(Sys.Date()), "\n"))
cat(paste0("message('loading easyrd source ver. ver: ", ver, " (", vername ,"), date: " , as.character(Sys.Date()), "')\n"))
cat(paste0("require(", dep, ");") |> str_flatten())
sink()

sink(paste0("development/easyrd_", str_replace_all(ver, "\\.", "_"), ".R"))
cat(here("development/source_head.R"), sep ="\n")
for(file in source_files){
  current_file = readLines(file)
  cat(current_file, sep ="\n")
  cat("\n")
}

sink()
