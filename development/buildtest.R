setwd("~/GitHub/simplerd")

library(devtools)
library(tinytest)
library(roxygen2)

rm(list = ls())

roxygenise()


load_all()


build()

