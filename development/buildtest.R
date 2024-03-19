setwd("~/GitHub/simplerd")

library(devtools)
library(tinytest)
library(roxygen2)

load_all()

roxygenise()
build()
