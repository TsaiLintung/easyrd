setwd("~/GitHub/simplerd")

library(devtools)
library(tinytest)
library(roxygen2)

library(data.table)
library(rdrobust)

load_all()

x <- runif(1000, -1, 1)
x1 <- runif(1000, -1, 1)
y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)
y2 <- 5+3*x+x1+3*(x>=0)+rnorm(1000)
dt <- data.table(x = x, y = y, x1 = x1, y2 = y2)

p <- get_param(running = "x", outcomes = c("y", "y2"), cutoff = 0)

rd_result <- simplerd(dt, p)

rd_result$plot$y
rd_result$plot$y2

alt_result <- simplerd(dt, p, "bandwidth", c(0.1, 0.2), result_type = "estimate")
alt_result2 <- simplerd(dt, p, "subsample", c("TRUE", "x1>0.2"))
