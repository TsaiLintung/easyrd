setwd("~/Documents/GitHub/easyrd")

library(roxygen2)
library(devtools)

#roxygenise()
#load_all()

source("development/easyrd_sourcever.R")

x <- runif(1000, -1, 1)
x1 <- runif(1000, -1, 1)
y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)
y2 <- 5+3*x+x1+3*(x>=0)+rnorm(1000)
dt <- data.table(x = x, y = y, x1 = x1, y2 = y2)

p <- get_rd_param(running = "x", outcomes = c("y", "y2"), cutoff = 0,
               plot_param = list(kernel = "triangular", p = 1, nbins = c(20, 20)))

rd_result <- easyrd(dt, p)
plot(rd_result)

rd_result$plot$y
rd_result$plot$y2

alt_result <- easyrd(dt, p, "bandwidth", c(0.1, 0.2))
alt_result2 <- easyrd(dt, p, "subsample", c("TRUE", "x1>0.2"))

rd_result <- easyrd(dt, p, result_type = "plot_source")
plot_rd(rd_result$plot_source[outcome == "y"])
plot_rd(rd_result)
plot(alt_result2)
plot(alt_result)