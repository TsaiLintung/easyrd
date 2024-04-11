setwd("~/GitHub/easyrd")

library(data.table)
library(ggplot2)
library(devtools)
load_all()

rm(list = ls())
gc()

# Simulate some example data
x0 <- runif(1000, -1, 1)
x <- runif(1000, -1, 1)
x2 <- runif(1000, -1, 1)
y <- 5 + 3*x + x2 + 2*(x >= 0) + rnorm(1000)
y2 <- 5 + 3*x + x2 + 3*(x >= 0) + rnorm(1000)
dt <- data.table(x = x, x2 = x2, y = y, y2 = y2, x0 = x0)

# Set parameters for RD analysis
outcomes <- c("y", "y2")
running <- "x"
cutoff <- 0
params <- get_param(outcomes, running, cutoff)

# Execute the main RD analysis
rd_result <- easyrd(dt, params)

#Validity checks

# Predetermined Covariates and Placebo Outcomes
cov_params <- params
cov_params$outcomes <- c("x0") #
cov_result <- easyrd(dt, cov_params)

# Placebo cutoffs
cutoff_results <- easyrd(dt, params, alt_type = "cutoff", values = c(-0.1, 0.1))

# Density check is not included in easyrd but can be done easily with rddensity:
# require(rddensity)
# density_check <- rddensity(dt[,x], cutoff)

#Sensitivity checks

# Sensitivity to Observations near the Cutoff (donut)
donut_results <- easyrd(dt, params, alt_type = "donut", values = c(0.01, 0.02, 0.03))

# Sensitivity to Bandwidth Choice
bandwidth_results <- easyrd(dt, params, alt_type = "bandwidth", values = c(0.1, 0.2, 0.3))

plot(rd_result) #standard rd plot
summary(rd_result) #result table
plot(cutoff_results) #estimates with alternative specifications

plot_sources <- rd_result$plot_source #plot_sources is a data.table that can be saved as csv easily
plot_rd(plot_sources) + ggtitle("my custom title")



