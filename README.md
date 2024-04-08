# easyrd - Regression Discontinuity Made Easy

**easyrd** is a one-line solution for Regression Discontinuity analysis and its standard checks. It is mostly a wrapper around the **rdrobust** package to facilitate the procedured recommended in [Cattaneo](https://www.cambridge.org/core/elements/abs/practical-introduction-to-regression-discontinuity-designs/F04907129D5C1B823E3DB19C31CAB905) (2019).

# Installation

To install **easyrd** from GitHub, run the following script:

```
# First, ensure devtools is installed
# install.packages("devtools"); library(devtools)

# Install easyrd from GitHub
devtools::install_github("TsaiLintung/easyrd")
```

# Usage

Here’s a basic workflow for the main RD analysis using **easyrd**:

```
# Load the package
library(data.table)
library(ggplot2)
library(easyrd)

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
```

# Features

Conduct standard checks with ease. :

```
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
```

Result can be generated easily with `summary` and `plot`

```
plot(rd_result) #standard rd plot
summary(rd_result) #result table
plot(cutoff_results) #estimates with alternative specifications
```

The source of RD plots can be saved separately for later customization

```
plot_sources <- rd_result$plot_source #plot_sources is a data.table that can be saved as csv easily
plot_rd(plot_sources) + ggtitle("my custom title")
```