# easyrd - Regression Discontinuity Made Easy

**easyrd** is a one-line solution for Regression Discontinuity analysis and its standard checks. It is mostly a wrapper around the **rdrobust** package to facilitate the procedures recommended in [Cattaneo](https://www.cambridge.org/core/elements/abs/practical-introduction-to-regression-discontinuity-designs/F04907129D5C1B823E3DB19C31CAB905) (2019).

# Installation

To install **easyrd** from GitHub, run the following script:

```
# First, ensure remotes is installed
# install.packages("remotes"); library(remotes)

# Install easyrd from GitHub
remotes::install_github("TsaiLintung/easyrd")
```

# Usage

Here is a basic workflow for the main RD analysis using **easyrd**:

```
# Load the package
library(data.table)
library(ggplot2)
library(easyrd)

# Simulate some example data
n <- 10000
dt <- data.table(x = runif(n, -1, 1), x2 = runif(n, -1, 1), x0 = runif(n, -1, 1),
                 y = 5 + 3*x + x2 + 2*(x >= 0) + rnorm(n),
                 y2 = 5 + 3*x + x2 + 3*(x >= 0) + rnorm(n))

# Set parameters for RD analysis
params <- get_rd_param(c("y", "y2"), "x", 0)

# Execute the main RD analysis
rd_result <- easyrd(dt, params)
rd_result #see the results!
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

Result can be generated easily with `print` and `plot`

```
plot(rd_result) #standard rd plot
print(rd_result) #result table
plot(cutoff_results) #estimates with alternative specifications
print(cutoff_results) #estimates with alternative specifications
```

The source of RD plots can be saved separately for later customization

```
plot_sources <- rd_result$plot_source #plot_sources is a data.table that can be saved as csv easily
plot_rd(plot_sources) + ggtitle("my custom title")
```