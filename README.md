# easyrd - Simplifying Regression Discontinuity Analysis

**easyrd** offers one-line solutions to simplify regression discontinuity (RD) analysis and adherence to its standards. At its core, easyrd serves as a wrapper around the **rdrobust** package, enhancing its accessibility and ease of use.

# Installation

To install **easyrd**, use the following commands in R. This package is hosted on GitHub, and devtools is required for installation:

```
# First, ensure devtools is installed
# install.packages("devtools")

# Install easyrd from GitHub
devtools::install_github("TsaiLintung/easyrd")
```

# Usage

**easyrd** simplifies the process of conducting RD analysis, from data simulation to analysis and visualization. Here’s a basic workflow:

```
# Load the package
library(easyrd)

# Simulate some example data
x <- runif(1000, -1, 1)
x2 <- runif(1000, -1, 1)
y <- 5 + 3*x + x2 + 2*(x >= 0) + rnorm(1000)
y2 <- 5 + 3*x + x2 + 3*(x >= 0) + rnorm(1000)
dt <- data.table(x = x, x2 = x2, y = y, y2 = y2)

# Set parameters for RD analysis
outcomes <- c("y", "y2")
running <- "x"
cutoff <- 0
params <- get_param(outcomes, running, cutoff)

# Execute the main RD analysis
rd_result <- easyrd(dt, params)
plot(rd_result)
summary(rd_result)

```

Explore alternative specifications with ease:

```
# Analyze with different bandwidths
bw_results <- easyrd(dt, params, alt_type = "bandwidth", values = c(0.1, 0.2))
summary(bw_results)

# Perform subsample analysis
hetero_results <- easyrd(dt, params, alt_type = "subsample", values = c("x2 > 0", "x2 <= 0"))
```
