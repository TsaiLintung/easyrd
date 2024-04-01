# simplerd makes RD simple. 

Simplerd provides one-line solutions that simplifies RD analysis and its standards checks. 

It is primarily wrapper around the rdrobust package.

# Installation

You can install **simplerd** from GitHub.

```
# install.packages("devtools")
devtools::install_github("TsaiLintung/simplerd")
```

# Usage

```
# Load the package
library(simplerd)

# simulate some data
x <- runif(1000, -1, 1)
x2 <- runif(1000, -1, 1)
y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)
y2 <- 5+3*x+x1+3*(x>=0)+rnorm(1000)
dt <- data.table(x = x, x2 = x2, y = y, y2 = y2)

#set parameters
outcomes <- c("y", "y2")
running <- "x"
cutoff <- 0
params <- get_param(outcomes, running, cutoff)

# Run the main analysis
main_results <- simplerd(dt, params)

# Do all kinds of alternative specifications
bw_results <- simplerd(dt, params, alt_type = "bandwidth", values = c(-0.1, 0.2))
hetero_results <- simplerd(dt, params, alt_type = "subsample", values = c("x2 > 0", "x2 <= 0"))

# Get data-driven rd plots
main_results$plot$y

# The source of the plot can be extracted separately for style customization
main_plot_source <- simplerd(dt, params, result_type = "plot_source")
plot_rd(main_plot_source) + ggtitle("Custom title")

```
