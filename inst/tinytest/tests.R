# setup ---------------------------------------

x <- runif(1000, -1, 1)
x1 <- runif(1000, -1, 1)
y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)
y2 <- 5+3*x+x1+3*(x>=0)+rnorm(1000)
dt <- data.table(x = x, y = y, x1 = x1, y2 = y2)

p <- get_param(running = "x", outcomes = c("y", "y2"), cutoff = 0)

# simplerd basic ------------------------------------------------------------

# Assuming you have some mock data 'dt' and parameters 'p' defined

# Test that simplerd returns correct types
results <- simplerd(dt, p)
expect_equal(typeof(results), "list")
expect_true("estimate" %in% names(results))
expect_true("plot" %in% names(results))

# Test that simplerd handles alternative types correctly
results <- simplerd(dt, p, alt_type = "subsample", values = c("x1>0", "x1<=0"))
expect_equal(nrow(results$estimate), 4) #2 outcome * 2 specs
expect_equal(results$estimate$type, rep("subsample", nrow(results$estimate)))
expect_true(results$estimate[outcome == "y2" & value == "x1<=0", coef] != results$estimate[outcome == "y2" & value == "x1>0", coef])

# Test that simplerd handles alternative types correctly
results <- simplerd(dt, p, alt_type = "cutoff", values = c(-0.1, 0.1))
expect_equal(nrow(results$estimate), 4) #2 outcome * 2 specs
expect_equal(results$estimate$type, rep("cutoff", nrow(results$estimate)))
expect_true(results$estimate[outcome == "y2" & value == -0.1, coef] != results$estimate[outcome == "y2" & value == 0.1, coef])


# Test verbose flag
expect_message(simplerd(dt, p, verbose = TRUE))

# Test plot from source
results <- simplerd(dt, p, result_type = "plot_source")
expect_equal(class(plot_rd(results$plot_source[outcome == "y"])), c("gg", "ggplot"))
# param basic -----------------------------------------------------------------

# Test that get_param returns a list with correct default values
params <- get_param(
  outcomes = c("test_score", "graduation_rate"),
  running = "age",
  cutoff = 18,
  order = 2
)

expect_equal(typeof(params), "list")
expect_equal(params$cutoff, 18) #specified
expect_equal(params$vce, "hc1") #default
expect_null(params$covariate) #default null
expect_equal(params$order, 2) #specified non-default
