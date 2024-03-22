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

expect_equal(nrow(results$estimate), length(unique(p$outcomes)) * length(c("x1>0", "x<=0")))
expect_equal(results$estimate$type, rep("subsample", nrow(results$estimate)))

# Test verbose flag
expect_silent(simplerd(dt, p, verbose = FALSE))

# param basic -----------------------------------------------------------------

# Test that get_param returns a list with correct default values
params <- get_param(
  outcomes = c("test_score", "graduation_rate"),
  running = "age",
  cutoff = 18
)

expect_equal(typeof(params), "list")
expect_equal(params$vce, "hc1")
expect_equal(params$est, "robust")
expect_equal(params$order, 1)
expect_null(params$bandwidth)
expect_null(params$bin_bandwidth)
expect_null(params$covariate)

# Test that get_param correctly sets non-default parameters
params <- get_param(
  outcomes = c("test_score"),
  running = "age",
  cutoff = 18,
  vce = "bootstrap",
  est = "conventional",
  order = 2,
  bandwidth = 5,
  bin_bandwidth = 0.5,
  covariate = c("gender", "income")
)

expect_equal(params$vce, "bootstrap")
expect_equal(params$est, "conventional")
expect_equal(params$order, 2)
expect_equal(params$bandwidth, 5)
expect_equal(params$bin_bandwidth, 0.5)
expect_equal(params$covariate, c("gender", "income"))
