# setup ---------------------------------------

x <- runif(1000, -1, 1)
x1 <- runif(1000, -1, 1)
y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)
y2 <- 5+3*x+x1+3*(x>=0)+rnorm(1000)
dt <- data.table(x = x, y = y, x1 = x1, y2 = y2)

p <- get_rd_param(running = "x", outcomes = c("y", "y2"), cutoff = 0)

# easyrd basic ------------------------------------------------------------

# Assuming you have some mock data 'dt' and parameters 'p' defined

# Test that easyrd returns correct types
results <- easyrd(dt, p)
expect_equal(typeof(results), "list")

expect_true("estimate" %in% names(results))
expect_true("plot" %in% names(results))

# Test that easyrd handles alternative types correctly
results <- easyrd(dt, p, alt_type = "subsample", values = c("x1>0", "x1<=0"))
expect_equal(nrow(results$estimate), 4) #2 outcome * 2 specs
expect_equal(results$estimate$type, rep("subsample", nrow(results$estimate)))
expect_true(results$estimate[outcome == "y2" & value == "x1<=0", coef] != results$estimate[outcome == "y2" & value == "x1>0", coef])

# Test that easyrd handles alternative types correctly
results <- easyrd(dt, p, alt_type = "cutoff", values = c(-0.1, 0.1))
expect_equal(nrow(results$estimate), 4) #2 outcome * 2 specs
expect_equal(results$estimate$type, rep("cutoff", nrow(results$estimate)))
expect_true(results$estimate[outcome == "y2" & value == -0.1, coef] != results$estimate[outcome == "y2" & value == 0.1, coef])

# Test verbose flag
expect_message(easyrd(dt, p, verbose = TRUE))

# Test plot from source
results <- easyrd(dt, p, result_type = "plot_source")
expect_equal(class(plot_rd(results$plot_source[outcome == "y"])), c("gg", "ggplot"))

rm(results)

# plot ----------------------------------------

results <- easyrd(dt, p)
expect_equal(class(plot(results)), c("gg", "ggplot"))
expect_stdout(print(results), "outcome")

alt_results <- easyrd(dt, p, alt_type = "cutoff", values = c(-0.1, 0.1))
expect_equal(class(plot(alt_results)), c("gg", "ggplot"))
expect_stdout(print(alt_results), "result")

# summary --------------------------------------
