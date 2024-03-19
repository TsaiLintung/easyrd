


x <- runif(1000, -1, 1)
x1 <- runif(1000, -1, 1)
y <- 5+3*x+x1+2*(x>=0)+rnorm(1000)

dt <- data.table(x = x, y = y, x1 = x1)

p <- list()
p$outcomes <- "y"
p$running <- "x"
p$cutoff <- 0

rd_result <- run_rd(dt, p)


alt_result <- alt_rd(dt, p, "bandwidth", c(0.1, 0.2))
alt_result <- alt_rd(dt, p, "subgroup", c("x1>0.2"))

result <- get_rd_source(dt, "y", 0.3, 0.2, p)
