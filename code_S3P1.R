# b)
# F_3 is not marginally calibrated:
plot(function(x) 0.5*(pnorm(x) + 0.5*pnorm(x-1) + 0.5*pnorm(x+1)), xlim = c(-5,5),ylab = "E[F(x)]")
plot(function(x) pnorm(x/sqrt(2)),xlim = c(-5,5),col = "red",add = TRUE)

# c)
n <- 1e4
mu <- rnorm(n)
y <- rnorm(n) + mu
tau <- sample(c(-1, 1), prob=c(.5, .5), size = n, replace = TRUE)

F1 <- function(x) pnorm(x - mu)
F2 <- function(x) pnorm(x / sqrt(2))
F3 <- function(x) 0.5*(pnorm(x - mu) + pnorm(x - mu - tau))
F4 <- function(x) pnorm(x + mu)

par(mfrow = c(2, 2))
hist(F1(y))
hist(F2(y))
hist(F3(y))
hist(F4(y))

marg.calibration.plot <- function(predCDF, xlim = c(-3, 3)) {
  x <- seq(xlim[1], xlim[2], length.out = 101)
  ecdfY <- sapply(x, function(x) mean(y <= x))
  meanpredCDF <- sapply(x, function(x) mean(predCDF(x)))
  plot(x, ecdfY, type="l")
  lines(x, meanpredCDF, col="red")
}

par(mfrow = c(2, 2))
marg.calibration.plot(F1)
marg.calibration.plot(F2)
marg.calibration.plot(F3)
marg.calibration.plot(F4)

# d)
mu <- 0
tau <- 1
quantileF3 <- function(p) {
  optimize(f = function(x) abs(p - F3(x)),
           interval = c(0, 1) + qnorm(p))$minimum
}
length <- c(qnorm(0.95) - qnorm(0.05),
            sqrt(2)*(qnorm(0.95) - qnorm(0.05)),
            quantileF3(0.95) - quantileF3(0.05),
            qnorm(0.95) - qnorm(0.05))
names(length) <- c("F1", "F2", "F3", "F4")
round(length,2)
