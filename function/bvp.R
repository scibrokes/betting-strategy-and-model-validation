bvp <- function (x, y = NULL, lambda = c(1, 1, 1), log = FALSE)
  # bvp is the bivariate poisson model, input same length of x and y,
  # then key in lambda1, lambda2, lambda3 values, lambda3 is covariance.
  # exmp: bivp <- bvp(FTHG~1, FTAG~1, lambda = c(lmb1, lmb2, lmb3))

{ if (is.matrix(x)) { var1 <- x[, 1]; var2 <- x[, 2]
} else if (is.vector(x) & is.vector(y)) {
  if (length(x) == length(y)) { var1 <- x; var2 <- y
  } else { stop('lengths of x and y are not equal') }
} else { stop('x is not a matrix or x and y are not vectors') }

n <- length(var1); logbp <- vector(length = n)
for (k in 1:n) {
  x0 <- var1[k]; y0 <- var2[k]; xymin <- min(x0, y0)
  lambdaratio <- lambda[3]/(lambda[1] * lambda[2]); i <- 0:xymin
  sums <- -lgamma(var1[k] - i + 1) - lgamma(i + 1) - 
    lgamma(var2[k] - i + 1) + i * log(lambdaratio)
  maxsums <- max(sums); sums <- sums - maxsums
  logsummation <- log(sum(exp(sums))) + maxsums
  logbp[k] <- -sum(lambda) + var1[k] * log(lambda[1]) + 
    var2[k] * log(lambda[2]) + logsummation }
if (log) { result <- logbp } else { result <- exp(logbp) }
result }
