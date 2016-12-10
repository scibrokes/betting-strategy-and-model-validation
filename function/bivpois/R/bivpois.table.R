"bivpois.table" <-
function(x, y, lambda = c(1, 1, 1))
{
# ------------------------------------------------------------------------------
# Karlis and Ntzoufras (2003, 2004)
# EM algorithms for Bivariate Poisson Models
# ------------------------------------------------------------------------------
# x     : 1st count variable
# y     : 2nd count variable
# lambda: parameters of the bivariate poisson distribution.
# ------------------------------------------------------------------------------

	j<-0
	n <- length(x)
	maxy <- c(max(x), max(y))	#Set initial values for parameters
	lambda1 <- lambda[1]
	lambda2 <- lambda[2]
	lambda3 <- lambda[3]
	if((x == 0) | (y == 0)) {
		prob <- matrix(NA, nrow = maxy[1] + 1, ncol = maxy[2]+1, byrow = T)
		prob[maxy[1] + 1, maxy[2] + 1] <- exp( - lambda3) * 
			dpois(x[j], lambda1[j]) * dpois(y[j], lambda2[j])
	}
	else {
		prob <- matrix(NA, nrow = maxy[1] + 1, ncol = maxy[2]+1, byrow = T)
		k <- 1
		m <- 1
		prob[k, m] <- exp( - lambda1 - lambda2 - lambda3)
		for(i in 2:(maxy[1] + 1)) {
			prob[i, 1] <- (prob[i - 1, 1] * lambda1)/(i - 1)
		}
		for(j in 2:(maxy[2] + 1)) {
			prob[1, j] <- (prob[1, j - 1] * lambda2)/(j - 1)
		}
		for(j in 2:(maxy[2] + 1)) {
			for(i in 2:(maxy[1] + 1)) {
				prob[i, j] <- (lambda1 * prob[i - 1, j] + 
				  lambda3 * prob[i - 1, j - 1])/(i - 1)
			}
		}
	}
	result <- prob
	result
}

