## two multivariate normal distribution functions from 'mvtnorm' and 'MASS' packages
## There will be endless looping from below while loops.
mvrnorm0 <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = var(scores)) %>% tbl_df
while(any(mvrnorm0$FTHG < 0)|any(mvrnorm0$FTAG < 0)) {
  mvrnorm0 <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = var(scores)) %>% tbl_df
  ## need to adjust the FTHG and FTAG, truncated or zero inflated
}


rmvnorm0 <- function (n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)), 
          method = c("eigen", "svd", "chol"), pre0.9_9994 = FALSE) {
  
  if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps), 
                   check.attributes = FALSE)) {
    stop("sigma must be a symmetric matrix")
  }
  if (length(mean) != nrow(sigma)) 
    stop("mean and sigma have non-conforming size")
  method <- match.arg(method)
  R <- if (method == "eigen") {
    ev <- eigen(sigma, symmetric = TRUE)
    if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
      warning("sigma is numerically not positive definite")
    }
    t(ev$vectors %*% (t(ev$vectors) * sqrt(ev$values)))
  }
  else if (method == "svd") {
    s. <- svd(sigma)
    if (!all(s.$d >= -sqrt(.Machine$double.eps) * abs(s.$d[1]))) {
      warning("sigma is numerically not positive definite")
    }
    t(s.$v %*% (t(s.$u) * sqrt(s.$d)))
  }
  else if (method == "chol") {
    R <- chol(sigma, pivot = TRUE)
    R[, order(attr(R, "pivot"))]
  }
  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = !pre0.9_9994) %*% 
    R
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

## ------------------------------------------------------------------------

mvrnorm0 <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, 
          EISPACK = FALSE) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  if (EISPACK) 
    stop("'EISPACK' is no longer supported by R", domain = NA)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
    t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
}




