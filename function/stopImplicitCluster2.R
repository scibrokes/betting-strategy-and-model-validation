stopImplicitCluster2 <- function() {
  ## http://qiita.com/hoxo_m/items/02b82d44a09752509dc3
  
  options <- doParallel:::.options
  if (exists(".revoDoParCluster", where = options) &&
      !is.null(get(".revoDoParCluster", envir = options))) {
    stopCluster(get(".revoDoParCluster", envir = options))
    remove(".revoDoParCluster", envir = options)
  }
}
