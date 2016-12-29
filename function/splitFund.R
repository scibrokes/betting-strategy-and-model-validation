splitFund <- function(.print = FALSE, parallel = FALSE, .progress = .progress) {
  
  ## --------------------- Load packages --------------------------------------
  suppressMessages(library('plyr'))
  suppressMessages(library('magrittr'))
  suppressMessages(library('stringr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('rlist'))
  suppressMessages(library('quantmod'))
  suppressMessages(source('./function/readKelly.R', local = TRUE))
  suppressMessages(source('./function/compareKelly.R', local = TRUE))
  
  if(parallel == TRUE) {
    suppressMessages(library('doParallel'))
    doParallel::registerDoParallel(cores = detectCores())
  }
  
  ## --------------------- Read data ------------------------------------------
  if(file.exists('./data/BR.rds')) {
    BR <- read_rds(path = './data/BR.rds')
  } else {
    BR <- readKelly(.summary = FALSE, .progress = .progress)
    saveRDS(BR, file = './data/BR.rds')
  }
  
  #'@ fnds <- llply(BR$KM, function(x) {
  #'@   unlist(x, recursive = FALSE) %>% data.frame %>% tbl_df
  #'@ })
  
  #'@ sapply(fdns, function(x) {
  #'@   names(x) = str_replace_all(names(x), '.BR.', '\\.')
  #'@   x
  #'@ })
  
  fnds <- sapply(BR$KM, function(x) {
    y = unlist(x, recursive = FALSE) %>% data.frame %>% tbl_df
    names(y) = str_replace_all(names(y), '.BR.', '\\.')
    y
  })
  
  fnds <- unlist(BR$KM, recursive = FALSE) %>% data.frame %>% tbl_df
  names(fnds) <- str_replace_all(names(fnds), '.BR.', '\\.')
  
  ## --------------------- Data Categorization -------------------------------------
  ## categorise the dataset.
  fd <- sapply(BR$KM, names) %>% unlist
  fd <- ifelse(nchar(names(fd)) > 2, substr(names(fd), 1, nchar(names(fd)) - 1), 
               names(fd)) %>% paste0(., '.', fd)
  
  fd2 <- Op(BR$KM$K1$Kelly1$BR) %>% names %>% str_replace_all('\\..*$','')
  name <- BR$KMnames
  subname <- fd2
  
  allfds <- paste0(rep(fd, each = length(fd2)), '.', fd2)
  
  ## --------------------- Split data ------------------------------------------
  Kbase <- llply(BR$KM[[name]], function(x) {
    z = llply(subname, function(y) {
      x[, y == str_replace_all(
        names(x), '\\.Open|\\.High|\\.Low|\\.Close|\\.Volume|\\.Adjusted', '')]
    }, .progress = .progress, .parallel = parallel)
    names(z) = subname; z
  }, .progress = .progress, .parallel = parallel)
  
  Kbase <- llply(num, function(i) {
    z = llply(subnum, function(j) {
      Kbase[[i]][[j]]
    }, .progress = .progress, .parallel = parallel)
    names(z) <- subname[subnum]; z
  }, .progress = .progress, .parallel = parallel)
  names(Kbase) <- name[num]
  
  
}