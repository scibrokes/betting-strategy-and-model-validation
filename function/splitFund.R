splitFund <- function(pth = 'basic', .print = FALSE, parallel = FALSE, progress = 'none') {
  
  ## --------------------- Load packages --------------------------------------
  suppressMessages(library('BBmisc'))
  suppressMessages(library('plyr'))
  suppressMessages(library('magrittr'))
  suppressMessages(library('stringr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('stringr'))
  suppressMessages(library('rlist'))
  suppressMessages(library('quantmod'))
  suppressMessages(source('./function/readKelly.R', local = TRUE))
  suppressMessages(source('./function/compareKelly.R', local = TRUE))
  
  if(parallel == TRUE) {
    suppressMessages(library('doParallel'))
    doParallel::registerDoParallel(cores = detectCores())
  }
  
  if(pth == 'basic') {
    pth <- '/data1/'
    
  } else if(pth == 'portfolio') {
    pth <- '/data2/'
    iniVal <- read_rds(path = './data/initial.rds') %>% .$KM %>% .[6] %>% 
      as.character %>% as.numeric
    
  } else {
    stop('Kindly select pth = "basic" or pth = "portfolio".')
  }
  
  ## --------------------- Read data ------------------------------------------
  if(file.exists('./data/BR.rds')) {
    BR <- read_rds(path = './data/BR.rds')
    
  } else {
    BR <- readKelly(.summary = FALSE, .progress = progress)
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
  
  if(pth == 'portfolio') {
    r0 <- iniVal - fnds[1, grep('.Open', names(fnds), value = TRUE)] %>% unlist
    names(r0) %<>% str_replace_all('.Open', '')
    fnds[grep('.Open|.High|.Low|.Close', names(fnds), value = TRUE)]
  }
  
  llply(seq(r0), function(i) {
    df1 <- r0[i] + fnds[paste0(names(r0)[i], c('.Open', '.High', '.Low', '.Close'))]
    fnds[paste0(names(r0)[i], c('.Open', '.High', '.Low', '.Close'))] <- df1
  }, .parallel = parallel, .progress = progress) %>% bind_rows
  
  
  ## --------------------- Data Categorization ---------------------------------
  ## categorise the dataset.
  fd <- sapply(BR$KM, names) %>% unlist
  fd <- ifelse(nchar(names(fd)) > 2, substr(names(fd), 1, nchar(names(fd)) - 1), 
               names(fd)) %>% paste0(., '.', fd)
  
  fd2 <- Op(BR$KM$K1$Kelly1$BR) %>% names %>% str_replace_all('\\..*$','')
  allfds <- paste0(rep(fd, each = length(fd2)), '.', fd2)
  
  rm(BR)
  
  #> length(allfds)
  #[1] 6194
  #
  #> 326 * 4 # sub Kelly1~Kelly4 from 110 K-funds, 
  #>         # column name with c('.id', 'DateUS', 'TimeUS', 'League')
  #[1] 1304
  
  #> 6194 - 1304 #Op(x), Hi(x), Lo(x), Cl(x), Vo(x), Ad(x) has 4890 columns
  #[1] 4890
  
  #> 38468-1304 + 4 #remove all duplicated c('.id', 'TimeUS', 'DateUS', 'League').
  #[1] 37168
  
  ## --------------------- Split data ------------------------------------------
  dtt <- llply(c('.id', 'TimeUS', 'DateUS', 'League'), function(y) {
      fnds[grep(y, names(fnds), value = TRUE)][, 1]
    }) %>% bind_cols
  names(dtt) <- c('.id', 'TimeUS', 'DateUS', 'League')
  
  allfnds <- data.frame(dtt, Op(fnds), Hi(fnds), Lo(fnds), Cl(fnds), 
                        Vo(fnds), Ad(fnds)) %>% tbl_df
  
  allfnds2 <- suppressWarnings(llply(allfds, function(y) {
    ty = c('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted')
    df = allfnds[c('DateUS', paste0(y, ty))] %>% tbl_df
    
    ## http://www.magesblog.com/2012/06/transforming-subsets-of-data-in-r-with.html
    ## r1 # by
    ##  user  system elapsed 
    ## 13.690   4.178  42.118
    ## 
    ## r2 # ddply 
    ##  user  system elapsed 
    ## 18.215   6.873  53.061
    ## 
    ## r3 # data.table 
    ##  user  system elapsed 
    ## 0.171   0.036   0.442
    ## 
    ## need to use data.table package for transforming data.
    
    if(ty == '.Open') {
      xx = ddply(df, .(DateUS), numcolwise(head, 1)) %>% tbl_df
    } else if(ty == '.High') {
      xx = ddply(df, .(DateUS), numcolwise(max)) %>% tbl_df
    } else if(ty == '.Low') {
      xx = ddply(df, .(DateUS), numcolwise(min)) %>% tbl_df
    } else if(ty == '.Close') {
      xx = ddply(df, .(DateUS), numcolwise(tail, 1)) %>% tbl_df
    } else if(ty == '.Volume') {
      xx = ddply(df, .(DateUS), numcolwise(sum)) %>% tbl_df
    } else if(ty == '.Adjusted') {
      xx = ddply(df, .(DateUS), numcolwise(mean)) %>% tbl_df
    } else {
      xx = df
    }
    xx = xts(xx[-1], xx$DateUS)
    
    saveRDS(eval(parse(text = paste(allfds, '= xx'))), 
            file = paste0('./KellyApps', pth, y, '.rds'))
    rm(df, ty, xx)
    if(.print == TRUE) cat('\n', y, ' had saved.')
    
  }, .progress = progress, .parallel = parallel))
}