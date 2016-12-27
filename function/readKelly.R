readKelly <- function(details = 'bankroll', .summary = TRUE, .progress = 'none', parallel = FALSE){
  ## read 110 vKelly() and vKelly2() models. You can either choose 
  ## details = 'bankroll', details = 'dataset' or details = 'initial-fund-size'. It will read the 
  ##   necessary dataset from the list of Kelly models. Besides, you can choose 
  ## .summary = TRUE or .summary = FALSE, get summarise the dataset.
  
  ## --------------------- Load packages ------------------------------------------
  suppressMessages(library('rlist'))
  suppressMessages(library('plyr'))
  suppressMessages(library('magrittr'))
  suppressMessages(library('tidyverse'))
  
  if(parallel == TRUE) {
    suppressMessages(library('doParallel'))
    doParallel::registerDoParallel(cores = detectCores())
  }
  
  KMnames <- c('K1', 'K2', 'K1W1', 'K1W2', 'K2W1', 'K2W2', #6
               'K1W1WS1', 'K1W1WS2', 'K1W2WS1', 'K1W2WS2', #4 #10
               'K2W1WS1', 'K2W1WS2', 'K2W2WS1', 'K2W2WS2', #4 #14
               'K1D1', 'K1D2', 'K2D1', 'K2D2', #4 #18
               'K1DWS1', 'K1DWS2', 'K2DWS1', 'K2DWS2', #4 #22
               'K1D1WS1', 'K1D1WS2', 'K1D2WS1', 'K1D2WS2', #4 #26
               'K2D1WS1', 'K2D1WS2', 'K2D2WS1', 'K2D2WS2', #4 #30
               'K1W1DWS1', 'K1W1DWS2', 'K1W2DWS1', 'K1W2DWS2', #4 #34
               'K2W1DWS1', 'K2W1DWS2', 'K2W2DWS1', 'K2W2DWS2', #4 #38
               'K1D1DWS1', 'K1D1DWS2', 'K1D2DWS1', 'K1D2DWS2', #4 #42
               'K2D1DWS1', 'K2D1DWS2', 'K2D2DWS1', 'K2D2DWS2', #4 #46
               'K1D1DWS1TT', 'K1D1DWS2TT', 'K1D2DWS1TT', 'K1D2DWS2TT', #4 #50
               'K2D1DWS1TT', 'K2D1DWS2TT', 'K2D2DWS1TT', 'K2D2DWS2TT', #4 #54
               'K1D1DWS1TO', 'K1D1DWS2TO', 'K1D2DWS1TO', 'K1D2DWS2TO', #4 #58
               'K2D1DWS1TO', 'K2D1DWS2TO', 'K2D2DWS1TO', 'K2D2DWS2TO', #4 #62
               'K1D1DWS1DD', 'K1D1DWS2DD', 'K1D2DWS1DD', 'K1D2DWS2DD', #4 #66
               'K2D1DWS1DD', 'K2D1DWS2DD', 'K2D2DWS1DD', 'K2D2DWS2DD', #4 #70
               'K1D1DWS1DT', 'K1D1DWS2DT', 'K1D2DWS1DT', 'K1D2DWS2DT', #4 #74
               'K2D1DWS1DT', 'K2D1DWS2DT', 'K2D2DWS1DT', 'K2D2DWS2DT', #4 #78
               'K1D1DWS1DO', 'K1D1DWS2DO', 'K1D2DWS1DO', 'K1D2DWS2DO', #4 #82
               'K2D1DWS1DO', 'K2D1DWS2DO', 'K2D2DWS1DO', 'K2D2DWS2DO', #4 #86
               'K1D1DWS1OD', 'K1D1DWS2OD', 'K1D2DWS1OD', 'K1D2DWS2OD', #4 #90
               'K2D1DWS1OD', 'K2D1DWS2OD', 'K2D2DWS1OD', 'K2D2DWS2OD', #4 #94
               'K1D1DWS1OT', 'K1D1DWS2OT', 'K1D2DWS1OT', 'K1D2DWS2OT', #4 #98
               'K2D1DWS1OT', 'K2D1DWS2OT', 'K2D2DWS1OT', 'K2D2DWS2OT', #4 #102
               'K1D1DWS1OO', 'K1D1DWS2OO', 'K1D2DWS1OO', 'K1D2DWS2OO', #4 #106
               'K2D1DWS1OO', 'K2D1DWS2OO', 'K2D2DWS1OO', 'K2D2DWS2OO') #4 #110
  
  if(details == 'dataset') {
    ## ========= read dataset ================================
    
    KM <- llply(KMnames, function(filename) {
      x = readr::read_rds(path = paste0('./data/', filename, '.rds'))
      rlist::list.select(x[grepl('Kelly', list.names(x))], data)
    }, .progress = .progress, .parallel = parallel)
    names(KM) <- KMnames
    
    if(.summary == TRUE) {
      KM <- llply(seq(KM), function(i) {
        x = llply(seq(KM[[i]]), function(j) {
          summary(KM[[i]][[j]]$data)
        }, .progress = .progress, .parallel = parallel)
        names(x) = paste0('Kelly', seq(x)); x
      }, .progress = .progress, .parallel = parallel)
      names(KM) <- KMnames
    }
    
  } else if(details == 'bankroll') {
    ## ========= read Bank Roll ================================
    
    KM <- llply(KMnames, function(filename) {
      x = readr::read_rds(path = paste0('./data/', filename, '.rds'))
      rlist::list.select(x[grepl('Kelly', list.names(x))], BR)
    }, .progress = .progress, .parallel = parallel)
    names(KM) <- KMnames
    
    if(.summary == TRUE) {
      KM <- llply(seq(KM), function(i) {
        x = llply(seq(KM[[i]]), function(j) {
          summary(KM[[i]][[j]]$BR)
          }, .progress = .progress, .parallel = parallel)
        names(x) = paste0('Kelly', seq(x)); x
      }, .progress = .progress, .parallel = parallel)
      names(KM) <- KMnames
    }
    
  } else if(details == 'initial-fund-size') {
    ## ========= read Bank Roll ================================
    
    KM <- llply(KMnames, function(filename) {
      x = readr::read_rds(path = paste0('./data/', filename, '.rds'))
      unlist(rlist::list.select(x[grepl('Kelly', list.names(x))], initial))
    }, .progress = .progress, .parallel = parallel)
    names(KM) <- KMnames
    
    if(.summary == TRUE) KM <- summary(unlist(KM))
   
  } else {
    stop('Kindly choose details = "dataset", details = "bankroll" or details = "initial-fund-size".')
  }
  
  return(list(KMnames = KMnames, KM = KM))
}


