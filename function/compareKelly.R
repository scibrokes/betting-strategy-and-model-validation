compareKelly <- function(K, initial = NULL, parallel = as.logical(FALSE), by = 'daily', 
                         by.league = as.logical(FALSE), adjusted = 1, chart = as.logical(FALSE), 
                         type = NULL, event = NULL, event.dates = NULL, chart.type = NULL) {
  ## A function which compare the staking, P&L result as well as the risk (sd and var)
  ##   of vKelly() and also vKelly2() models. Set an initial fund and simulate a moving
  ##   average or so call bollinger bands for risk control and avoid the bankruptcy while 
  ##   the risk adjuster is `weight.stakes` in vKelly() and vKelly2() which will measure 
  ##   by simulateKelly() later. compareKelly has compare the outcome of the various 
  ##   Kelly models with plot graph in order to easier understand and comparison.
  ##   The data format convert to xts format which compatibility to quantmod package.
  ## K is a dataset after apply vKelly() or vKelly2().
  ## initial = NULL or any numeric value. You can set an initial fund size.
  ## parallel = FALSE or parallel = TRUE if you want to set parallel computing.
  ## by = 'daily' or by = 'time' is summarise the data by daily or TimeUS (Kick-Off Time).
  ## by.league = TRUE or by.league = FALSE if you want to breakdown the league or not.
  ## chart = FALSE or chart = TRUE and sub-option type = 'single' or type = 'multiple'.
  ## event = NULL, you can set a vector of event contain and event.dates.
  ## chart.type = 'Cl', you can choose open, high, low or close daily fund size data for 'multiple'.
  
  ## --------------------- Load packages ------------------------------------------
  options(warn = -1)
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('formattable'))
  suppressMessages(library('quantmod'))
  suppressMessages(library('highcharter'))
  suppressMessages(library('doParallel'))
  suppressMessages(library('BBmisc'))
  suppressMessages(library('quantmod'))
  suppressMessages(source('./function/plotChart.R', local = TRUE))
  
  if(parallel == TRUE) {
    #'@ registerDoParallel(cores = detectCores())
    registerDoParallel(cores = 3)
  }
  ## --------------------- Data Validation ----------------------------------------
  
  if(any(str_detect(names(K), 'Kelly'))) {
    Kbase <- llply(K[grep('Kelly', names(K), value = TRUE)], function(x) x$BR)
    
  } else {
    stop('Kindly apply vKelly() or vKelly2() to get the Kelly models and set as the data input `K`.')
  }
  
  if(!is.numeric(adjusted)) stop('Kindly insert a vector of numeric values.')
  
  if(is.numeric(initial)) {
    initial <- initial
    
  } else if(is.null(initial)) {
    
    ## retrieve the initial fund size.
    initial <- llply(K, function(x) {
      unlist(x[grep('initial', names(x), value = TRUE)])
    }, .parallel = parallel) %>% unlist %>% as.list
    
  } else {
    stop('Kindly set a numeric value as initial fund to compare the various Kelly models.')
  }
  
  ## --------------------- Data Manipulation --------------------------------------
  
  if(by == 'daily') {
    if(by.league == TRUE) {
      Kbase <- llply(Kbase, function(x) {
        suppressAll(llply(list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
          df <- data.frame(x[c('TimeUS', 'DateUS', 'League')], 
                           x[grep(y, names(x), value = TRUE)]) %>% tbl_df
          
          ## Alternative, you can use below functions from `quantmod` to retrieve the data.
          ## quantmod::Op(), Hi(), Lo(), Cl()
          ## adjustOHLC() not yet modify to .
          if(y == '.Open') {
            ddply(df, .(DateUS, League), numcolwise(head, 1)) %>% tbl_df
          } else if(y == '.High') {
            ddply(df, .(DateUS, League), numcolwise(max)) %>% tbl_df
          } else if(y == '.Low') {
            ddply(df, .(DateUS, League), numcolwise(min)) %>% tbl_df
          } else if(y == '.Close') {
            ddply(df, .(DateUS, League), numcolwise(tail, 1)) %>% tbl_df
          } else if(y == '.Volume') {
            ddply(df, .(DateUS, League), numcolwise(sum)) %>% tbl_df
          } else if(y == '.Adjusted') {
            ddply(df, .(DateUS, League), numcolwise(mean)) %>% tbl_df
          } else {
            df
          }
        }, .parallel = parallel) %>% join_all %>% tbl_df)
      }, .parallel = parallel) %>% llply(split(., .$League), function(x) xts(x[-c(1:2)], x$DateUS))
    
    } else {
      Kbase <- llply(Kbase, function(x) {
        suppressAll(llply(list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
          df <- data.frame(x[c('TimeUS', 'DateUS', 'League')], 
                           x[grep(y, names(x), value = TRUE)]) %>% tbl_df
          
          if(y == '.Open') {
            ddply(df, .(DateUS), numcolwise(head, 1)) %>% tbl_df
          } else if(y == '.High') {
            ddply(df, .(DateUS), numcolwise(max)) %>% tbl_df
          } else if(y == '.Low') {
            ddply(df, .(DateUS), numcolwise(min)) %>% tbl_df
          } else if(y == '.Close') {
            ddply(df, .(DateUS), numcolwise(tail, 1)) %>% tbl_df
          } else if(y == '.Volume') {
            ddply(df, .(DateUS), numcolwise(sum)) %>% tbl_df
          } else if(y == '.Adjusted') {
            ddply(df, .(DateUS), numcolwise(mean)) %>% tbl_df
          } else {
            df
          }
        }, .parallel = parallel) %>% join_all %>% tbl_df)
      }, .parallel = parallel) %>% llply(function(x) xts(x[-1], x$DateUS))
    }
    
  } else if(by == 'time') {
    if(by.league == TRUE) {
      Kbase <- llply(Kbase, function(x) {
        suppressAll(llply(list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
          df <- data.frame(x[c('TimeUS', 'DateUS', 'League')], 
                           x[grep(y, names(x), value = TRUE)]) %>% tbl_df
          
          if(y == '.Open') {
            ddply(df, .(TimeUS, League), numcolwise(head, 1)) %>% tbl_df
          } else if(y == '.High') {
            ddply(df, .(TimeUS, League), numcolwise(max)) %>% tbl_df
          } else if(y == '.Low') {
            ddply(df, .(TimeUS, League), numcolwise(min)) %>% tbl_df
          } else if(y == '.Close') {
            ddply(df, .(TimeUS, League), numcolwise(tail, 1)) %>% tbl_df
          } else if(y == '.Volume') {
            ddply(df, .(TimeUS, League), numcolwise(sum)) %>% tbl_df
          } else if(y == '.Adjusted') {
            ddply(df, .(TimeUS, League), numcolwise(mean)) %>% tbl_df
          } else {
            df
          }
        }, .parallel = parallel) %>% join_all %>% tbl_df)
      }, .parallel = parallel) %>% llply(split(., .$League), function(x) xts(x[-c(1:2)], x$TimeUS))
    
    } else {
      Kbase <- llply(Kbase, function(x) {
        suppressAll(llply(list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), 
                          function(y) {
          df <- data.frame(x[c('TimeUS', 'DateUS', 'League')], 
                           x[grep(y, names(x), value = TRUE)]) %>% tbl_df
          
          if(y == '.Open') {
            ddply(df, .(TimeUS), numcolwise(head, 1)) %>% tbl_df
          } else if(y == '.High') {
            ddply(df, .(TimeUS), numcolwise(max)) %>% tbl_df
          } else if(y == '.Low') {
            ddply(df, .(TimeUS), numcolwise(min)) %>% tbl_df
          } else if(y == '.Close') {
            ddply(df, .(TimeUS), numcolwise(tail, 1)) %>% tbl_df
          } else if(y == '.Volume') {
            ddply(df, .(TimeUS), numcolwise(sum)) %>% tbl_df
          } else if(y == '.Adjusted') {
            ddply(df, .(TimeUS), numcolwise(mean)) %>% tbl_df
          } else {
            df
          }
        }, .parallel = parallel) %>% join_all %>% tbl_df)
      }, .parallel = parallel) %>% llply(function(x) xts(x[-1], x$TimeUS))
    }
    
  } else {
    stop('Kindly select by = "daily" or by = "time".')
  }
  
  ## --------------------- Data Visualization -------------------------------------
  
  if(chart == TRUE) {
    ## load plotChart()
    if(type == 'single') {
      plotFund <- llply(Kbase, plotChart, type = type, event = event, event.dates = event.dates)
    } else if(type == 'multiple') {
      plotFund <- llply(Kbase, plotChart, type = type, event = event, event.dates = event.dates, 
                        chart.type = chart.type)
    } else {
      stop('Kindly choose type = "single" or type = "multiple" if you choose chart = TRUE.')
    }
    
    return(plotFund)
    
  } else if(chart == FALSE) {
    ## --------------------- Return Function ----------------------------------------
    options(warn = 0)
    return(Kbase)
    
  } else {
    options(warn = 0)
    stop('Kindly choose chart = TRUE or chart = FALSE.')
  }
}

