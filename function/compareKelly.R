compareKelly <- function(K, initial = NULL, parallel = as.logical(FALSE), by = 'daily', 
                         by.league = as.logical(FALSE), adjusted = 1, 
                         chart = as.logical(FALSE), type = 'multiple', event = NULL, 
                         event.dates = NULL, chart.type = NULL, num = NULL, subnum = NULL) {
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
  ## chart = FALSE or chart = TRUE
  ## type = 'single' or type = 'multiple' will use for both dataset and plot. For 'single' 
  ##   will return with a list of main funds and a list of sub funds. For 'multiple' will 
  ##   return with a list of main funds while all sub funds join as one xts format data frame.
  ## event = NULL, you can set a vector of event contain and event.dates.
  ## chart.type = 'Cl', you can choose open, high, low or close daily fund size data for 
  ##   'multiple'.
  ## num is a vector value which up to the max number of main funds. vKelly() is 1 or 4 while 
  ##   vKelly2() is 1 or 2. You can select to plot or get only the dataset of certain fund.
  ## subnum is a vector value which up to the max number of sub funds. max 19 sub funds per 
  ##   main fund. You can select to plot or get only the dataset of certain fund but not all.
  
  ## --------------------- Load packages ------------------------------------------
  options(warn = -1)
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
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
  
  if(!is.numeric(adjusted)) {
    stop('Kindly insert a vector of numeric values.')
    
  } else {
    adjusted <- adjusted
  }
  
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
        suppressAll(llply(
          list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
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
      }, .parallel = parallel) %>% llply(split(., .$League), 
                                         function(x) xts(x[-c(1:2)], x$DateUS))
    
    } else {
      Kbase <- llply(Kbase, function(x) {
        suppressAll(llply(
          list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
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
        suppressAll(llply(
          list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
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
      }, .parallel = parallel) %>% llply(split(., .$League), 
                                         function(x) xts(x[-c(1:2)], x$TimeUS))
    
    } else {
      Kbase <- llply(Kbase, function(x) {
        suppressAll(llply(
          list('.Open', '.High', '.Low', '.Close', '.Volume', '.Adjusted'), function(y) {
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
  
  ## --------------------- Data Categorization -------------------------------------
  ## categorise single or multiple dataset.
  name <- names(Kbase)
  subname <- grep('.Open', names(Op(Kbase[[1]])), value = TRUE) %>% 
    str_split_fixed('\\.', 2) %>% .[, 1]
  
  if(is.null(num)) {
    num <- seq(name)
  } else if(all(num %in% seq(name)) == FALSE) {
    stop('Kindly set num as a vector which must be inside the set of ', 
         paste(seq(name), collapse = ', '), '.')
  } else {
    num <- num
  }
  
  if(is.null(subnum)) {
    subnum <- seq(subname)
  } else if(all(subnum %in% seq(subname)) == FALSE) {
    stop('Kindly set subnum as a vector which must be inside the set of ', 
         paste(seq(subname), collapse = ', '), '.')
  } else {
    subnum <- subnum
  }
  
  if(type == 'single') {
    ## --------------------- Single Sub Funds -------------------------------------
    Kbase <- llply(Kbase, function(x) {
      z = llply(subname, function(y) {
        x[, y == str_replace_all(
          names(x), '\\.Open|\\.High|\\.Low|\\.Close|\\.Volume|\\.Adjusted', '')]
      }, .parallel = parallel)
      names(z) = subname; z
    }, .parallel = parallel)
    
    Kbase <- llply(num, function(i) {
      z = llply(subnum, function(j) {
        plotFund[[i]][[j]]
      }, .parallel = parallel)
      names(z) <- subname[subnum]; z
    }, .parallel = parallel)
    names(Kbase) <- name[num]
    
    if(chart == TRUE) {
      ## --------------------- Data Visualization -------------------------------
      ## plot a single fund candle stick chart.
      
      plotFund <- paste('plotFund(Fund = ', names(Kbase), '$', sapply(Kbase, names), 
               ', type = \'single\', event = event, event.dates = event.dates);')
      
      options(warn = 0)
      return(eval(parse(text = plotFund)))
      
    } else if(chart == FALSE) {
      options(warn = 0)
      return(Kbase)
      
    } else {
      options(warn = 0)
      stop('Kindly choose chart = TRUE or chart = FALSE.')
    }
    
  } else if(type == 'multiple') {
    ## --------------------- Multiple Sub Funds ---------------------------------
    Kbase <- llply(Kbase, function(x) {
      x[, sapply(
        subname[subnum], paste0, 
        c('.Open', 'High', '.Low', '.Close', '.Volume', '.Adjusted')) %>% as.vector]
    }, .parallel = parallel)
      
    if(chart == TRUE) {
      ## --------------------- Data Visualization -------------------------------
      ## plot a multiple sub funds trend chart.
      plotFund <- paste0(
        'plotFund(Fund = ', names(Kbase), 
        ', type = \'multiple\', event = event, event.dates = event.dates, chart.type = chart.type);')
      
      options(warn = 0)
      return(eval(parse(text = plotFund)))
      
    } else if(chart == FALSE) {
      options(warn = 0)
      return(Kbase)
      
    } else {
      options(warn = 0)
      stop('Kindly choose chart = TRUE or chart = FALSE.')
    }
    
  } else {
    stop('Kindly choose type = "single" or type = "multiple" if you choose chart = TRUE.')
  }
}

