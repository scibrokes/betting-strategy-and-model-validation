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
    if(as.logical(by.league) == TRUE) {
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
      }, .parallel = parallel) %>% llply(function(x) xts(x[-1], x$DateUS))
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
  }
  
  if(by == 'time') {
    if(as.logical(by.league) == TRUE) {
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
      }, .parallel = parallel) %>% llply(function(x) xts(x[-1], x$DateUS))
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
      }, .parallel = parallel) %>% llply(function(x) xts(x[-1], x$DateUS))
    }
  }
  
  ## --------------------- Data Visualization -------------------------------------
  
  if(chart == TRUE) {
    
    plotChart <- function(Fund = substitute(Fund), type = 'single', event = NULL, 
                          event.dates = NULL, chart.type = NULL) {
      ## http://jkunst.com/highcharter/highstock.html
      ## type = 'single' or type = 'multiple'. Plot comparison graph or single fund details.
      ## chart.type = 'Op', chart.type = 'Hi', chart.type = 'Lo', chart.type = 'Cl'. Use 
      ##   what kind of fund size to plot for multiple funds comparison.
      suppressMessages(library('formattable'))
      suppressMessages(library('quantmod'))
      
      if(type == 'single') {
        
        ## single model details, volume, moving average and daily open, high, low, close.
        FUND.SMA.10  <- SMA(Cl(Fund), n = 10)
        FUND.SMA.200 <- SMA(Cl(Fund), n = 200)
        FUND.RSI.14  <- RSI(Cl(Fund), n = 14)
        FUND.RSI.SellLevel <- xts(rep(70, NROW(Fund)), index(Fund))
        FUND.RSI.BuyLevel  <- xts(rep(30, NROW(Fund)), index(Fund))
        
        plotc <- highchart() %>% 
          # create axis :)
          hc_yAxis_multiples(
            list(title = list(text = NULL), height = '45%', top = '0%'),
            list(title = list(text = NULL), height = '25%', top = '47.5%', opposite = TRUE),
            list(title = list(text = NULL), height = '25%', top = '75%')
            ) %>% 
          # series :D
          hc_add_series_ohlc(Fund, yAxis = 0, name = Fund) %>% 
          hc_add_series_xts(FUND.SMA.10,  yAxis = 0, name = 'Fast MA') %>% 
          hc_add_series_xts(FUND.SMA.200, yAxis = 0, name = 'Slow MA') %>% 
          hc_add_series_xts(Fund$Fund.Volume, color = 'gray', yAxis = 1, name = 'Volume', 
                            type = 'column') %>% 
          hc_add_series_xts(FUND.RSI.14, yAxis = 2, name = 'Osciallator') %>% 
          hc_add_series_xts(FUND.RSI.SellLevel, color = 'red', yAxis = 2, 
                            name = 'Sell level', enableMouseTracking = FALSE) %>% 
          hc_add_series_xts(FUND.RSI.BuyLevel, color = 'blue', yAxis = 2, 
                            name = 'Buy level', enableMouseTracking = FALSE) %>% 
          # I <3 themes
          hc_add_theme(hc_theme_smpl())
        
      } else if(type == 'multiple') {
        
        ## put remarks on big gap within highest and lowest within a day.
        #'@ event <- Hi(Fund) - Lo(Fund) # need to modify...
                                     # single chart high-low candle stick might need to 
                                     # label the reason and event to cause a hight volatility.
        
        initial <- Op(Fund)[1, ] %>% unique
        chart.type <- ifelse(is.null(chart.type), 'Cl', chart.type)
        
        ## comparison of fund size and growth of various Kelly models
        #'@ event <- c('netEMEdge', 'PropHKPriceEdge', 'PropnetProbBEdge', 'KProbHKPrice',
        #'@              'KProbnetProbB', 'KProbFixed', 'KProbFixednetProbB', 'KEMProb',
        #'@              'KEMProbnetProbB', 'KProbHalf','KProbHalfnetProbB', 'KProbQuarter',
        #'@              'KProbQuarternetProbB', 'KProbAdj','KProbAdjnetProbB', 'KHalfAdj',
        #'@              'KHalfAdjnetProbB', 'KEMQuarterAdj', 'KEMQuarterAdjnetProbB')
        
        ## add dates for event...
        ##   label the high volatility daily event.
        if(is.null(event.dates)) {
          event.dates <- as.Date(period.apply(
            diff(Op(Fund)), INDEX = endpoints(Fund), FUN = max) %>% data.frame %>% 
              rownames, format = '%Y-%m-%d')
        } else {
          event.dates <- as.Date(event.dates)
        }
        
        ## id of event label, event text
        id <- seq(length(event.dates))
        
        if(is.null(event)) {
          event <- id
        } else {
          event <- event
        }
        
        if(length(event) == length(event.dates)) {
          event <- event
          event.dates <- event.dates
        } else {
          stop('The vector length of event must be same with vector length of event.dates.')
        }
        
        if(chart.type == 'Op') {
          Fund <- Op(Fund)
        } else if(chart.type == 'Hi') {
          Fund <- Hi(Fund)
        } else if(chart.type == 'Lo') {
          Fund <- Lo(Fund)
        } else if(chart.type == 'Cl') {
          Fund <- Cl(Fund)
        } else {
          stop('Kindly choose chart.type = "Op", chart.type = "Hi", chart.type = "Lo", chart.type = "Cl".')
        }
        
        plotc <- highchart(type = "stock") %>% 
          hc_title(text = "Charting some Funds") %>% 
          hc_subtitle(text = paste("Data extracted using various Kelly functions. Initial fund size : $", initial)) %>% 
          hc_add_series_xts(Fund[, 1], id = names(Fund)[1]) %>% 
          hc_add_series_xts(Fund[, 2], id = names(Fund)[2]) %>% 
          hc_add_series_xts(Fund[, 3], id = names(Fund)[3]) %>% 
          hc_add_series_xts(Fund[, 4], id = names(Fund)[4]) %>% 
          hc_add_series_xts(Fund[, 5], id = names(Fund)[5]) %>% 
          hc_add_series_xts(Fund[, 6], id = names(Fund)[6]) %>% 
          hc_add_series_xts(Fund[, 7], id = names(Fund)[7]) %>% 
          hc_add_series_xts(Fund[, 8], id = names(Fund)[8]) %>% 
          hc_add_series_xts(Fund[, 9], id = names(Fund)[9]) %>% 
          hc_add_series_xts(Fund[,10], id = names(Fund)[10]) %>% 
          hc_add_series_xts(Fund[,11], id = names(Fund)[11]) %>% 
          hc_add_series_xts(Fund[,12], id = names(Fund)[12]) %>% 
          hc_add_series_xts(Fund[,13], id = names(Fund)[13]) %>% 
          hc_add_series_xts(Fund[,14], id = names(Fund)[14]) %>% 
          hc_add_series_xts(Fund[,15], id = names(Fund)[15]) %>% 
          hc_add_series_xts(Fund[,16], id = names(Fund)[16]) %>% 
          hc_add_series_xts(Fund[,17], id = names(Fund)[17]) %>% 
          hc_add_series_xts(Fund[,18], id = names(Fund)[18]) %>% 
          hc_add_series_xts(Fund[,19], id = names(Fund)[19]) %>% 
          
          ## add event remarks onto the chart.
          hc_add_series_flags(event.dates, title = paste0('E', event), #label of the event box
                              text = paste('Event : High volatility ', event), id = id) %>% #text inside the event box
          hc_add_theme(hc_theme_flat())
        
        return(plotc)
        
      } else {
        stop('Kindly choose type = "single" or type = "multiple" if you choose chart = TRUE.')
      }
    }
    
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

