leagueRiskProf <- function(mbase, type = 'summary', breakdown = FALSE, weight.type = 'annual', 
                           parallel = FALSE) {
  ## summarise the return rates of leagues or weight stakes of leagues.
  ## mbase is a dataset readfirmData() and follow by arrfirmData().
  ## type = 'summary' or type = 'weight'. Once you select type = 'weight', you need to choose either 
  ##   all, annual, daily, time or dynamic.
  ## weight.type = 'all', weight.type = 'annual', weight.type = 'daily', weight.type = 'time' or 
  ##   weight.type = 'time' is kick-off time based, weight.type = 'dynamic' will evaluate the match 
  ##   up to latest observation by same league.
  ## breakdown = TRUE or breakdown = FALSE, you can breakdown the result of staking. It is works on 
  ##   either type = 'summary' or type = 'weight'. For type = 'weight', you can get to know the 
  ##   weight value of different result win, win-half, push, cancelled, loss-half and loss accordingly.
  ## 
  ## All weight values are current data value, therefore lag() function required to fit into a dataset 
  ##   in order to get the weighted result. Otherwise we need to use current value and fit to next 
  ##   observation. $x_{i} = x_{t}$ while the $x_{t} are a data set from x_{0} up to x_{i-1}$. x_{0} is 
  ##   the data start from last session.
  
  ## --------------------- Load packages ----------------------------------------
  options(warn = -1)
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('plyr'))
  suppressMessages(library('formattable'))
  suppressMessages(library('doParallel'))
  
  if(parallel == TRUE) {
    #'@ registerDoParallel(cores = detectCores())
    registerDoParallel(cores = 3)
  }
  ## --------------------- Data validation --------------------------------------
  breakdown <- as.logical(breakdown)
  
  ## Re-categorise the soccer financial settlement date. Due to I have no the 
  ##   history matches dataset from bookmakers. The scrapped spbo time is not 
  ##   stable (always change, moreover there just an information website) where 
  ##   firm A is the firm who placed bets with millions HKD (although the 
  ##   kick-off time might also changed after placed that particular bet), 
  ##   therefore I follow the kick-off time of the firm A.
  
  if(!c('DateUS', 'TimeUS') %in% names(mbase)) {
    mbase <- mbase[order(mbase$No.x),] %>% mutate(
      TimeUS = format(DateUK, tz = 'EST', usetz = TRUE, format = '%Y-%m-%d %H:%M:%S'), 
      DateUS = as.Date(TimeUS))
  }
  
  dateUSID <- unique(mbase$DateUS)
  timeUSID <- unique(mbase$TimeUS)
  
  if(type == 'summary') {
    
    if(breakdown == FALSE) {
      ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
      ##   across the years.
      lRiskProf <- ddply(mbase[c('League', 'Result', 'Stakes')], .(League, Result), summarise, 
                         min = currency(min(Stakes)), mean = currency(mean(Stakes)), 
                         median = currency(median(Stakes)), sd = currency(sd(Stakes)), 
                         max = currency(max(Stakes)), .parallel = parallel) %>% 
        tbl_df %>% mutate(League = factor(League))
      
    } else if(breakdown == TRUE) {
      ## summarise the min, mean, median, sd, max of every leagues across the years.
      lRiskProf <- ddply(mbase[c('League', 'Stakes')], .(League), summarise, min = currency(min(Stakes)), 
                         mean = currency(mean(Stakes)), median = currency(median(Stakes)), 
                         sd = currency(sd(Stakes)), max = currency(max(Stakes)), .parallel = parallel) %>% 
        tbl_df %>% mutate(League = factor(League))
      
    } else {
      stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
    }
    
  } else if(type == 'weight') {
    
    if(weight.type == 'all') {
      
      if(breakdown == FALSE) {
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        lRiskProf <- ddply(mbase[c('League', 'Result', 'Stakes', 'PL')], .(League, Result), 
                           summarise, all.1 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
          tbl_df %>% mutate(League = factor(League))
        
      } else if(breakdown == TRUE) {
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        lRiskProf <- ddply(mbase[c('League', 'Result', 'Stakes', 'PL')], .(League), summarise, 
                           all.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
          tbl_df %>% mutate(League = factor(League))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
    
    } else if(weight.type == 'annual') {
      
      if(breakdown == FALSE) {
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        lRiskProf <- ddply(mbase[c('Sess', 'League', 'Result', 'Stakes', 'PL')], .(Sess, League, Result), 
                           summarise, annual.1 = exp(mean(PL / Stakes)), .parallel = parallel) %>% tbl_df %>% 
          mutate(League = factor(League))
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(Sess = rep(unique(mbase$Sess), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$Sess))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(annual.1 = as.numeric(str_replace_na(annual.1, 1)))
        
      } else if(breakdown == TRUE) {
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        lRiskProf <- ddply(mbase[c('Sess', 'League', 'Result', 'Stakes', 'PL')], .(Sess, League), summarise, 
                           annual.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
          tbl_df %>% mutate(League = factor(League))
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(Sess = rep(unique(mbase$Sess), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$Sess))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(annual.2 = as.numeric(str_replace_na(annual.2, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else if(weight.type == 'daily') {
      
      if(breakdown == FALSE) {
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        lRiskProf <- suppressAll(ldply(dateUSID, function(x) {
          Sessb <- unique(filter(mbase, DateUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'DateUS', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(DateUS <= x & Sess %in% Sessb)
          dt1 <- mb$DateUS[length(mb$DateUS)]
          mb1 <- ddply(mb, .(League, Result), summarise, daily.1 = exp(mean(PL / Stakes)), 
                       .parallel = parallel) %>% tbl_df %>% mutate(League = factor(League))
          
          data.frame(DateUS = dt1, mb1) %>% tbl_df
          }, .parallel = parallel)) %>% tbl_df
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(DateUS = rep(unique(mbase$DateUS), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$DateUS))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(daily.1 = as.numeric(str_replace_na(daily.1, 1)))
        
      } else if(breakdown == TRUE) {
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        lRiskProf <- suppressAll(ldply(dateUSID, function(x) {
          Sessb <- unique(filter(mbase, DateUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'DateUS', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(DateUS <= x & Sess %in% Sessb)
          dt1 <- mb$DateUS[length(mb$DateUS)] 
          mb1 <- ddply(mb, .(League), summarise, daily.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df %>% mutate(League = factor(League))
          
          data.frame(DateUS = dt1, mb1) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(DateUS = rep(unique(mbase$DateUS), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$DateUS))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(daily.2 = as.numeric(str_replace_na(daily.2, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    }else if(weight.type == 'time') {
      
      if(breakdown == FALSE) {
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        lRiskProf <- suppressAll(ldply(timeUSID, function(x) {
          Sessb <- unique(filter(mbase, TimeUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'TimeUS', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(TimeUS <= x & Sess %in% Sessb)
          dt1 <- mb$TimeUS[length(mb$TimeUS)]
          mb1 <- ddply(mb, .(League, Result), summarise, time.1 = exp(mean(PL / Stakes)), 
                       .parallel = parallel) %>% tbl_df %>% mutate(League = factor(League))
          
          data.frame(TimeUS = dt1, mb1) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(TimeUS = rep(unique(mbase$TimeUS), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$TimeUS))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(time.1 = as.numeric(str_replace_na(time.1, 1)))
        
      } else if(breakdown == TRUE) {
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        lRiskProf <- suppressAll(ldply(timeUSID, function(x) {
          Sessb <- unique(filter(mbase, TimeUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'TimeUS', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(TimeUS <= x & Sess %in% Sessb)
          dt1 <- mb$TimeUS[length(mb$TimeUS)] 
          mb1 <- ddply(mb, .(League), summarise, time.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df %>% mutate(League = factor(League))
          
          data.frame(TimeUS = dt1, mb1) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(TimeUS = rep(unique(mbase$TimeUS), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$TimeUS))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(time.2 = as.numeric(str_replace_na(time.2, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    }else if(weight.type == 'dynamic') {
      
      if(breakdown == FALSE) {
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        lRiskProf <- suppressAll(ldply(mbase$No.x, function(x) {
          Sessb <- unique(filter(mbase, No.x <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'No.x', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(No.x <= x & Sess %in% Sessb)
          dt1 <- mb$No.x[length(mb$No.x)]
          mb1 <- ddply(mb, .(League, Result), summarise, dym.1 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df %>% mutate(League = factor(League))
          
          data.frame(No.x = dt1, mb1) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(No.x = rep(unique(mbase$No.x), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$No.x))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(dym.1 = as.numeric(str_replace_na(dym.1, 1)))
        
      } else if(breakdown == TRUE) {
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        lRiskProf <- suppressAll(ldply(mbase$No.x, function(x) {
          Sessb <- unique(filter(mbase, No.x <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'No.x', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(No.x <= x & Sess %in% Sessb)
          dt1 <- mb$No.x[length(mb$No.x)] 
          mb1 <- ddply(mb, .(League), summarise, dym.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df %>% mutate(League = factor(League))
          
          data.frame(No.x = dt1, mb1) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## build a listing of all leagues to every single year.
        lProf = data_frame(No.x = rep(unique(mbase$No.x), each = length(factor(unique(mbase$League))))) %>% 
          mutate(League = rep(sort(factor(unique(mbase$League))), times = length(unique(mbase$No.x))))
        
        ## join data to be a complete league listing risk profile.
        lRiskProf <- suppressMessages(join(lProf, lRiskProf)) %>% tbl_df %>% 
          mutate(dym.2 = as.numeric(str_replace_na(dym.2, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else {
      stop('Kindly choose weight.type == "all", weight.type == "annual", weight.type == "daily", weight.type == "time" or weight.type == "dynamic".')
    }
    
  } else {
    stop('Kindly choose type = "summary" or type = "weight".')
  }
  
    ## --------------------- Return function --------------------------------------
  
  if(type == 'summary'){
    return(lRiskProf)
  } else {
    return(lRiskProf)
  }
}