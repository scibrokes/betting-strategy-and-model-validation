leagueRiskProf <- function(mbase, type = 'summary', breakdown = FALSE, weight.type = 'annual', 
                           parallel = FALSE) {
  ## summarise the return rates of leagues or weight stakes of leagues.
  ## mbase is a dataset readfirmData() and follow by arrfirmData().
  ## type = 'summary', type = 'weight.stakes' or type = 'weight'. Once you select type = 'weight', you 
  ##   need to choose either all, annual, daily, time or dynamic.
  ## weight.type = 'all', weight.type = 'annual', weight.type = 'daily', weight.type = 'time', 
  ##   weight.type = 'dynamic' or weight.type = 'ts'.
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
    mbase <- mbase %>% mutate(
      TimeUS = ymd_hms(format(DateUK, tz = 'EST', usetz = TRUE, format = '%Y-%m-%d %H:%M:%S')), 
      DateUS = as.Date(TimeUS))
    mbase <- mbase[order(mbase$TimeUS, mbase$No.x, decreasing = FALSE),]
  }
  
  if(!any(grepl('obs', names(mbase)))) {
    ## observation based for dynamic simulation use.
    mbase %<>% mutate(obs = seq(1, nrow(.)))
  }
  
  dateUSID <- sort(unique(mbase$DateUS)) %>% ymd
  timeUSID <- sort(unique(mbase$TimeUS)) %>% ymd_hms
  
  ## --------------------- Summary --------------------------------------
  if(type == 'summary') {
    
    if(breakdown == TRUE) {
      ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
      ##   across the years.
      lRiskProf <- ddply(mbase[c('League', 'Result', 'Stakes')], .(League, Result), summarise, 
                         min = currency(min(Stakes)), mean = currency(mean(Stakes)), 
                         median = currency(median(Stakes)), sd = currency(sd(Stakes)), 
                         max = currency(max(Stakes)), .parallel = parallel) %>% 
        tbl_df %>% mutate(League = factor(League))
      
    } else if(breakdown == FALSE) {
      ## summarise the min, mean, median, sd, max of every leagues across the years.
      lRiskProf <- ddply(mbase[c('League', 'Stakes')], .(League), summarise, min = currency(min(Stakes)), 
                         mean = currency(mean(Stakes)), median = currency(median(Stakes)), 
                         sd = currency(sd(Stakes)), max = currency(max(Stakes)), .parallel = parallel) %>% 
        tbl_df %>% mutate(League = factor(League))
      
    } else {
      stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
    }
  
  ## --------------------- Weight Stakes --------------------------------------
  } else if(type == 'weight.stakes') {
    
    if(weight.type == 'all') {
      
      if(breakdown == TRUE) {
        ## constant weight stakes parameter across the leagues by result.
        lRiskProf <- ddply(mbase[c('League', 'Result', 'Stakes', 'PL')], .(League, Result), 
                           summarise, all.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
          tbl_df %>% mutate(League = factor(League))
        
      } else if(breakdown == FALSE) {
        ## constant weight stakes parameters across the leagues.
        lRiskProf <- ddply(mbase[c('League', 'Result', 'Stakes', 'PL')], .(League), summarise, 
                           all.1 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
          tbl_df %>% mutate(League = factor(League))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
    
    } else if(weight.type == 'annual') {
      
      if(breakdown == TRUE) {
        ## constant annual weight stakes parameter across the leagues by result.
        lRiskProf <- ddply(mbase[c('Sess', 'League', 'Result', 'Stakes', 'PL')], .(Sess, League, Result), 
                           summarise, annual.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% tbl_df %>% 
          mutate(League = factor(League))
        
      } else if(breakdown == FALSE) {
        ## constant annual weight stakes parameter across the leagues.
        lRiskProf <- ddply(mbase[c('Sess', 'League', 'Result', 'Stakes', 'PL')], .(Sess, League), summarise, 
                           annual.1 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
          tbl_df %>% mutate(League = factor(League))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else if(weight.type == 'daily') {
      
      if(breakdown == TRUE) {
        
        ## build a listing of all leagues to every single year.
        n1 <- length(unique(mbase$Result))
        n2 <- n1 * length(unique(mbase$League))
        lProf <- data.frame(DateUS = rep(unique(mbase$DateUS), each = n2), 
                            League = rep(sort(factor(unique(mbase$League))), each = n1), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n1, n2)
        
        ## constant daily weight stakes parameter across the leagues by result.
        lRiskProf <- suppressAll(ldply(dateUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, DateUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'DateUS', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(DateUS <= x & Sess %in% Sessb)
          dt1 <- lProf %>% filter(DateUS == x)
          mb1 <- ddply(mb, .(League, Result), summarise, daily.2 = exp(mean(PL / Stakes)), 
                       .parallel = parallel) %>% tbl_df %>% mutate(League = factor(League))
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
          }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        lRiskProf %<>% mutate(daily.2 = ifelse(is.na(daily.2) & DateUS == dateUSID[1], 1, daily.2)) %>% 
          transform(., na.locf(daily.2)) %>% 
          mutate(daily.2 = as.numeric(str_replace_na(daily.2, 1)))
        
      } else if(breakdown == FALSE) {
        
        ## build a listing of all leagues to every single year.
        n <- length(unique(mbase$League))
        lProf <- data.frame(DateUS = rep(unique(mbase$DateUS), each = n), 
                            League = sort(factor(unique(mbase$League)))) %>% tbl_df
        rm(n)
        
        ## constant daily weight stakes parameter across the leagues.
        lRiskProf <- suppressAll(ldply(dateUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, DateUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'DateUS', 'League', 'Stakes', 'PL')] %>% 
            filter(DateUS <= x & Sess %in% Sessb)
          dt1 <- lProf %>% filter(DateUS == x)
          mb1 <- ddply(mb, .(League), summarise, daily.1 = exp(mean(PL / Stakes)), 
                       .parallel = parallel) %>% tbl_df %>% mutate(League = factor(League))
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        lRiskProf %<>% mutate(daily.1 = ifelse(is.na(daily.1) & DateUS == dateUSID[1], 1, daily.1)) %>% 
          transform(., na.locf(daily.1)) %>% 
          mutate(daily.1 = as.numeric(str_replace_na(daily.1, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    }else if(weight.type == 'time') {
      
      if(breakdown == TRUE) {
        
        ## build a listing of all leagues to every single year.
        n1 <- length(unique(mbase$Result))
        n2 <- n1 * length(unique(mbase$League))
        lProf <- data.frame(TimeUS = rep(unique(mbase$TimeUS), each = n2), 
                            League = rep(sort(factor(unique(mbase$League))), each = n1), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n1, n2)
        
        ## kick-off-time based constant weight stakes parameter across the leagues by result.
        lRiskProf <- suppressAll(ldply(timeUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, TimeUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'TimeUS', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(TimeUS <= x & Sess %in% Sessb)
          dt1 <- lProf %>% filter(TimeUS == x)
          mb1 <- ddply(mb, .(League, Result), summarise, time.2 = exp(mean(PL / Stakes)), 
                       .parallel = parallel) %>% tbl_df %>% mutate(League = factor(League))
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        lRiskProf %<>% mutate(time.2 = ifelse(is.na(time.2) & TimeUS == timeUSID[1], 1, time.2)) %>% 
          transform(., na.locf(time.2)) %>% 
          mutate(time.2 = as.numeric(str_replace_na(time.2, 1)))
        
      } else if(breakdown == FALSE) {
        
        ## build a listing of all leagues to every single year.
        n <- length(unique(mbase$League))
        lProf <- data.frame(TimeUS = rep(unique(mbase$TimeUS), each = n), 
                            League = sort(factor(unique(mbase$League)))) %>% tbl_df
        rm(n)
        
        ## kick-off-time based constant weight stakes parameter across the leagues.
        lRiskProf <- suppressAll(ldply(timeUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, TimeUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'TimeUS', 'League', 'Stakes', 'PL')] %>% 
            filter(TimeUS <= x & Sess %in% Sessb)
          dt1 <- lProf %>% filter(TimeUS == x)
          mb1 <- ddply(mb, .(League), summarise, time.1 = exp(mean(PL / Stakes)), 
                       .parallel = parallel) %>% tbl_df %>% mutate(League = factor(League))
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        lRiskProf %<>% mutate(time.1 = ifelse(is.na(time.1) & TimeUS == timeUSID[1], 1, time.1)) %>% 
          transform(., na.locf(time.1)) %>% 
          mutate(time.1 = as.numeric(str_replace_na(time.1, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    }else if(weight.type == 'dynamic') {
      
      if(!any(grepl('obs', names(mbase)))) {
        ## observation based for dynamic simulation use.
        mbase %<>% mutate(obs = seq(1, nrow(.)))
      }
      
      if(breakdown == TRUE) {
        
        ## build a listing of all leagues to every single year.
        n1 <- length(unique(mbase$Result))
        n2 <- n1 * length(unique(mbase$League))
        lProf <- data.frame(obs = rep(unique(mbase$obs), each = n2), 
                            League = rep(sort(factor(unique(mbase$League))), each = n1), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n1, n2)
        
        ## observation based constant weight stakes parameter across the leagues by result.
        lRiskProf <- suppressAll(ldply(mbase$obs, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, obs <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'obs', 'League', 'Result', 'Stakes', 'PL')] %>% 
            filter(obs <= x & Sess %in% Sessb)
          dt1 <- lProf %>% filter(obs == x)
          mb1 <- ddply(mb, .(League, Result), summarise, dym.2 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df %>% mutate(League = factor(League))
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        lRiskProf %<>% mutate(dym.2 = ifelse(is.na(dym.2) & obs == obs[1], 1, dym.2)) %>% 
          transform(., na.locf(dym.2)) %>% 
          mutate(dym.2 = as.numeric(str_replace_na(dym.2, 1)))
        
      } else if(breakdown == FALSE) {
        
        ## build a listing of all leagues to every single year.
        n <- length(unique(mbase$League))
        lProf <- data.frame(obs = rep(unique(mbase$obs), each = n), 
                            League = sort(factor(unique(mbase$League)))) %>% tbl_df
        rm(n)
        
        ## observation based constant weight stakes parameter across the leagues.
        lRiskProf <- suppressAll(ldply(mbase$obs, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, obs <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'obs', 'League', 'Stakes', 'PL')] %>% 
            filter(obs <= x & Sess %in% Sessb)
          dt1 <- lProf %>% filter(obs == x)
          mb1 <- ddply(mb, .(League), summarise, dym.1 = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df %>% mutate(League = factor(League))
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        lRiskProf %<>% mutate(dym.1 = ifelse(is.na(dym.1) & obs == obs[1], 1, dym.1)) %>% 
          transform(., na.locf(dym.1)) %>% 
          mutate(dym.1 = as.numeric(str_replace_na(dym.1, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else {
      stop('Kindly choose weight.type == "all", weight.type == "annual", weight.type == "daily", weight.type == "time" or weight.type == "dynamic".')
    }
    
  ## --------------------- Weight --------------------------------------
  } else if(type == 'weight') {
    
    ## Due to the existing weight parameter 'dres' is not workable, we unable to know the result in advance 
    ##   therefore I modify it as Handicap based but same theory. For every single possible win-half, push, 
    ##   or loss-half handicaps will categorise as own groups accordingly to know the possibility of 
    ##   bet on different handicap will get different outcome. Bet on -0.75 and -1 will get win-half and 
    ##   push among the option.
    ## weighted parameter estimation
    
    ## Option 1 : (cancelled)
    ## list the possible win-loss of handicaps as 0, 0.25, 0.5, 0.75.
    ##   I breakdown from -0.75 to 0.75. For level ball, I categorise the favorite odds as similar with 
    ##   concedes 1, 2, 3... balls.
    ## 
    ## Option 2 : (take it)
    ## list the possible win-loss of handicaps as 0, 0.25, 0.5, 0.75, 1.00.
    ##   I breakdown from -1.00 to 1.00. For level ball, I differentiate the level ball with concedes 1, 2, 
    ##   3... balls due to from the level ball we cannot know the possibility between concedes or taken. Let 
    ##   say favorite team at level ball, concedes 1 ball and taken 1 ball will combine together and it will 
    ##   over-weight the possibility of push and make the comparison of gap of weight between -0.75 and -1.00, 
    ##   etc. Therefore here I try to st it as defult handicap risk profile.
    ## Here I use the PL/Stakes since it includes the payout as well as the result, therefore the weight value 
    ##   might probably be more accurate more to only result. $PO_{i}R_{i}$. I dont pretend to know the correct 
    ##   formula but I try to built two weight models which are solely result based weight and also handicap 
    ##   breakdown weighted parameters.
    
    hcpset <- seq(-1, 1, 0.25)
    ccal <- seq(-1, -51)
    cch1 <- seq(-0.75, -50.75)
    cchf <- seq(-0.5, -50.5)
    cclh <- seq(-0.25, -50.25)
    levl <- 0
    tklh <- seq(0.25, 50.25)
    tkhf <- seq(0.5, 50.5)
    tkh1 <- seq(0.75, 50.75)
    tkal <- seq(1, 51)
    
    mbase %<>% mutate(theta = suppressAll(
      ifelse(Result == 'Win', 1, 
      ifelse(Result == 'Half Win', 0.5, 
      ifelse(Result == 'Push'|Result == 'Cancelled', 0, 
      ifelse(Result == 'Half Loss', -0.5, 
      ifelse(Result == 'Loss', -1, NA)))))), 
      #'@ dWin = ifelse(Result == 'Win', 1, 0), 
      #'@ dwhf = ifelse(Result == 'Half Win', 0.5, 0), 
      #'@ dpus = ifelse(Result == 'Push'|Result == 'Cancelled', 0, 0), 
      #'@ dlhf = ifelse(Result == 'Half Loss', -0.5, 0), 
      #'@ dlos = ifelse(Result == 'Loss', -1, 0), 
      #'@ F1.00 = ifelse(HCap %in% ccal, hcpset[1], NA), 
      #'@ F0.75 = ifelse(HCap %in% cch1, hcpset[2], NA), 
      #'@ F0.50 = ifelse(HCap %in% cchf, hcpset[3], NA), 
      #'@ F0.25 = ifelse(HCap %in% cclh, hcpset[4], NA), 
      #'@ Level = ifelse(HCap %in% levl, hcpset[5], NA), 
      #'@ U0.25 = ifelse(HCap %in% tklh, hcpset[6], NA), 
      #'@ U0.50 = ifelse(HCap %in% tkhf, hcpset[7], NA), 
      #'@ U0.75 = ifelse(HCap %in% tkh1, hcpset[8], NA),
      #'@ U1.00 = ifelse(HCap %in% tkal, hcpset[9], NA), 
      hdpC = ifelse(HCap %in% ccal, hcpset[1], 
             ifelse(HCap %in% cch1, hcpset[2], 
             ifelse(HCap %in% cchf, hcpset[3], 
             ifelse(HCap %in% cclh, hcpset[4], 
             ifelse(HCap %in% levl, hcpset[5], 
             ifelse(HCap %in% tklh, hcpset[6], 
             ifelse(HCap %in% tkhf, hcpset[7], 
             ifelse(HCap %in% tkh1, hcpset[8], 
             ifelse(HCap %in% tkal, hcpset[9], NA))))))))))
    
    rm(ccal, cch1, cchf, cclh, levl, tklh, tkhf, tkh1, tkal)
    
    if(weight.type == 'all') {
      
      if(breakdown == FALSE) {
        ## constant weight stakes parameter across the data set.
        weightProf <- data_frame(Result = unique(mbase$Result), all.theta = exp(mean(mbase$theta)))
        
      } else if(breakdown == TRUE) {
        ## constant weight stakes parameter by result across the data set.
        weightProf <- ddply(mbase[c('hdpC', 'Stakes', 'PL')], 
                            .(hdpC), summarise, all.hdpW = exp(mean(PL / Stakes)), 
                           .parallel = parallel) %>% tbl_df
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else if(weight.type == 'annual') {
      
      if(breakdown == FALSE) {
        ## annual constant weight stakes parameter.
        weightProf <- ddply(mbase[c('Sess', 'Result', 'theta')], 
                            .(Sess), summarise, annual.theta = exp(mean(theta)), 
                            .parallel = parallel) %>% tbl_df
        
      } else if(breakdown == TRUE) {
        ## annual constant weight stakes parameter by result.
        weightProf <- ddply(mbase[c('Sess', 'hdpC', 'Stakes', 'PL')], 
                            .(Sess, hdpC), summarise, annual.hdpW = exp(mean(PL / Stakes)), 
                            .parallel = parallel) %>% tbl_df
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else if(weight.type == 'daily') {
      
      if(breakdown == FALSE) {
        
        ## build a listing of all leagues to every single year.
        n <- length(unique(mbase$Result))
        wProf <- data.frame(DateUS = rep(unique(mbase$DateUS), each = n), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n)
        
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        weightProf <- suppressAll(ldply(dateUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, DateUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'DateUS', 'Result', 'theta')] %>% filter(DateUS <= x & Sess %in% Sessb)
          dt1 <- wProf %>% filter(DateUS == x)
          mb1 <- ddply(mb, .(Result), summarise, daily.theta = exp(mean(theta)), 
                       .parallel = parallel) %>% tbl_df
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        weightProf %<>% mutate(daily.theta = ifelse(is.na(daily.theta) & DateUS == dateUSID[1], 1, daily.theta)) %>% 
          transform(., na.locf(daily.theta)) %>% 
          mutate(daily.theta = as.numeric(str_replace_na(daily.theta, 1)))
        
      } else if(breakdown == TRUE) {
        
        ## build a listing of all leagues to every single year.
        n1 <- length(unique(mbase$Result))
        n2 <- n1 * length(unique(mbase$hdpC))
        wProf <- data.frame(DateUS = rep(unique(mbase$DateUS), each = n2), 
                            hdpC = rep(sort(factor(unique(mbase$hdpC))), each = n1), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n1, n2)
        
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        weightProf <- suppressAll(ldply(dateUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, DateUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'DateUS', 'hdpC', 'Stakes', 'PL')] %>% filter(DateUS <= x & Sess %in% Sessb)
          dt1 <- wProf %>% filter(DateUS == x)
          mb1 <- ddply(mb, .(hdpC), summarise, daily.hdpW = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        weightProf %<>% mutate(daily.hdpW = ifelse(is.na(daily.hdpW) & DateUS == dateUSID[1], 1, daily.hdpW)) %>% 
          transform(., na.locf(daily.hdpW)) %>% 
          mutate(daily.hdpW = as.numeric(str_replace_na(daily.hdpW, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    }else if(weight.type == 'time') {
      
      if(breakdown == FALSE) {
        
        ## build a listing of all leagues to every single year.
        n <- length(unique(mbase$Result))
        wProf <- data.frame(TimeUS = rep(unique(mbase$TimeUS), each = n), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n)
        
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        weightProf <- suppressAll(ldply(timeUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, TimeUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'TimeUS', 'Result', 'theta')] %>% filter(TimeUS <= x & Sess %in% Sessb)
          dt1 <- wProf %>% filter(TimeUS == x)
          mb1 <- ddply(mb, .(Result), summarise, time.theta = exp(mean(theta)), 
                       .parallel = parallel) %>% tbl_df
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        weightProf %<>% mutate(time.theta = ifelse(is.na(time.theta) & TimeUS == timeUSID[1], 1, time.theta)) %>% 
          transform(., na.locf(time.theta)) %>% 
          mutate(time.theta = as.numeric(str_replace_na(time.theta, 1)))
        
      } else if(breakdown == TRUE) {
        
        ## build a listing of all leagues to every single year.
        n1 <- length(unique(mbase$Result))
        n2 <- n1 * length(unique(mbase$hdpC))
        wProf <- data.frame(TimeUS = rep(unique(mbase$TimeUS), each = n2), 
                            hdpC = rep(sort(factor(unique(mbase$hdpC))), each = n1), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n1, n2)
        
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        weightProf <- suppressAll(ldply(timeUSID, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, TimeUS <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'TimeUS', 'hdpC', 'Stakes', 'PL')] %>% filter(TimeUS <= x & Sess %in% Sessb)
          dt1 <- wProf %>% filter(TimeUS == x)
          mb1 <- ddply(mb, .(hdpC), summarise, time.hdpW = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        weightProf %<>% mutate(time.hdpW = ifelse(is.na(time.hdpW) & TimeUS == timeUSID[1], 1, time.hdpW)) %>% 
          transform(., na.locf(time.hdpW)) %>% mutate(time.hdpW = as.numeric(str_replace_na(time.hdpW, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    }else if(weight.type == 'dynamic') {
      
      if(!any(grepl('obs', names(mbase)))) {
        ## observation based for dynamic simulation use.
        mbase %<>% mutate(obs = seq(1, nrow(.)))
      }
      
      if(breakdown == FALSE) {
        
        ## build a listing of all leagues to every single year.
        n <- length(unique(mbase$Result))
        wProf <- data.frame(obs = rep(unique(mbase$obs), each = n), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n)
        
        ## summarise the min, mean, median, sd, max of every leagues with Win-Draw-Loss result breakdown 
        ##   across the years.
        weightProf <- suppressAll(ldply(mbase$obs, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, obs <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'obs', 'Result', 'theta')] %>% filter(obs <= x & Sess %in% Sessb)
          dt1 <- wProf %>% filter(obs == x)
          mb1 <- ddply(mb, .(Result), summarise, dym.theta = exp(mean(theta)), .parallel = parallel) %>% 
            tbl_df
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        weightProf %<>% mutate(dym.theta = ifelse(is.na(dym.theta) & obs == obs[1], 1, dym.theta)) %>% 
          transform(., na.locf(dym.theta)) %>% mutate(dym.theta = as.numeric(str_replace_na(dym.theta, 1)))
        
      } else if(breakdown == TRUE) {
        
        ## build a listing of all leagues to every single year.
        n1 <- length(unique(mbase$Result))
        n2 <- n1 * length(unique(mbase$hdpC))
        wProf <- data.frame(obs = rep(unique(mbase$obs), each = n2), 
                            hdpC = rep(sort(factor(unique(mbase$hdpC))), each = n1), 
                            Result = sort(factor(unique(mbase$Result)))) %>% tbl_df
        rm(n1, n2)
        
        ## summarise the min, mean, median, sd, max of every leagues across the years.
        weightProf <- suppressAll(ldply(mbase$obs, function(x) {
          
          suppressMessages(library('plyr'))
          suppressMessages(library('tidyverse'))
          
          Sessb <- unique(filter(mbase, obs <= x)$Sess) %>% tail(2)
          mb <- mbase[c('Sess', 'obs', 'hdpC', 'Stakes', 'PL')] %>% filter(obs <= x & Sess %in% Sessb)
          dt1 <- wProf %>% filter(obs == x)
          mb1 <- ddply(mb, .(hdpC), summarise, dym.hdpW = exp(mean(PL / Stakes)), .parallel = parallel) %>% 
            tbl_df
          
          suppressAll(join(dt1, mb1)) %>% tbl_df
        }, .parallel = parallel)) %>% tbl_df
        
        ## convert data to be a complete league listing risk profile in panel data format.
        weightProf %<>% mutate(dym.hdpW = ifelse(is.na(dym.hdpW) & obs == obs[1], 1, dym.hdpW)) %>% 
          transform(., na.locf(dym.hdpW)) %>% 
          mutate(dym.hdpW = as.numeric(str_replace_na(dym.hdpW, 1)))
        
      } else {
        stop('Kindly assign breakdown = TRUE, breakdown = FALSE or breakdown = 1, breakdown = 0 as logical value required by system.')
      }
      
    } else {
      stop('Kindly choose weight.type == "all", weight.type == "annual", weight.type == "daily", weight.type == "time" or weight.type == "dynamic".')
    }
      
  } else {
    stop('Kindly choose type = "summary", type = "weight.stakes" or type = "weight".')
  }
  
    ## --------------------- Return function --------------------------------------
  
  if(type == 'summary' | type == 'weight.stakes') {
    
    return(lRiskProf)
    
  } else if(type == 'weight') {
    
    return(weightProf)
    
  }
}
