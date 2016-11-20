vKelly <- function(mbase, weight.stakes = 1, weight = 1, type = 'flat', dynamic.type = 'time', 
                   adjusted = 1, parallel = FALSE) {
  ## Comparison of various fractional Kelly models
  ## 
  ## Kindly apply readfirmDate() and arrfirmData() prior to measure the various 
  ##   Kelly models.
  ## 
  ## weight.stakes = a numeric weight parameter in single or vector format. You can
  ##   interpret it as a league risk profile. Different league has thier own league's 
  ##   max stakes per bet and also max stakes per match.
  ## weight = a numeric weight parameter in single or vector format. You can 
  ##   interpret it as the weight adjustment for the form of Win-All, Win-Half, Push, 
  ##   Loss-Half, Loss-All and also Cancelled bets. The probability of the outcome of 
  ##   bet on Win-All and Win-Half might come out with different P&L although it is 
  ##   exactly same strength of indexes between both teams.
  ## type = 'flat' : both 'weight.stakes' and 'weight' will only usable for 'flat' type.
  ## All weight type = W1, W2, WS1 or WS2 are annual based parameters unless you choose dynamic.
  ## type = 'W1', type = 'W2', type = 'WS1',  type = 'WS2', type = 'W1WS1', type = 'W1WS2', 
  ##   type = 'W2WS1', type = 'W2WS2'. Once you choose 
  ##   type = either 'weight', both 'weight.stakes' and 'weight' will auto ignore all 
  ##   input value but using previous year data to get a constant weight parameter.
  ##   `theta` will be W1 and `dres` will be W2.
  ## Once you choose dynamic type, the default dynamic.type is 'time' which is based on 
  ##   kick-off time, however you can choose other options like 'daily' or 'dynamic'.
  ## type = 'D1', type = 'D2', type = 'D1WS1', type = 'D1WS2', 
  ##   type = 'D2WS1', type = 'D2WS2', type = 'W1DWS1', type = 'W1DWS2', 
  ##   type = 'D2WS1', type = 'D2WS2', type = 'D1DWS1', type = 'D1DWS2'. 
  ##   type = 'D2DWS1', type = 'D2DWS2'. Once you choose 
  ##   'dynamic' type, both 'weight.stakes' and 'weight'  will auto ignore all input value 
  ##   but using data from previous until latest staked match to generates a vector of 
  ##   weighted parameters.
  ## adjusted = 1, adjusted is a vector with regards the bonus or dividend for fund.
  ##   you might refer to quantmod::adjustOHLC()
  ## parallel = FALSE or parallel = TRUE.
  ## For example : vKelly(mbase, type = 'D1', )
  
  ############################# Start #########################################
  ## Due to the I need to use mean value of reversed probability "rEMProb" as the \pho,
  ##   therefore need to skip the probabilities section and use stakes section.
  ##   Reverse probabilities from stakes via different fractional Kelly models might 
  ##   cause the bias to reverse the real probabiliies similar to rEMProb (due to 
  ##   rEMProb has already the mean value).
  ##   Here I rewrote vKelly2() which skip the prob section and use rEMProb in 
  ##  staking section.
  ############################# End ############################################
  
  ## --------------------- Load packages ----------------------------------------
  options(warn = -1)
  suppressMessages(library('formattable'))
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('stringr'))
  suppressMessages(library('BBmisc'))
  suppressMessages(library('doParallel'))
  suppressMessages(library('quantmod'))
  
  suppressMessages(source('./function/leagueRiskProf.R'))
  suppressMessages(source('./function/KellyPL.R'))
  
  if(parallel == TRUE) {
    #'@ registerDoParallel(cores = detectCores())
    registerDoParallel(cores = 3)
  }
  
  ## --------------------- Data validation --------------------------------------
  if(!is.data.frame(mbase)) {
    stop('Kindly apply the readfirmData() and arrfirmData() in order to turn the data into a fittable data frame.')
  }
  
  wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
  if(!is.numeric(adjusted)) stop('Kindly insert a vector of numeric values.')
  
  ## Re-categorise the soccer financial settlement date. Due to I have no the 
  ##   history matches dataset from bookmakers. The scrapped spbo time is not 
  ##   stable (always change, moreover there just an information website) where 
  ##   firm A is the firm who placed bets with millions HKD (although the 
  ##   kick-off time might also changed after placed that particular bet), 
  ##   therefore I follow the kick-off time of the firm A.
  if(!c('DateUS', 'TimeUS') %in% names(mbase)) {
    mbase <- mbase %>% mutate(
      TimeUS = format(DateUK, tz = 'EST', usetz = TRUE, format = '%Y-%m-%d %H:%M:%S'), 
      DateUS = as.Date(TimeUS))
    mbase <- mbase[order(mbase$TimeUS, mbase$No.x, decreasing = c(FALSE, FALSE)),]
  }
  
  if(!any(grepl('obs', names(mbase)))) {
    ## observation based for dynamic simulation use.
    mbase %<>% mutate(obs = seq(1, nrow(.)))
  }
  
  dateUSID <- sort(unique(mbase$DateUS))
  timeUSID <- sort(unique(mbase$TimeUS))
  
  if(!c('PL.R', 'RebatesS') %in% names(mbase)) {
    mbase <- mbase[order(mbase$No.x),] %>% mutate(PL.R = PL / Stakes, RebatesS = Stakes * Rebates)
  }
  
  ## --------------------- Convert probabilities -------------------------------- 
  ## weighted parameter estimation
  ## 
  ## I moved the `dres` parameter and rewrite the theta and hcpW for weight parameter since 
  ##   the win, win-half, push, loss-hlaf, loss cannot be foreseen. Therefore rewrite handicap based 
  ##   to forecast will be applicable to real life betting.
  
  #'@ mbase %<>% mutate(theta = suppressAll(
  #'@   ifelse(Result == 'Win', 1, 
  #'@   ifelse(Result == 'Half Win', 0.5, 
  #'@   ifelse(Result == 'Push'|Result == 'Cancelled', 0, 
  #'@   ifelse(Result == 'Half Loss', -0.5, 
  #'@   ifelse(Result == 'Loss', -1, NA))))))#, 
    #'@ dWin = ifelse(Result == 'Win', 1, 0), 
    #'@ dwhf = ifelse(Result == 'Half Win', 1, 0), 
    #'@ dpus = ifelse(Result == 'Push'|Result == 'Cancelled', 1, 0), 
    #'@ dlhf = ifelse(Result == 'Half Loss', 1, 0), 
    #'@ dlos = ifelse(Result == 'Loss', 1, 0))
  
  #'@ mbase %<>% mutate(
  #'@   theta = as.numeric(plyr::mapvalues(Result, 
  #'@                 c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
  #'@                 c(1, 0.5, 0, 0, -0.5, -1))), 
  #'@   dWin = ifelse(Result == 'Win', 1, 0), 
  #'@   dwhf = ifelse(Result == 'Half Win', 1, 0), 
  #'@   dpus = ifelse(Result == 'Push' | Result == 'Cancelled', 1, 0), 
  #'@   dlhf = ifelse(Result == 'Half Loss', 1, 0), 
  #'@   dlos = ifelse(Result == 'Loss', 1, 0))
  
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
  
  if(type == 'flat' | type == 'W1' | type == 'W2' | type == 'WS1' | type == 'WS2' | 
     type == 'W1WS1' | type == 'W1WS2' | type == 'W2WS1' | type == 'W2WS2') {
    
    m <- ddply(mbase, .(Sess), summarise, rRates = mean(Return / Stakes), 
               theta = mean(theta)#, dWin = mean(dWin), dwhf = mean(dwhf), 
               #dpus = mean(dpus), dlhf = mean(dlhf), dlos = mean(dlos)
               ) %>% tbl_df
    
    if(any(!c('rRates', 'rEMProbB', 'rEMProbL', 'netEMEdge') %in% names(mbase))){
      mbase$rRates <- rep(as.numeric(m$rRates + 1), 
                          ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase %<>% mutate(rEMProbB = rRates * netProbB, rEMProbL = 1 - rEMProbB, 
                        netEMEdge = rEMProbB / netProbB)
    }
    
    ## --------------------- flat parameters ---------------------------------
    if(type == 'flat') {
    
      if(!is.numeric(weight.stakes)) {
        wt$weight.stakes <- 1 # exp(0) = 1; log(1) = 0
      } else {
        if(!is.vector(weight.stakes)) {
          stop('Kindly insert a range of vector or single numeric value as weight.stakes parameter.')
        } else {
          if(is.vector(weight.stakes)) wt$weight.stakes <- weight.stakes # exp(0) = 1; log(1) = 0
        }
      }
      
      if(!is.numeric(weight)) {
        wt$weight <- 1 # exp(0) = 1; log(1) = 0
      } else {
        if(!is.vector(weight)) {
          stop('Kindly insert a range of vector or single numeric value as weight parameter.')
        } else {
          if(is.vector(weight)) wt$weight <- weight # exp(0) = 1; log(1) = 0
        }
      }
      
    } else {
      
      ## --------------------- weight parameters ---------------------------------
      if(type == 'W1') {
        
        if(file.exists('./data/wProf2A.rds')) {
          if(readRDS('./data/wProf2A.rds') %>% names %>% grepl('annual.theta', .) %>% any) {
            wp <- readRDS('./data/wProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$annual.theta %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(wp)
        
      } else if(type == 'W2') {
        
        if(file.exists('./data/wProf2B.rds')) {
          if(readRDS('./data/wProf2B.rds') %>% names %>% grepl('annual.hdpW', .) %>% any) {
            wp <- readRDS('./data/wProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$annual.hdpW %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(wp)
        
      } else if(type == 'WS1') {
        
        if(file.exists('./data/lRProf2A.rds')) {
          if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
            lrp <- readRDS('./data/lRProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          lrp <- leagueRiskProf(mbase, type = 'weight.stakes', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- 1
        wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.1 %>% 
          str_replace_na(1) %>% as.numeric
        rm(lrp)
        
      } else if(type == 'WS2') {
        
        if(file.exists('./data/lRProf2B.rds')) {
          if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
            lrp <- readRDS('./data/lRProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          lrp <- leagueRiskProf(mbase, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- 1
        wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.2 %>% 
          str_replace_na(1) %>% as.numeric
        rm(lrp)
        
      } else if(type == 'W1WS1') {
        
        if(file.exists('./data/wProf2A.rds')) {
          if(readRDS('./data/wProf2A.rds') %>% names %>% grepl('annual.theta', .) %>% any) {
            wp <- readRDS('./data/wProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        if(file.exists('./data/lRProf2A.rds')) {
          if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
            lrp <- readRDS('./data/lRProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          lrp <- leagueRiskProf(mbase, type = 'weight.stakes', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$annual.theta %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.1 %>% 
          str_replace_na(1) %>% as.numeric
        rm(wp, lrp)
        
      } else if(type == 'W1WS2') {
        
        if(file.exists('./data/wProf2A.rds')) {
          if(readRDS('./data/wProf2A.rds') %>% names %>% grepl('annual.theta', .) %>% any) {
            wp <- readRDS('./data/wProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        if(file.exists('./data/lRProf2B.rds')) {
          if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
            lrp <- readRDS('./data/lRProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          lrp <- leagueRiskProf(mbase, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$annual.theta %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.2 %>% 
          str_replace_na(1) %>% as.numeric
        rm(wp, lrp)
        
      } else if(type == 'W2WS1') {
        
        if(file.exists('./data/wProf2B.rds')) {
          if(readRDS('./data/wProf2B.rds') %>% names %>% grepl('annual.hdpW', .) %>% any) {
            wp <- readRDS('./data/wProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        if(file.exists('./data/lRProf2A.rds')) {
          if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
            lrp <- readRDS('./data/lRProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          lrp <- leagueRiskProf(mbase, type = 'weight.stakes', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$annual.hdpW %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.1 %>% 
          str_replace_na(1) %>% as.numeric
        rm(wp, lrp)
        
      } else if(type == 'W2WS2') {
        
        if(file.exists('./data/wProf2B.rds')) {
          if(readRDS('./data/wProf2B.rds') %>% names %>% grepl('annual.hdpW', .) %>% any) {
            wp <- readRDS('./data/wProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        if(file.exists('./data/lRProf2B.rds')) {
          if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
            lrp <- readRDS('./data/lRProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
              filter(Sess < max(Sess))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          lrp <- leagueRiskProf(mbase, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$annual.hdpW %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.2 %>% 
          str_replace_na(1) %>% as.numeric
        rm(wp, lrp)
      }
    }
    
  } else {
    
    ## measure the current year rRates to know the rEMProbB. 
    m <- ddply(mbase, .(Sess), summarise, rRates = mean(Return / Stakes), 
               theta = mean(theta)#, dWin = mean(dWin), dwhf = mean(dwhf), 
               #dpus = mean(dpus), dlhf = mean(dlhf), dlos = mean(dlos)
    ) %>% tbl_df
    
    if(any(!c('rRates', 'rEMProbB', 'rEMProbL', 'netEMEdge') %in% names(mbase))){
      mbase$rRates <- rep(as.numeric(m$rRates + 1), 
                          ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase %<>% mutate(rEMProbB = rRates * netProbB, rEMProbL = 1 - rEMProbB, 
                        netEMEdge = rEMProbB / netProbB)
    }
    
    ## --------------------- dynamic parameters ---------------------------------
    if(type == 'D1'){
      
      if(!any(grepl('obs', names(mbase)))) {
        ## observation based for dynamic simulation use.
        mbase %<>% mutate(obs = seq(1, nrow(.)))
      }
      
      if(dynamic.type == 'daily') {
        
        if(file.exists('./data/wProf3A.rds')) {
          if(readRDS('./data/wProf3A.rds') %>% names %>% grepl('daily.theta', .) %>% any) {
            wp <- readRDS('./data/wProf3A.rds') %>% 
              mutate(DateUS = DateUS + 1) %>% filter(DateUS < max(DateUS))
            ## weighted value lag one day since use today's weighted
            ##   parameter to predict tomorrow's matches.
            
            ################## need to modify as survSplit() event panel dataset ####################
            ## create a markov chain data set for event analysis
            wp <- data_frame(DateUS = rep(unique(wp$DateUS), each = length(factor(unique(wp$Result))))) %>% 
              mutate(Result = rep(sort(factor(unique(wp$Result))), times = length(unique(wp$DateUS)))) %>% 
              join(wp) %>% tbl_df %>% mutate(daily.theta = as.numeric(str_replace_na(daily.theta, 1)))
            ##################
            #'@ data_frame(DateUS = rep(unique(wp$DateUS), each = length(factor(unique(wp$Result))))) %>% 
            #'@ mutate(Result = rep(sort(factor(unique(wp$Result))), times = length(unique(wp$DateUS)))) %>% 
            #'@   mutate(.id = mapvalues(wp$DateUS, from = dateUSID, to = as.numeric(dateUSID)))
            ###################
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'daily') %>% 
            mutate(DateUS = DateUS + 1) %>% filter(DateUS < max(DateUS))
          ## weighted value lag one day since use today's weighted
          ##   parameter to predict tomorrow's matches.
          
          ################## need to modify as survSplit() event panel dataset ####################
          ## create a markov chain data set for event analysis
          wp <- data_frame(DateUS = rep(unique(wp$DateUS), each = length(factor(unique(wp$Result))))) %>% 
            mutate(Result = rep(sort(factor(unique(wp$Result))), times = length(unique(wp$DateUS)))) %>% 
            mutate(.id = mapvalues(DateUS, unique(DateUS), seq(unique(DateUS))), .) %>% 
            join(wp) %>% tbl_df %>% mutate(daily.theta = as.numeric(str_replace_na(daily.theta, 1)))
          ##################
          #'@ data_frame(DateUS = rep(unique(wp$DateUS), each = length(factor(unique(wp$Result))))) %>% 
          #'@ mutate(Result = rep(sort(factor(unique(wp$Result))), times = length(unique(wp$DateUS)))) %>% 
          #'@   mutate(.id = mapvalues(wp$DateUS, from = dateUSID, to = as.numeric(dateUSID)))
          ###################
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$daily.theta %>% 
          str_replace_na(1) %>% as.numeric #### need to review since the join() sounds unable get desired result but all NA values.
        wt$weight.stakes <- 1
        rm(wp)
        
      } else if (dynamic.type == 'time') {
        
        if(file.exists('./data/wProf4A.rds')) {
          if(readRDS('./data/wProf4A.rds') %>% names %>% grepl('time.theta', .) %>% any) {
            wp <- readRDS('./data/wProf4A.rds')
            dt1 <- unique(wp$TimeUS)
            dt2 <- lead(unique(wp$TimeUS))
            wp %<>% mutate(TimeUS = mapvalues(TimeUS, from = unique(TimeUS), 
                                              to = as.character(lead(unique(TimeUS))), 
                                              warn_missing = FALSE)) %>% na.omit
            ## weighted value lag one kick-off time since use current 
            ##   kick-off time weighted parameter to predict next kick-off time matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'time')
          dt1 <- unique(wp$TimeUS)
          dt2 <- lead(unique(wp$TimeUS))
          wp %<>% mutate(TimeUS = mapvalues(TimeUS, from = unique(TimeUS), 
                                            to = as.character(lead(unique(TimeUS))), 
                                            warn_missing = FALSE)) %>% na.omit
          ## weighted value lag one day since use today's weighted
          ##   parameter to predict tomorrow matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$time.theta %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(dt1, dt2, wp)
        
      } else if (dynamic.type == 'dynamic') {
        
        if(!any(grepl('obs', names(mbase)))) {
          ## observation based for dynamic simulation use.
          mbase %<>% mutate(obs = seq(1, nrow(.)))
        }
        
        if(file.exists('./data/wProf5A.rds')) {
          if(readRDS('./data/wProf5A.rds') %>% names %>% grepl('dym.theta', .) %>% any) {
            wp <- readRDS('./data/wProf5A.rds')
            dt1 <- unique(wp$obs)
            dt2 <- lead(unique(wp$obs))
            wp %<>% mutate(obs = mapvalues(obs, from = unique(obs), 
                                           to = as.character(lead(unique(obs))), 
                                           warn_missing = FALSE)) %>% na.omit
            ## weighted value lag one kick-off time since use current 
            ##   kick-off time weighted parameter to predict next kick-off time matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'time')
          dt1 <- unique(wp$obs)
          dt2 <- lead(unique(wp$obs))
          wp %<>% mutate(obs = mapvalues(obs, from = unique(obs), 
                                         to = as.character(lead(unique(obs))), 
                                         warn_missing = FALSE)) %>% na.omit
          ## weighted value lag one day since use today's weighted
          ##   parameter to predict tomorrow matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$time.theta %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(dt1, dt2, wp)
        
      } else {
        
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D2') {
      
      ## ========================= start wrong ========================================
      ## due to ddply(, (.TimeUS)) function just split the data base on specific time
      ## 
      #'@ dw <- ddply(mbase, .(TimeUS), summarise, 
      #'@             thetac = sum(theta), thetan = length(theta),
      #'@             dWinc = sum(dWin), dWinn = length(dWin), 
      #'@             dwhfc = sum(dwhf), dwhfn = length(dwhf), 
      #'@             dpusc = sum(dpus), dpusn = length(dpus), 
      #'@             dlhfc = sum(dlhf), dlhfn = length(dlhf), 
      #'@             dlosc = sum(dlos), dlosn = length(dlos), 
      #'@             .parallel = parallel) %>% tbl_df %>% 
      #'@   mutate(lagTimeUS = c(0, lag(TimeUS)[-1])) %>% .[-1] %>% # drop the TimeUS to 
      #'@   mutate(thetas = cumsum(thetac)/cumsum(thetan),          #  avoid duplicate column
      #'@          dWins = cumsum(dWinc)/cumsum(dWinn),             #  TimeUS after join()
      #'@          dwhfs = cumsum(dwhfc)/cumsum(dwhfn), 
      #'@          dpuss = cumsum(dpusc)/cumsum(dpusn), 
      #'@          dlhfs = cumsum(dlhfc)/cumsum(dlhfn), 
      #'@          dloss = cumsum(dlosc)/cumsum(dlosn))
      
      #'@ mbase %<>% mutate(lagTimeUS = c(0, lag(TimeUS)[-1]))
      #'@ mbase %<>% join(dw, by = 'lagTimeUS') %>% tbl_df %>% 
      #'@   mutate(dres = suppressAll(
      #'@     ifelse(Result == 'Win', dWins, ## dres value will be W2
      #'@     ifelse(Result == 'Half Win', dwhfs, 
      #'@     ifelse(Result == 'Push'|Result == 'Cancelled', dpuss, 
      #'@     ifelse(Result == 'Half Loss', dlhfs, 
      #'@     ifelse(Result == 'Loss', dloss, NA)))))))
      
      #'@ mbase %<>% filter(Sess != unique(Sess)[1])
      #'@ wt <- data_frame(No = seq(nrow(mbase)))
      #'@ wt$weight <- exp(mbase$dres)
      #'@ wt$weight.stakes <- weight.stakes
      ## ============================ end wrong ============================
      
      if(!any(grepl('obs', names(mbase)))) {
        ## observation based for dynamic simulation use.
        mbase %<>% mutate(obs = seq(1, nrow(.)))
      }
      
      if(dynamic.type == 'daily') {
        
        if(file.exists('./data/wProf3B.rds')) {
          if(readRDS('./data/wProf3B.rds') %>% names %>% grepl('daily.hdpC', .) %>% any) {
            wp <- readRDS('./data/wProf3B.rds') %>% 
              mutate(DateUS = DateUS + 1) %>% filter(DateUS < max(DateUS))
              ## weighted value lag one day since use today's weighted
              ##   parameter to predict tomorrow's matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', breakdown = TRUE, weight.type = 'daily') %>% 
            mutate(DateUS = DateUS + 1) %>% filter(DateUS < max(DateUS))
            ## weighted value lag one day since use today's weighted
            ##   parameter to predict tomorrow's matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$daily.hdpC %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(wp)
        
      } else if (dynamic.type == 'time') {
        
        if(file.exists('./data/wProf4B.rds')) {
          if(readRDS('./data/wProf4B.rds') %>% names %>% grepl('time.hdpC', .) %>% any) {
            wp <- readRDS('./data/wProf4B.rds')
            dt1 <- unique(wp$TimeUS)
            dt2 <- lead(unique(wp$TimeUS))
            wp %<>% mutate(
              TimeUS = mapvalues(TimeUS, from = unique(TimeUS), 
                                 to = as.character(lead(unique(TimeUS))), 
                                 warn_missing = FALSE)) %>% na.omit
              ## weighted value lag one kick-off time since use current 
              ##   kick-off time weighted parameter to predict next kick-off time matches.
          
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', breakdown = TRUE, weight.type = 'time')
          dt1 <- unique(wp$TimeUS)
          dt2 <- lead(unique(wp$TimeUS))
          wp %<>% mutate(
            TimeUS = mapvalues(TimeUS, from = unique(TimeUS), 
                               to = as.character(lead(unique(TimeUS))), 
                               warn_missing = FALSE)) %>% na.omit
            ## weighted value lag one day since use today's weighted
            ##   parameter to predict tomorrow matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$time.hdpC %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(dt1, dt2, wp)
        
      } else if (dynamic.type == 'dynamic') {
        
        if(!any(grepl('obs', names(mbase)))) {
          ## observation based for dynamic simulation use.
          mbase %<>% mutate(obs = seq(1, nrow(.)))
        }
        
        if(file.exists('./data/wProf5B.rds')) {
          if(readRDS('./data/wProf5B.rds') %>% names %>% grepl('dym.hdpC', .) %>% any) {
            wp <- readRDS('./data/wProf5B.rds')
            dt1 <- unique(wp$obs)
            dt2 <- lead(unique(wp$obs))
            wp %<>% mutate(
              obs = mapvalues(obs, from = unique(obs), 
                              to = as.character(lead(unique(obs))), 
                              warn_missing = FALSE)) %>% na.omit
              ## weighted value lag one kick-off time since use current 
              ##   kick-off time weighted parameter to predict next kick-off time matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', breakdown = TRUE, weight.type = 'time')
          dt1 <- unique(wp$obs)
          dt2 <- lead(unique(wp$obs))
          wp %<>% mutate(
            obs = mapvalues(obs, from = unique(obs), 
                            to = as.character(lead(unique(obs))), 
                            warn_missing = FALSE)) %>% na.omit
            ## weighted value lag one day since use today's weighted
            ##   parameter to predict tomorrow matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$time.hdpC %>% 
          str_replace_na(1) %>% as.numeric
        wt$weight.stakes <- 1
        rm(dt1, dt2, wp)
        
      } else {
        
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D1WS1'){
      
      if(!any(grepl('obs', names(mbase)))) {
        ## observation based for dynamic simulation use.
        mbase %<>% mutate(obs = seq(1, nrow(.)))
      }
      
      if(file.exists('./data/lRProf2A.rds')) {
        if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
          lrp <- readRDS('./data/lRProf2A.rds') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        } else {
          stop('Kindly use default preset data set.')
        }
      } else {
        lrp <- leagueRiskProf(mbase, type = 'weight.stakes', weight.type = 'annual') %>% 
          mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
          filter(Sess < max(Sess))
      }
      
      wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
      wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.1 %>% 
        str_replace_na(1) %>% as.numeric
      rm(lrp)
      
      if(dynamic.type == 'daily') {
        
        if(file.exists('./data/wProf3A.rds')) {
          if(readRDS('./data/wProf3A.rds') %>% names %>% grepl('daily.theta', .) %>% any) {
            wp <- readRDS('./data/wProf3A.rds') %>% 
              mutate(DateUS = DateUS + 1) %>% filter(DateUS < max(DateUS))
              ## weighted value lag one day since use today's weighted
              ##   parameter to predict tomorrow's matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'daily') %>% 
            mutate(DateUS = DateUS + 1) %>% filter(DateUS < max(DateUS))
            ## weighted value lag one day since use today's weighted
            ##   parameter to predict tomorrow's matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$daily.theta %>% 
          str_replace_na(1) %>% as.numeric ##
        rm(wp)
        
      } else if (dynamic.type == 'time') {
        
        if(file.exists('./data/wProf4A.rds')) {
          if(readRDS('./data/wProf4A.rds') %>% names %>% grepl('time.theta', .) %>% any) {
            wp <- readRDS('./data/wProf4A.rds')
            dt1 <- unique(wp$TimeUS)
            dt2 <- lead(unique(wp$TimeUS))
            wp %<>% mutate(TimeUS = mapvalues(TimeUS, from = unique(TimeUS), 
                                              to = as.character(lead(unique(TimeUS))), 
                                              warn_missing = FALSE)) %>% na.omit
            ## weighted value lag one kick-off time since use current 
            ##   kick-off time weighted parameter to predict next kick-off time matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'time')
          dt1 <- unique(wp$TimeUS)
          dt2 <- lead(unique(wp$TimeUS))
          wp %<>% mutate(TimeUS = mapvalues(TimeUS, from = unique(TimeUS), 
                                            to = as.character(lead(unique(TimeUS))), 
                                            warn_missing = FALSE)) %>% na.omit
          ## weighted value lag one day since use today's weighted
          ##   parameter to predict tomorrow matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$time.theta %>% 
          str_replace_na(1) %>% as.numeric
        rm(dt1, dt2, wp)
        
      } else if (dynamic.type == 'dynamic') {
        
        if(!any(grepl('obs', names(mbase)))) {
          ## observation based for dynamic simulation use.
          mbase %<>% mutate(obs = seq(1, nrow(.)))
        }
        
        if(file.exists('./data/wProf5A.rds')) {
          if(readRDS('./data/wProf5A.rds') %>% names %>% grepl('dym.theta', .) %>% any) {
            wp <- readRDS('./data/wProf5A.rds')
            dt1 <- unique(wp$obs)
            dt2 <- lead(unique(wp$obs))
            wp %<>% mutate(obs = mapvalues(obs, from = unique(obs), 
                                            to = as.character(lead(unique(obs))), 
                                            warn_missing = FALSE)) %>% na.omit
            ## weighted value lag one kick-off time since use current 
            ##   kick-off time weighted parameter to predict next kick-off time matches.
            
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          wp <- leagueRiskProf(mbase, type = 'weight', weight.type = 'time')
          dt1 <- unique(wp$obs)
          dt2 <- lead(unique(wp$obs))
          wp %<>% mutate(obs = mapvalues(obs, from = unique(obs), 
                                         to = as.character(lead(unique(obs))), 
                                         warn_missing = FALSE)) %>% na.omit
          ## weighted value lag one day since use today's weighted
          ##   parameter to predict tomorrow matches.
        }
        
        #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                       #   the growth rate from same initial fund size.
        wt$weight <- suppressAll(join(mbase, wp)) %>% tbl_df %>% .$time.theta %>% 
          str_replace_na(1) %>% as.numeric
        rm(dt1, dt2, wp)
        
      } else {
        
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D1WS2'){
      
      if(!any(grepl('obs', names(mbase)))) {
        ## observation based for dynamic simulation use.
        mbase %<>% mutate(obs = seq(1, nrow(.)))
      }
      
      if(file.exists('./data/lRProf2B.rds')) {
        if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
          lrp <- readRDS('./data/lRProf2B.rds') %>% 
            mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
            filter(Sess < max(Sess))
        } else {
          stop('Kindly use default preset data set.')
        }
      } else {
        lrp <- leagueRiskProf(mbase, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual') %>% 
          mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE)) %>% 
          filter(Sess < max(Sess))
      }
      
      #'@ mbase %<>% filter(Sess != unique(Sess)[1]) # keep the initial year as flat model to compare
                                                     #   the growth rate from same initial fund size.
      wt <- data_frame(No = seq(nrow(mbase)), weight = weight, weight.stakes = weight.stakes)
      wt$weight.stakes <- suppressAll(join(mbase, lrp)) %>% tbl_df %>% .$annual.2 %>% 
        str_replace_na(1) %>% as.numeric
      rm(lrp)
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D2WS1'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D2WS2'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'W1DWS1'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'W1DWS2'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'W2DWS1'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'W2DWS2'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D1DWS1'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D1DWS2'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D1DWS1'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else if(type == 'D1DWS2'){
      
      if(dynamic.type == 'daily') {
        aaaa
      } else if (dynamic.type == 'time') {
        aaaa
      } else if (dynamic.type == 'dynamic') {
        aaa
      } else {
        stop('Kindly select dynamic.type = "daily", dynamic.type = "time" or dynamic.type = "dynamic".')
      }
      
    } else {
      stop('Kindly choose the parameter `type = flat`, `type = W1`, `type = W2`, `type = D1` or `type = D2`.')
    }
  }
  ## ====================== Kelly weight 1 prob ===========================
  ## The weight.stakes and weight parameters will be equal to 1 if there has no 
  ##   any value insert or insert as 1.
  
  ## Weight for probabilities and weight.stakes for stakes adjustment both 
  ##   applied within fraction.
  ## 
  K1 <- mbase %>% select(TimeUS, DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                         Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, 
                         netProbB, netProbL, rEMProbB, rEMProbL) %>% cbind(wt) %>% select(-No)
  
  K1 %<>% mutate(
    
    PropHKPriceEdge = ((weight * Stakes * netEMEdge) * HKPrice + 1) / EUPrice, 
    
    PropnetProbBEdge = ((weight * Stakes * netEMEdge) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
    
    KProbHKPrice = (weight * (Stakes) * HKPrice + 1) / EUPrice, 
    
    KProbnetProbB = (weight * (Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB),
    
    KProbFixed = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
    
    KProbFixednetProbB = exp(((log(weight * Stakes)) * (1 / netProbB - 1) + 1) / (1 / netProbB)),
    
    KEMProb = (weight * rEMProbB * HKPrice + 1) / EUPrice, 
    
    KEMProbnetProbB = (weight * rEMProbB *  (1 / netProbB - 1) + 1) / (1 / netProbB),
    
    KProbHalf = (weight * (0.5 * Stakes) * HKPrice + 1) / EUPrice, 
    
    KProbHalfnetProbB = (weight * (0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
    
    KProbQuarter = (weight * (0.25 * Stakes) * HKPrice + 1) / EUPrice, 
    
    KProbQuarternetProbB = (weight * (0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
    
    KProbAdj = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
    
    KProbAdjnetProbB = exp((log(weight * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
    
    KHalfAdj = exp((log(weight * 0.5 * Stakes) * HKPrice + 1) / EUPrice), 
    
    KHalfAdjnetProbB = exp((log(weight * 0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
    
    KEMQuarterAdj = exp((log(weight * 0.25 * Stakes) * HKPrice + 1) / EUPrice), 
    
    KEMQuarterAdjnetProbB = exp((log(weight * 0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
    
    ## --------------------- Kelly weight 1 stakes ---------------------------------
    KStakesHKPriceEdge = ((weight.stakes * PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice, 
    
    KStakesnetProbBEdge = ((weight.stakes * PropnetProbBEdge * netEMEdge) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
    
    KStakesHKPrice = ((weight.stakes * KProbHKPrice) * EUPrice - 1) / HKPrice, 
    
    KStakesnetProbB = ((weight.stakes * KProbnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
    
    KStakesFixed = exp((log(weight.stakes * KProbFixed) * EUPrice - 1) / HKPrice), 
    
    KStakesFixednetProbB = exp(((log(weight.stakes * KProbFixednetProbB)) * (1 / netProbB) - 1) / (1 / netProbB - 1)),
    
    KStakesEMProb = (weight.stakes * KEMProb * EUPrice - 1) / HKPrice, 
    
    KStakesEMProbnetProbB = (weight.stakes * KEMProbnetProbB *  (1 / netProbB) - 1) / (1 / netProbB - 1),
    
    KStakesHalf = ((weight.stakes * 0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
    
    KStakesHalfnetProbB = (weight.stakes * (0.5 * KProbHalfnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
    
    KStakesQuarter = (weight.stakes * (0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
    
    KStakesQuarternetProbB = (weight.stakes * (0.25 * KProbQuarternetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
    
    KStakesAdj = exp((log(weight.stakes * KProbAdj) * EUPrice - 1) / HKPrice), 
    
    KStakesAdjnetProbB = exp((log(weight.stakes * KProbAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
    
    KStakesHalfAdj = exp((log(weight.stakes * 0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
    
    KStakesHalfAdjnetProbB = exp((log(weight.stakes * 0.5 * KHalfAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
    
    KStakesEMQuarterAdj = exp((log(weight.stakes * 0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
    
    KStakesEMQuarterAdjnetProbB = exp((log(weight.stakes * 0.25 * KEMQuarterAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1))
    
  ) %>% tbl_df %>% mutate(
    KStakesHKPriceEdge = ifelse(KStakesHKPriceEdge > 0, KStakesHKPriceEdge, 0), 
    KStakesnetProbBEdge = ifelse(KStakesnetProbBEdge > 0, KStakesnetProbBEdge, 0), 
    KStakesHKPrice = ifelse(KStakesHKPrice > 0, KStakesHKPrice, 0), 
    KStakesnetProbB = ifelse(KStakesnetProbB > 0, KStakesnetProbB, 0), 
    KStakesFixed = ifelse(KStakesFixed > 0, KStakesFixed, 0), 
    KStakesFixednetProbB = ifelse(KStakesFixednetProbB > 0, KStakesFixednetProbB, 0), 
    KStakesEMProb = ifelse(KStakesEMProb > 0, KStakesEMProb, 0), 
    KStakesEMProbnetProbB = ifelse(KStakesEMProbnetProbB > 0, KStakesEMProbnetProbB, 0), 
    KStakesHalf = ifelse(KStakesHalf > 0, KStakesHalf, 0), 
    KStakesHalfnetProbB = ifelse(KStakesHalfnetProbB > 0, KStakesHalfnetProbB, 0), 
    KStakesQuarter = ifelse(KStakesQuarter > 0, KStakesQuarter, 0), 
    KStakesQuarternetProbB = ifelse(KStakesQuarternetProbB > 0, KStakesQuarternetProbB, 0), 
    KStakesAdj = ifelse(KStakesAdj > 0, KStakesAdj, 0), 
    KStakesAdjnetProbB = ifelse(KStakesAdjnetProbB > 0, KStakesAdjnetProbB, 0), 
    KStakesHalfAdj = ifelse(KStakesHalfAdj > 0, KStakesHalfAdj, 0), 
    KStakesHalfAdjnetProbB = ifelse(KStakesHalfAdjnetProbB > 0, KStakesHalfAdjnetProbB, 0), 
    KStakesEMQuarterAdj = ifelse(KStakesEMQuarterAdj > 0, KStakesEMQuarterAdj, 0), 
    KStakesEMQuarterAdjnetProbB = ifelse(KStakesEMQuarterAdjnetProbB > 0, KStakesEMQuarterAdjnetProbB, 0)
  ) %>% na.omit
  
  ## ==================== Kelly weight 2 prob ========================================
  ## The models will be execute only when all weight.stakes and all weight 
  ##   parameters or vectors value not equal to 1.
  ## 
  
  ## Weight for probabilities applied within fraction but weight.stakes for stakes 
  ##   adjustment applied outside the fraction.
  ## 
  if((all(wt$weight.stakes != 1) | all(wt$weight != 1)) | 
     type == 'W1' | type == 'W2' | type == 'D1' | type == 'D2') {
    K2 <- mbase %>% select(TimeUS, DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                           Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, 
                           netProbB, netProbL, rEMProbB, rEMProbL) %>% cbind(wt) %>% select(-No)
    
    K2 %<>% mutate(
      PropHKPriceEdge = ((weight * Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
      
      PropnetProbBEdge = ((weight * Stakes * netEMEdge) * (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbHKPrice = (weight * (Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbnetProbB = (weight * (Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbFixed = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbFixednetProbB = exp(((log(weight * Stakes)) * (1 / netProbB - 1) + 1) / (1 / netProbB)),
      
      KEMProb = (weight * rEMProbB * HKPrice + 1) / EUPrice, 
      
      KEMProbnetProbB = (weight * rEMProbB *  (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbHalf = (weight * (0.5 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbHalfnetProbB = (weight * (0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
      
      KProbQuarter = (weight * (0.25 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbQuarternetProbB = (weight * (0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
      
      KProbAdj = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbAdjnetProbB = exp((log(weight * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      KHalfAdj = exp((log(weight * 0.5 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KHalfAdjnetProbB = exp((log(weight * 0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      KEMQuarterAdj = exp((log(weight * 0.25 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KEMQuarterAdjnetProbB = exp((log(weight * 0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      ## --------------------- Kelly weight 2 stakes ---------------------------------
      KStakesHKPriceEdge = weight.stakes * ((PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
      
      KStakesnetProbBEdge = weight.stakes * ((PropnetProbBEdge * netEMEdge) * (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesHKPrice = weight.stakes * ((KProbHKPrice) * EUPrice - 1) / HKPrice, 
      
      KStakesnetProbB = weight.stakes * ((KProbnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesFixed = weight.stakes * exp((log(KProbFixed) * EUPrice - 1) / HKPrice), 
      
      KStakesFixednetProbB = weight.stakes * exp(((log(KProbFixednetProbB)) * (1 / netProbB) - 1) / (1 / netProbB - 1)),
      
      KStakesEMProb = weight.stakes * (KEMProb * EUPrice - 1) / HKPrice, 
      
      KStakesEMProbnetProbB = weight.stakes * (KEMProbnetProbB *  (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesHalf = weight.stakes * ((0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
      
      KStakesHalfnetProbB = weight.stakes * ((0.5 * KProbHalfnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
      
      KStakesQuarter = weight.stakes * ((0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
      
      KStakesQuarternetProbB = weight.stakes * ((0.25 * KProbQuarternetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
      
      KStakesAdj = weight.stakes * exp((log(KProbAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesAdjnetProbB = weight.stakes * exp((log(KProbAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
      
      KStakesHalfAdj = weight.stakes * exp((log(0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesHalfAdjnetProbB = weight.stakes * exp((log(0.5 * KHalfAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
      
      KStakesEMQuarterAdj = weight.stakes * exp((log(0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesEMQuarterAdjnetProbB = weight.stakes * exp((log(0.25 * KEMQuarterAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1))
      
    ) %>% tbl_df %>% mutate(
      KStakesHKPriceEdge = ifelse(KStakesHKPriceEdge > 0, KStakesHKPriceEdge, 0), 
      KStakesnetProbBEdge = ifelse(KStakesnetProbBEdge > 0, KStakesnetProbBEdge, 0), 
      KStakesHKPrice = ifelse(KStakesHKPrice > 0, KStakesHKPrice, 0), 
      KStakesnetProbB = ifelse(KStakesnetProbB > 0, KStakesnetProbB, 0), 
      KStakesFixed = ifelse(KStakesFixed > 0, KStakesFixed, 0), 
      KStakesFixednetProbB = ifelse(KStakesFixednetProbB > 0, KStakesFixednetProbB, 0), 
      KStakesEMProb = ifelse(KStakesEMProb > 0, KStakesEMProb, 0), 
      KStakesEMProbnetProbB = ifelse(KStakesEMProbnetProbB > 0, KStakesEMProbnetProbB, 0), 
      KStakesHalf = ifelse(KStakesHalf > 0, KStakesHalf, 0), 
      KStakesHalfnetProbB = ifelse(KStakesHalfnetProbB > 0, KStakesHalfnetProbB, 0), 
      KStakesQuarter = ifelse(KStakesQuarter > 0, KStakesQuarter, 0), 
      KStakesQuarternetProbB = ifelse(KStakesQuarternetProbB > 0, KStakesQuarternetProbB, 0), 
      KStakesAdj = ifelse(KStakesAdj > 0, KStakesAdj, 0), 
      KStakesAdjnetProbB = ifelse(KStakesAdjnetProbB > 0, KStakesAdjnetProbB, 0), 
      KStakesHalfAdj = ifelse(KStakesHalfAdj > 0, KStakesHalfAdj, 0), 
      KStakesHalfAdjnetProbB = ifelse(KStakesHalfAdjnetProbB > 0, KStakesHalfAdjnetProbB, 0), 
      KStakesEMQuarterAdj = ifelse(KStakesEMQuarterAdj > 0, KStakesEMQuarterAdj, 0), 
      KStakesEMQuarterAdjnetProbB = ifelse(KStakesEMQuarterAdjnetProbB > 0, KStakesEMQuarterAdjnetProbB, 0)
    ) %>% na.omit
    
    ## ==================== Kelly weight 3 prob ========================================
    ## The models will be execute only when all weight.stakes and all weight 
    ##   parameters or vectors value not equal to 1.
    ## 

    ## Weight for probabilities applied outside the fraction but weight.stakes for 
    ## stakes adjustment applied within the fraction.
    ## 
    K3 <- mbase %>% select(TimeUS, DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                           Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, 
                           netProbB, netProbL, rEMProbB, rEMProbL) %>% cbind(wt) %>% select(-No)
    
    K3 %<>% mutate(
      
      PropHKPriceEdge = weight * ((Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
      
      PropnetProbBEdge = weight * ((Stakes * netEMEdge) * (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbHKPrice = weight * ((Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbnetProbB = weight * ((Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbFixed = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbFixednetProbB = weight * exp(((log(Stakes)) * (1 / netProbB - 1) + 1) / (1 / netProbB)),
      
      KEMProb = weight * (rEMProbB * HKPrice + 1) / EUPrice, 
      
      KEMProbnetProbB = weight * (rEMProbB *  (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbHalf = weight * ((0.5 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbHalfnetProbB = weight * ((0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
      
      KProbQuarter = weight * ((0.25 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbQuarternetProbB = weight * ((0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
      
      KProbAdj = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbAdjnetProbB = weight * exp((log(Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      KHalfAdj = weight * exp((log(0.5 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KHalfAdjnetProbB = weight * exp((log(0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      KEMQuarterAdj = weight * exp((log(0.25 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KEMQuarterAdjnetProbB = weight * exp((log(0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      ## --------------------- Kelly weight 3 stakes ---------------------------------
      KStakesHKPriceEdge = ((weight.stakes * PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
      
      KStakesnetProbBEdge = ((weight.stakes * PropnetProbBEdge * netEMEdge) * (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesHKPrice = ((weight.stakes * KProbHKPrice) * EUPrice - 1) / HKPrice, 
      
      KStakesnetProbB = ((weight.stakes * KProbnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesFixed = exp((log(weight.stakes * KProbFixed) * EUPrice - 1) / HKPrice), 
      
      KStakesFixednetProbB = exp(((log(weight.stakes * KProbFixednetProbB)) * (1 / netProbB) - 1) / (1 / netProbB - 1)),
      
      KStakesEMProb = (weight.stakes * KEMProb * EUPrice - 1) / HKPrice, 
      
      KStakesEMProbnetProbB = (weight.stakes * KEMProbnetProbB *  (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesHalf = (weight.stakes * (0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
      
      KStakesHalfnetProbB = (weight.stakes * (0.5 * KProbHalfnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
      
      KStakesQuarter = (weight.stakes * (0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
      
      KStakesQuarternetProbB = (weight.stakes * (0.25 * KProbQuarternetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
      
      KStakesAdj = exp((log(weight.stakes * KProbAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesAdjnetProbB = exp((log(weight.stakes * KProbAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
      
      KStakesHalfAdj = exp((log(weight.stakes * 0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesHalfAdjnetProbB = exp((log(weight.stakes * 0.5 * KHalfAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
      
      KStakesEMQuarterAdj = exp((log(weight.stakes * 0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesEMQuarterAdjnetProbB = exp((log(weight.stakes * 0.25 * KEMQuarterAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1))
      
    ) %>% tbl_df %>% mutate(
      KStakesHKPriceEdge = ifelse(KStakesHKPriceEdge > 0, KStakesHKPriceEdge, 0), 
      KStakesnetProbBEdge = ifelse(KStakesnetProbBEdge > 0, KStakesnetProbBEdge, 0), 
      KStakesHKPrice = ifelse(KStakesHKPrice > 0, KStakesHKPrice, 0), 
      KStakesnetProbB = ifelse(KStakesnetProbB > 0, KStakesnetProbB, 0), 
      KStakesFixed = ifelse(KStakesFixed > 0, KStakesFixed, 0), 
      KStakesFixednetProbB = ifelse(KStakesFixednetProbB > 0, KStakesFixednetProbB, 0), 
      KStakesEMProb = ifelse(KStakesEMProb > 0, KStakesEMProb, 0), 
      KStakesEMProbnetProbB = ifelse(KStakesEMProbnetProbB > 0, KStakesEMProbnetProbB, 0), 
      KStakesHalf = ifelse(KStakesHalf > 0, KStakesHalf, 0), 
      KStakesHalfnetProbB = ifelse(KStakesHalfnetProbB > 0, KStakesHalfnetProbB, 0), 
      KStakesQuarter = ifelse(KStakesQuarter > 0, KStakesQuarter, 0), 
      KStakesQuarternetProbB = ifelse(KStakesQuarternetProbB > 0, KStakesQuarternetProbB, 0), 
      KStakesAdj = ifelse(KStakesAdj > 0, KStakesAdj, 0), 
      KStakesAdjnetProbB = ifelse(KStakesAdjnetProbB > 0, KStakesAdjnetProbB, 0), 
      KStakesHalfAdj = ifelse(KStakesHalfAdj > 0, KStakesHalfAdj, 0), 
      KStakesHalfAdjnetProbB = ifelse(KStakesHalfAdjnetProbB > 0, KStakesHalfAdjnetProbB, 0), 
      KStakesEMQuarterAdj = ifelse(KStakesEMQuarterAdj > 0, KStakesEMQuarterAdj, 0), 
      KStakesEMQuarterAdjnetProbB = ifelse(KStakesEMQuarterAdjnetProbB > 0, KStakesEMQuarterAdjnetProbB, 0)
    ) %>% na.omit
    
    ## ==================== Kelly weight 4 prob ========================================
    ## The models will be execute only when all weight.stakes and all weight 
    ##   parameters or vectors value not equal to 1.
    ## 
    
    ## Weight for probabilities and weight.stakes for stakes adjustment both applied 
    ##   outside the fraction.
    ## 
    K4 <- mbase %>% select(TimeUS, DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                           Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, 
                           netProbB, netProbL, rEMProbB, rEMProbL) %>% cbind(wt) %>% select(-No)
    
    K4 %<>% mutate(
      PropHKPriceEdge = weight * ((Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
      
      PropnetProbBEdge = weight * ((Stakes * netEMEdge) * (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbHKPrice = weight * ((Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbnetProbB = weight * ((Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbFixed = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbFixednetProbB = weight * exp(((log(Stakes)) * (1 / netProbB - 1) + 1) / (1 / netProbB)),
      
      KEMProb = weight * (rEMProbB * HKPrice + 1) / EUPrice, 
      
      KEMProbnetProbB = weight * (rEMProbB *  (1 / netProbB - 1) + 1) / (1 / netProbB),
      
      KProbHalf = weight * ((0.5 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbHalfnetProbB = weight * ((0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
      
      KProbQuarter = weight * ((0.25 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbQuarternetProbB = weight * ((0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB), 
      
      KProbAdj = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbAdjnetProbB = weight * exp((log(Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      KHalfAdj = weight * exp((log(0.5 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KHalfAdjnetProbB = weight * exp((log(0.5 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      KEMQuarterAdj = weight * exp((log(0.25 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KEMQuarterAdjnetProbB = weight * exp((log(0.25 * Stakes) * (1 / netProbB - 1) + 1) / (1 / netProbB)), 
      
      ## --------------------- Kelly weight 4 stakes ---------------------------------
      KStakesHKPriceEdge = weight.stakes * ((PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
      
      KStakesnetProbBEdge = weight.stakes * ((PropnetProbBEdge * netEMEdge) * (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesHKPrice = weight.stakes * ((KProbHKPrice) * EUPrice - 1) / HKPrice, 
      
      KStakesnetProbB = weight.stakes * ((KProbnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesFixed = weight.stakes * exp((log(KProbFixed) * EUPrice - 1) / HKPrice), 
      
      KStakesFixednetProbB = weight.stakes * exp(((log(KProbFixednetProbB)) * (1 / netProbB) - 1) / (1 / netProbB - 1)),
      
      KStakesEMProb = weight.stakes * (KEMProb * EUPrice - 1) / HKPrice, 
      
      KStakesEMProbnetProbB = weight.stakes * (KEMProbnetProbB *  (1 / netProbB) - 1) / (1 / netProbB - 1),
      
      KStakesHalf = weight.stakes * ((0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
      
      KStakesHalfnetProbB = weight.stakes * ((0.5 * KProbHalfnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
      
      KStakesQuarter = weight.stakes * ((0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
      
      KStakesQuarternetProbB = weight.stakes * ((0.25 * KProbQuarternetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
      
      KStakesAdj = weight.stakes * exp((log(KProbAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesAdjnetProbB = weight.stakes * exp((log(KProbAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
      
      KStakesHalfAdj = weight.stakes * exp((log(0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesHalfAdjnetProbB = weight.stakes * exp((log(0.5 * KHalfAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1)), 
      
      KStakesEMQuarterAdj = weight.stakes * exp((log(0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesEMQuarterAdjnetProbB = weight.stakes * exp((log(0.25 * KEMQuarterAdjnetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1))
      
    ) %>% tbl_df %>% mutate(
      KStakesHKPriceEdge = ifelse(KStakesHKPriceEdge > 0, KStakesHKPriceEdge, 0), 
      KStakesnetProbBEdge = ifelse(KStakesnetProbBEdge > 0, KStakesnetProbBEdge, 0), 
      KStakesHKPrice = ifelse(KStakesHKPrice > 0, KStakesHKPrice, 0), 
      KStakesnetProbB = ifelse(KStakesnetProbB > 0, KStakesnetProbB, 0), 
      KStakesFixed = ifelse(KStakesFixed > 0, KStakesFixed, 0), 
      KStakesFixednetProbB = ifelse(KStakesFixednetProbB > 0, KStakesFixednetProbB, 0), 
      KStakesEMProb = ifelse(KStakesEMProb > 0, KStakesEMProb, 0), 
      KStakesEMProbnetProbB = ifelse(KStakesEMProbnetProbB > 0, KStakesEMProbnetProbB, 0), 
      KStakesHalf = ifelse(KStakesHalf > 0, KStakesHalf, 0), 
      KStakesHalfnetProbB = ifelse(KStakesHalfnetProbB > 0, KStakesHalfnetProbB, 0), 
      KStakesQuarter = ifelse(KStakesQuarter > 0, KStakesQuarter, 0), 
      KStakesQuarternetProbB = ifelse(KStakesQuarternetProbB > 0, KStakesQuarternetProbB, 0), 
      KStakesAdj = ifelse(KStakesAdj > 0, KStakesAdj, 0), 
      KStakesAdjnetProbB = ifelse(KStakesAdjnetProbB > 0, KStakesAdjnetProbB, 0), 
      KStakesHalfAdj = ifelse(KStakesHalfAdj > 0, KStakesHalfAdj, 0), 
      KStakesHalfAdjnetProbB = ifelse(KStakesHalfAdjnetProbB > 0, KStakesHalfAdjnetProbB, 0), 
      KStakesEMQuarterAdj = ifelse(KStakesEMQuarterAdj > 0, KStakesEMQuarterAdj, 0), 
      KStakesEMQuarterAdjnetProbB = ifelse(KStakesEMQuarterAdjnetProbB > 0, KStakesEMQuarterAdjnetProbB, 0)
    ) %>% na.omit
  }
  
  ## ==================== P&L Comparison ========================================
  ## load KellyPL() to summarise the P&L of various Kelly models.
  #'@ suppressMessages(source('./function/KellyPL.R'))
  
  ## ==================== Return function ========================================
  
  if(all(wt$weight.stakes != 1) | all(wt$weight != 1)) {
    tmp <- list(data = mbase, Kelly1 = KellyPL(K1, adjusted = adjusted), 
                Kelly2 = KellyPL(K2, adjusted = adjusted), 
                Kelly3 = KellyPL(K3, adjusted = adjusted), 
                Kelly4 = KellyPL(K4, adjusted = adjusted), 
                weight.stakes = wt$weight.stakes, weight = wt$weight)
    options(warn = 0)
    return(tmp)
    
  } else {
    tmp <- list(data = mbase, Kelly1 = KellyPL(K1, adjusted = adjusted), 
                weight.stakes = wt$weight.stakes, weight = wt$weight)
    options(warn = 0)
    return(tmp)
  }
}

