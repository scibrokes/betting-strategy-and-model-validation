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
  ## For example : vKelly(dat, type = 'D1', )
  
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
  
  #'@ if(mbase$rEMProbB <= 0) stop('Invalid rEMProbB value, probabilities must be greater than 0 due to odds price cannot be 0.')
  #'@ if(mbase$rEMProbL <= 0) stop('Invalid rEMProbL value, probabilities must be greater than 0 due to odds price cannot be 0.')
  #'@ if(mbase$netProbB <= 0) stop('Invalid netProbB value, probabilities must be greater than 0 due to odds price cannot be 0.')
  #'@ if(mbase$netProbL <= 0) stop('Invalid netProbL value, probabilities must be greater than 0 due to odds price cannot be 0.')
  #'@ if(mbase$HKPrice <= 0) stop('Invalid HKPrice value, probabilities must be greater than 0 due to odds price cannot be 0.')
  #'@ if(mbase$EUPrice <= 1) stop('Invalid EUPrice value, probabilities must be greater than 0 due to odds price cannot be 0.')
  
  wt <- data_frame(No = seq(nrow(mbase)))
  
  if(!is.numeric(adjusted)) stop('Kindly insert a vector of numeric values.')
  
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
  
  if(!c('PL.R', 'RebatesS') %in% names(mbase)) {
    mbase <- mbase[order(mbase$No.x),] %>% mutate(PL.R = PL / Stakes, RebatesS = Stakes * Rebates)
  }
  
  ## --------------------- Convert probabilities -------------------------------- 
  ## weighted parameter estimation
  mbase %<>% mutate(theta = suppressAll(
    ifelse(Result == 'Win', 1, 
    ifelse(Result == 'Half Win', 0.5, 
    ifelse(Result == 'Push'|Result == 'Cancelled', 0, 
    ifelse(Result == 'Half Loss', -0.5, 
    ifelse(Result == 'Loss', -1, NA)))))), 
                    dWin = ifelse(Result == 'Win', 1, 0), 
                    dwhf = ifelse(Result == 'Half Win', 1, 0), 
                    dpus = ifelse(Result == 'Push'|Result == 'Cancelled', 1, 0), 
                    dlhf = ifelse(Result == 'Half Loss', 1, 0), 
                    dlos = ifelse(Result == 'Loss', 1, 0))
  
  #'@ mbase %<>% mutate(
  #'@   theta = as.numeric(plyr::mapvalues(Result, 
  #'@                 c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
  #'@                 c(1, 0.5, 0, 0, -0.5, -1))), 
  #'@   dWin = ifelse(Result == 'Win', 1, 0), 
  #'@   dwhf = ifelse(Result == 'Half Win', 1, 0), 
  #'@   dpus = ifelse(Result == 'Push' | Result == 'Cancelled', 1, 0), 
  #'@   dlhf = ifelse(Result == 'Half Loss', 1, 0), 
  #'@   dlos = ifelse(Result == 'Loss', 1, 0))
    
  if(type == 'flat' | type == 'W1' | type == 'W2' | type == 'WS1' | type == 'WS2' | 
     type == 'W1WS1' | type == 'W1WS2' | type == 'W2WS1' | type == 'W2WS2') {
    
    m <- ddply(mbase, .(Sess), summarise, rRates = mean(Return / Stakes), 
               theta = mean(theta), dWin = mean(dWin), dwhf = mean(dwhf), 
               dpus = mean(dpus), dlhf = mean(dlhf), dlos = mean(dlos)) %>% tbl_df
    
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
      
      mbase$theta <- rep(as.numeric(str_replace_na(lag(as.numeric(m$theta)), 0)), ## theta value will be W1, exp(theta)
                         ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase$dWin <- rep(as.numeric(str_replace_na(lag(as.numeric(m$dWin)), 0)), 
                        ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase$dwhf <- rep(as.numeric(str_replace_na(lag(as.numeric(m$dwhf)), 0)), 
                        ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase$dpus <- rep(as.numeric(str_replace_na(lag(as.numeric(m$dpus)), 0)), 
                        ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase$dlhf <- rep(as.numeric(str_replace_na(lag(as.numeric(m$dlhf)), 0)), 
                        ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      mbase$dlos <- rep(as.numeric(str_replace_na(lag(as.numeric(m$dlos)), 0)), 
                        ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
      
      mbase %<>% 
        mutate(dres = suppressAll(
                 ifelse(Result == 'Win', dWin, ## dres value will be W2
                 ifelse(Result == 'Half Win', dwhf, 
                 ifelse(Result == 'Push'|Result == 'Cancelled', dpus, 
                 ifelse(Result == 'Half Loss', dlhf, 
                 ifelse(Result == 'Loss', dlos, NA)))))))
               #'@ dres = plyr::mapvalues(Result, 
               #'@        c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
               #'@        c(dWin, dwhf, dpus, dpus, dlhf, dlos)))
      
      ## --------------------- weight parameters ---------------------------------
      if(type == 'W1') {
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- exp(mbase$theta)
        wt$weight.stakes <- weight.stakes
        
      } else if(type == 'W2') {
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- exp(mbase$dres)
        wt$weight.stakes <- weight.stakes
        
      } else if(type == 'WS1') {
        
        file.exists('./data/lRProf2A.rds') {
          if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
            lrp <- readRDS('./data/lRProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          stop('Kindly use leagueRiskProf() to measure to save a league risk profile data set.')
        }
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- 1
        wt$weight.stakes <- suppressAll(
          join(mbase, lrp) %>% tbl_df %>% filter(Sess != unique(Sess)[1])) %>% .$annual.1
        rm(lrp)
        
      } else if(type == 'WS2') {
        
        file.exists('./data/lRProf2B.rds') {
          if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
            lrp <- readRDS('./data/lRProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          stop('Kindly use leagueRiskProf() to measure to save a league risk profile data set.')
        }
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- 1
        wt$weight.stakes <- suppressAll(
          join(mbase, lrp) %>% tbl_df %>% filter(Sess != unique(Sess)[1])) %>% .$annual.2
        rm(lrp)
        
      } else if(type == 'W1WS1') {
        
        file.exists('./data/lRProf2A.rds') {
          if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
            lrp <- readRDS('./data/lRProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          stop('Kindly use leagueRiskProf() to measure to save a league risk profile data set.')
        }
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- exp(mbase$theta)
        wt$weight.stakes <- suppressAll(
          join(mbase, lrp) %>% tbl_df %>% filter(Sess != unique(Sess)[1])) %>% .$annual.1
        rm(lrp)
        
      } else if(type == 'W1WS2') {
        
        file.exists('./data/lRProf2B.rds') {
          if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
            lrp <- readRDS('./data/lRProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          stop('Kindly use leagueRiskProf() to measure to save a league risk profile data set.')
        }
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- exp(mbase$theta)
        wt$weight.stakes <- suppressAll(
          join(mbase, lrp) %>% tbl_df %>% filter(Sess != unique(Sess)[1])) %>% .$annual.2
        rm(lrp)
        
      } else if(type == 'W2WS1') {
        
        file.exists('./data/lRProf2A.rds') {
          if(readRDS('./data/lRProf2A.rds') %>% names %>% grepl('annual.1', .) %>% any) {
            lrp <- readRDS('./data/lRProf2A.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          stop('Kindly use leagueRiskProf() to measure to save a league risk profile data set.')
        }
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- exp(mbase$dres)
        wt$weight.stakes <- suppressAll(
          join(mbase, lrp) %>% tbl_df %>% filter(Sess != unique(Sess)[1])) %>% .$annual.1
        rm(lrp)
        
      } else if(type == 'W2WS2') {
        
        file.exists('./data/lRProf2B.rds') {
          if(readRDS('./data/lRProf2B.rds') %>% names %>% grepl('annual.2', .) %>% any) {
            lrp <- readRDS('./data/lRProf2B.rds') %>% 
              mutate(Sess = mapvalues(Sess, from = Sess, to = Sess + 1, warn_missing = FALSE))
          } else {
            stop('Kindly use default preset data set.')
          }
        } else {
          stop('Kindly use leagueRiskProf() to measure to save a league risk profile data set.')
        }
        
        mbase %<>% filter(Sess != unique(Sess)[1])
        wt <- data_frame(No = seq(nrow(mbase)))
        wt$weight <- exp(mbase$dres)
        wt$weight.stakes <- suppressAll(
          join(mbase, lrp) %>% tbl_df %>% filter(Sess != unique(Sess)[1])) %>% .$annual.2
        rm(lrp)
      }
    }
    
  } else {
    ## --------------------- dynamic parameters ---------------------------------
    if(type == 'D1'){
      
      ## measure the current year rRates to know the rEMProbB. 
      m <- ddply(mbase, .(Sess), summarise, rRates = mean(Return / Stakes), 
                 theta = mean(theta), dWin = mean(dWin), dwhf = mean(dwhf), 
                 dpus = mean(dpus), dlhf = mean(dlhf), dlos = mean(dlos)) %>% tbl_df
      
      if(any(!c('rRates', 'rEMProbB', 'rEMProbL', 'netEMEdge') %in% names(mbase))){
        mbase$rRates <- rep(as.numeric(m$rRates + 1), 
                            ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
        mbase %<>% mutate(rEMProbB = rRates * netProbB, rEMProbL = 1 - rEMProbB, 
                          netEMEdge = rEMProbB / netProbB)
      }
      
      dw <- ddply(mbase, .(TimeUS), summarise, 
                  thetac = sum(theta), thetan = length(theta),
                  dWinc = sum(dWin), dWinn = length(dWin), 
                  dwhfc = sum(dwhf), dwhfn = length(dwhf), 
                  dpusc = sum(dpus), dpusn = length(dpus), 
                  dlhfc = sum(dlhf), dlhfn = length(dlhf), 
                  dlosc = sum(dlos), dlosn = length(dlos), 
                  .parallel = parallel) %>% tbl_df %>% 
        mutate(lagTimeUS = c(0, lag(TimeUS)[-1])) %>% .[-1] %>% # drop the TimeUS to 
        mutate(thetas = cumsum(thetac)/cumsum(thetan),          #  avoid duplicate column
               dWins = cumsum(dWinc)/cumsum(dWinn),             #  TimeUS after join()
               dwhfs = cumsum(dwhfc)/cumsum(dwhfn), 
               dpuss = cumsum(dpusc)/cumsum(dpusn), 
               dlhfs = cumsum(dlhfc)/cumsum(dlhfn), 
               dloss = cumsum(dlosc)/cumsum(dlosn))
               
      
      ## Below codes took more than an hour. Therefore use below lag() and 
      ##  join() will be solved. 
      #'@ timeID <- dw$TimeUS
      #'@ mbase$thetas <- rep(0, nrow(mbase))
      #'@ mbase$dWins <- rep(0, nrow(mbase))
      #'@ mbase$dwhfs <- rep(0, nrow(mbase))
      #'@ mbase$dpuss <- rep(0, nrow(mbase))
      #'@ mbase$dlhfs <- rep(0, nrow(mbase))
      #'@ mbase$dloss <- rep(0, nrow(mbase))
      #'@ 
      #'@ for(i in 1:length(timeID)){
      #'@   if(i == 1) {
      #'@     mbase[mbase$TimeUS == timeID[i], ]$thetas <- 0
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dWins <- 0
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dwhfs <- 0
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dpuss <- 0
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dlhfs <- 0
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dloss <- 0
      #'@     
      #'@     mbase[mbase$TimeUS == timeID[i], ]$thetan <- 1
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dWinn <- 1
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dwhfn <- 1
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dpusn <- 1
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dlhfn <- 1
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dlosn <- 1
      #'@     
      #'@   } else {
      #'@     mbase[mbase$TimeUS == timeID[i], ]$thetas <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$thetac)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dWins <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dWinc)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dwhfs <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dwhfc)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dpuss <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dpusc)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dlhfs <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dlhfc)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dloss <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dlosc)
      #'@     
      #'@     mbase[mbase$TimeUS == timeID[i], ]$thetan <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$thetan)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dWinn <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dWinn)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dwhfn <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dwhfn)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dpusn <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dpusn)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dlhfn <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dlhfn)
      #'@     mbase[mbase$TimeUS == timeID[i], ]$dlosn <- suppressAll(dw[dw$TimeUS == timeID[i - 1], ]$dlosn)
      #'@   }
      #'@ }; rm(timeID, i)
      
      mbase %<>% mutate(lagTimeUS = c(0, lag(TimeUS)[-1]))
      mbase %<>% join(dw, by = 'lagTimeUS') %>% tbl_df %>% 
        mutate(dres = suppressAll(
          ifelse(Result == 'Win', dWins, ## dres value will be W2
          ifelse(Result == 'Half Win', dwhfs, 
          ifelse(Result == 'Push'|Result == 'Cancelled', dpuss, 
          ifelse(Result == 'Half Loss', dlhfs, 
          ifelse(Result == 'Loss', dloss, NA)))))))
      
      mbase %<>% filter(Sess != unique(Sess)[1])
      wt <- data_frame(No = seq(nrow(mbase)))
      wt$weight <- exp(mbase$thetas)
      wt$weight.stakes <- weight.stakes ## not yet found the way to weight the staking.
      ## might probably need to apply MCMC to siimulate the staking
      ##   in order to get the weight value for weight.stakes.
      ## Test the result prior to add EM simulation for stakes weight.
      
    } else if(type == 'D2'){
      
      ## measure the current year rRates to know the rEMProbB. 
      m <- ddply(mbase, .(Sess), summarise, rRates = mean(Return / Stakes), 
                 theta = mean(theta), dWin = mean(dWin), dwhf = mean(dwhf), 
                 dpus = mean(dpus), dlhf = mean(dlhf), dlos = mean(dlos)) %>% tbl_df
      
      if(any(!c('rRates', 'rEMProbB', 'rEMProbL', 'netEMEdge') %in% names(mbase))){
        mbase$rRates <- rep(as.numeric(m$rRates + 1), 
                            ddply(mbase, .(Sess), summarise, n = length(Sess))$n)
        mbase %<>% mutate(rEMProbB = rRates * netProbB, rEMProbL = 1 - rEMProbB, 
                          netEMEdge = rEMProbB / netProbB)
      }
      
      dw <- ddply(mbase, .(TimeUS), summarise, 
                  thetac = sum(theta), thetan = length(theta),
                  dWinc = sum(dWin), dWinn = length(dWin), 
                  dwhfc = sum(dwhf), dwhfn = length(dwhf), 
                  dpusc = sum(dpus), dpusn = length(dpus), 
                  dlhfc = sum(dlhf), dlhfn = length(dlhf), 
                  dlosc = sum(dlos), dlosn = length(dlos), 
                  .parallel = parallel) %>% tbl_df %>% 
        mutate(lagTimeUS = c(0, lag(TimeUS)[-1])) %>% .[-1] %>% # drop the TimeUS to 
        mutate(thetas = cumsum(thetac)/cumsum(thetan),          #  avoid duplicate column
               dWins = cumsum(dWinc)/cumsum(dWinn),             #  TimeUS after join()
               dwhfs = cumsum(dwhfc)/cumsum(dwhfn), 
               dpuss = cumsum(dpusc)/cumsum(dpusn), 
               dlhfs = cumsum(dlhfc)/cumsum(dlhfn), 
               dloss = cumsum(dlosc)/cumsum(dlosn))
      
      mbase %<>% mutate(lagTimeUS = c(0, lag(TimeUS)[-1]))
      mbase %<>% join(dw, by = 'lagTimeUS') %>% tbl_df %>% 
        mutate(dres = suppressAll(
          ifelse(Result == 'Win', dWins, ## dres value will be W2
          ifelse(Result == 'Half Win', dwhfs, 
          ifelse(Result == 'Push'|Result == 'Cancelled', dpuss, 
          ifelse(Result == 'Half Loss', dlhfs, 
          ifelse(Result == 'Loss', dloss, NA)))))))
      
      mbase %<>% filter(Sess != unique(Sess)[1])
      wt <- data_frame(No = seq(nrow(mbase)))
      wt$weight <- exp(mbase$dres)
      wt$weight.stakes <- weight.stakes
      
    } else if(type == 'D1WS1'){
      
    } else if(type == 'D1WS2'){
      
    } else if(type == 'D2WS1'){
      
    } else if(type == 'D2WS2'){
      
    } else if(type == 'W1DWS1'){
      
    } else if(type == 'W1DWS2'){
      
    } else if(type == 'W2DWS1'){
      
    } else if(type == 'W2DWS2'){
      
    } else if(type == 'D1DWS1'){
      
    } else if(type == 'D1DWS2'){
      
    } else if(type == 'D1DWS1'){
      
    } else if(type == 'D1DWS2'){
      
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

