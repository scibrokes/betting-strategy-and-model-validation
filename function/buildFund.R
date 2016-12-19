buildFund <- function(mbase, initial = NULL, parallel = as.logical(FALSE), 
                      by = 'daily', by.league = as.logical(FALSE), adjusted = 1) {
  ## The function similar with KellyPL() and compareKelly() but only convert existing 
  ##   staking data into quantmod xts format and fund size growth.
  
  ## --------------------- Load Packages --------------------------------------
  options(warn = -1)
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('stringr'))
  suppressMessages(library('doParallel'))
  suppressMessages(library('quantmod'))
  
  ## --------------------- Data Manipulation ----------------------------------
  if(!c('DateUS', 'TimeUS') %in% names(mbase)) {
    mbase <- mbase %>% mutate(
      TimeUS = ymd_hms(format(DateUK, tz = 'EST', usetz = TRUE, 
                              format = '%Y-%m-%d %H:%M:%S')), 
      DateUS = as.Date(TimeUS))
  }
  
  ## observation based for dynamic simulation use.
  mbase <- mbase[order(mbase$TimeUS, mbase$No.x, decreasing = FALSE),]
  #'@ mbase %<>% mutate(obs = seq(1, nrow(.)))
  adjusted <- adjusted
  
  dateUSID <- sort(unique(mbase$DateUS)) %>% ymd
  timeUSID <- sort(unique(mbase$TimeUS)) %>% ymd_hms
  
  Fund <- mbase[c('DateUS', 'TimeUS', 'League', 'Stakes', 'Return', 'PL')] %>% 
    mutate(Stakes = currency(Stakes), Return = currency(Return), PL = currency(PL), 
           PL.R = percent(ifelse(is.nan(PL / Stakes), 0, PL / Stakes)))
  
  ## ---------------- Quantmod Candle Stick Format ----------------------
  ## Set initial fund size from the staking and profit & loss, below are 2 criteria...
  ##  1) fund size must be enough to place a bet, therefore max stakes + $1.
  ##  2) fund size cannot <= 0 to simulate whole process. Therefore need to set 
  ##  max loss + $1.
  if(is.null(initial)) {
    initial <- max(max(Fund$Stakes), abs(min(cumsum(Fund$PL)))) + 1
  } else if(is.numeric(initial)) {
    initial <- initial
  } else {
    stop('Kindly insert a numeric value for parameter `initial`.')
  }
  
  KPL <- tbl_df(cumsum(Fund$PL) + initial)
  KPL2 <- KPL %>% mutate(value = lag(value))
  KPL2[1, 1] <- initial
  
  namesBR <- paste0('Ordinary', c('.Open', '.High', '.Low', '.Close', '.Volume', 
                                  '.Adjusted'))
  
  BR <- matrix(nc = length(namesBR), nr = nrow(Fund), 
               dimnames = list(NULL, namesBR)) %>% tbl_df %>% 
    mutate(.id = seq(nrow(Fund)), TimeUS = Fund$TimeUS, DateUS = Fund$DateUS, 
           League = Fund$League)
  
  BR[grep('.Close', names(BR), value = TRUE)] <- KPL
  BR[grep('.Open', names(BR), value = TRUE)] <- KPL2
  BR[grep('.High', names(BR), value = TRUE)] <- 
    apply(BR[grep('.Close|.Open', names(BR), value = TRUE)], 1, max, na.rm = TRUE)
  BR[grep('.Low', names(BR), value = TRUE)] <- 
    apply(BR[grep('.Close|.Open', names(BR), value = TRUE)], 1, min, na.rm = TRUE)
  BR[grep('.Volume', names(BR), value = TRUE)] <- Fund['Stakes']
  BR[grep('.Adjusted', names(BR), value = TRUE)] <- adjusted
  
  rm(KPL, KPL2, Fund)
  
  ## --------------------- Data Manipulation -----------------------------------
  
  if(by == 'daily') {
    if(by.league == TRUE) {
      BR <- ddply(BR, .(DateUS, League), summarise, 
                  Ordinary.Open = head(Ordinary.Open, 1), 
                  Ordinary.High = max(Ordinary.High), 
                  Ordinary.Low = max(Ordinary.Low), 
                  Ordinary.Close = tail(Ordinary.Close, 1), 
                  Ordinary.Volume = sum(Ordinary.Volume), 
                  Ordinary.Adjusted = mean(Ordinary.Adjusted)) %>% tbl_df
      BR %<>% llply(split(., .$League), function(x) xts(x[-c(1:2)], x$DateUS))
      
    } else {
      BR <- ddply(BR, .(DateUS), summarise, 
                  Ordinary.Open = head(Ordinary.Open, 1), 
                  Ordinary.High = max(Ordinary.High), 
                  Ordinary.Low = max(Ordinary.Low), 
                  Ordinary.Close = tail(Ordinary.Close, 1), 
                  Ordinary.Volume = sum(Ordinary.Volume), 
                  Ordinary.Adjusted = mean(Ordinary.Adjusted)) %>% tbl_df
      BR <- xts(BR[-1], BR$DateUS)
    }
    
  } else if(by == 'time') {
    if(by.league == TRUE) {
      BR <- ddply(BR, .(TimeUS, League), summarise, 
                  Ordinary.Open = head(Ordinary.Open, 1), 
                  Ordinary.High = max(Ordinary.High), 
                  Ordinary.Low = max(Ordinary.Low), 
                  Ordinary.Close = tail(Ordinary.Close, 1), 
                  Ordinary.Volume = sum(Ordinary.Volume), 
                  Ordinary.Adjusted = mean(Ordinary.Adjusted)) %>% tbl_df
      BR %<>% llply(split(., .$League), function(x) xts(x[-c(1:2)], x$TimeUS))
      
    } else {
      BR <- ddply(BR, .(TimeUS), summarise, 
                  Ordinary.Open = head(Ordinary.Open, 1), 
                  Ordinary.High = max(Ordinary.High), 
                  Ordinary.Low = max(Ordinary.Low), 
                  Ordinary.Close = tail(Ordinary.Close, 1), 
                  Ordinary.Volume = sum(Ordinary.Volume), 
                  Ordinary.Adjusted = mean(Ordinary.Adjusted)) %>% tbl_df
      BR <- xts(BR[-1], BR$TimeUS)
    }
    
  } else {
    stop('Kindly select by = "daily" or by = "time".')
  }
  
  options(warn = 0)
  return(BR)
}

