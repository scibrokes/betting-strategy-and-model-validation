profHandling <- function(type = 'weight', action = 'load', rm.files = as.logical(TRUE), 
                         overwrite = as.logical(FALSE)) {
  ## type = 'weight' or type = 'weight.stakes'.
  ## action = 'load' or action = 'save'.
  ## You can choose either load or save all risk profiles of Kelly models. It will 
  ##   auto calculate and save/load if the file(s) is(are) not exist.
  ## rm.files = TRUE or rm.files = FALSE. You can choose to remove all files from 
  ##   environment once saved. (applicable only for action = "save")
  ## overwrite = FALSE or overwrite = TRUE, you can choose if you want to overwrite 
  ##   if there has any existing files in directory. (applicable only for action = "save")
  
  ## ================== Load Function ================================
  ## Simulate and save the league profiling : all, annual, daily, time or dynamic.
  ## You just need to run once to complete whole weighted parameters measurement, 
  ##   and then you can just execute the === Kelly Models === section since it will 
  ##   auto read saved profiles which will be more efficient compare to use vKelly() 
  ##   to calculate the weighted parameters and get the result from start to end 
  ##   will take few minutes time to complete.
  source('./function/leagueRiskProf.R', local = TRUE)
  
  ## ================== Load Data ================================
  ## Load saved dataset to save the loading time.
  ##   directly load the dataset from running chunk `read-data-summary-table` 
  ##   and also chunk `scrap-data`. The spboData for filtering leagues and 
  ##   matches scores purpose.
  if('shinyData.RData' %in% dir('./KellyApps')) {
    load('./KellyApps/shinyData.RData')
  } else {
    stop('Kindly refer to chunk named `load-data` inside file `Betting Strategy and Ⓜodel Validation - Part I.Rmd`.')
  }
  
  ## ================== Data Validation ================================
  overwrite = as.logical(overwrite)
  rm.files = as.logical(rm.files)
  
  if(action == 'save') {
    ## ================== Save Profiles ================================
    
    if(type == 'weight') {
      ## -------------------- result based weight parameters --------------------
      
      if('wProf1A.rds' %in% dir('./data')) {
        if('all.theta' %in% names(readRDS('./data/wProf1A.rds'))) {
          ## --------------- wProf1A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf1A <- leagueRiskProf(dat, type = 'weight', weight.type = 'all')
            wProf1A <- saveRDS(wProf1A, file = './data/wProf1A.rds')
            if(rm.files == TRUE) rm(wProf1A)
          }
        }
      } else {
        wProf1A <- leagueRiskProf(dat, type = 'weight', weight.type = 'all')
        wProf1A <- saveRDS(wProf1A, file = './data/wProf1A.rds')
        if(rm.files == TRUE) rm(wProf1A)
      }
      
      if('wProf1B.rds' %in% dir('./data')) {
        if(all(c('hdpC', 'all.hdpW') %in% names(readRDS('./data/wProf1B.rds')))) {
          ## --------------- wProf1B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf1B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'all')
            wProf1B <- saveRDS(wProf1B, file = './data/wProf1B.rds')
            if(rm.files == TRUE) rm(wProf1B)
          }
        }
      } else {
        wProf1B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'all')
        wProf1B <- saveRDS(wProf1B, file = './data/wProf1B.rds')
        if(rm.files == TRUE) rm(wProf1B)
      }
      
      if('wProf2A.rds' %in% dir('./data')) {
        if(all(c('Sess', 'annual.theta') %in% names(readRDS('./data/wProf2A.rds')))) {
          ## --------------- wProf2A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf2A <- leagueRiskProf(dat, type = 'weight', weight.type = 'annual')
            wProf2A <- saveRDS(wProf2A, file = './data/wProf2A.rds')
            if(rm.files == TRUE) rm(wProf2A)
          }
        }
      } else {
        wProf2A <- leagueRiskProf(dat, type = 'weight', weight.type = 'annual')
        wProf2A <- saveRDS(wProf2A, file = './data/wProf2A.rds')
        if(rm.files == TRUE) rm(wProf2A)
      }
      
      if('wProf2B.rds' %in% dir('./data')) {
        if(all(c('Sess', 'hdpC', 'annual.hdpW') %in% names(readRDS('./data/wProf2B.rds')))) {
          ## --------------- wProf2B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf2B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'annual')
            wProf2B <- saveRDS(wProf2B, file = './data/wProf2B.rds')
            if(rm.files == TRUE) rm(wProf2B)
          }
        }
      } else {
        wProf2B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'annual')
        wProf2B <- saveRDS(wProf2B, file = './data/wProf2B.rds')
        if(rm.files == TRUE) rm(wProf2B)
      }
      
      if('wProf3A.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'daily.theta') %in% names(readRDS('./data/wProf3A.rds')))) {
          ## --------------- wProf3A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf3A <- leagueRiskProf(dat, type = 'weight', weight.type = 'daily')
            wProf3A <- saveRDS(wProf3A, file = './data/wProf3A.rds')
            if(rm.files == TRUE) rm(wProf3A)
          }
        }
      } else {
        wProf3A <- leagueRiskProf(dat, type = 'weight', weight.type = 'daily')
        wProf3A <- saveRDS(wProf3A, file = './data/wProf3A.rds')
        if(rm.files == TRUE) rm(wProf3A)
      }
      
      if('wProf3B.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'hdpC', 'daily.hdpW') %in% names(readRDS('./data/wProf3B.rds')))) {
          ## --------------- wProf3B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf3B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'daily')
            wProf3B <- saveRDS(wProf3B, file = './data/wProf3B.rds')
            if(rm.files == TRUE) rm(wProf3B)
          }
        }
      } else {
        wProf3B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'daily')
        wProf3B <- saveRDS(wProf3B, file = './data/wProf3B.rds')
        if(rm.files == TRUE) rm(wProf3B)
      }
      
      if('wProf4A.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'time.theta') %in% names(readRDS('./data/wProf4A.rds')))) {
          ## --------------- wProf4A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf4A <- leagueRiskProf(dat, type = 'weight', weight.type = 'time')
            wProf4A <- saveRDS(wProf4A, file = './data/wProf4A.rds')
            if(rm.files == TRUE) rm(wProf4A)
          }
        }
      } else {
        wProf4A <- leagueRiskProf(dat, type = 'weight', weight.type = 'time')
        wProf4A <- saveRDS(wProf4A, file = './data/wProf4A.rds')
        if(rm.files == TRUE) rm(wProf4A)
      }
      
      if('wProf4B.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'hdpC', 'time.hdpW') %in% names(readRDS('./data/wProf4B.rds')))) {
          ## --------------- wProf4B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf4B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'time')
            wProf4B <- saveRDS(wProf4B, file = './data/wProf4B.rds')
            if(rm.files == TRUE) rm(wProf4B)
          }
        }
      } else {
        wProf4B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'time')
        wProf4B <- saveRDS(wProf4B, file = './data/wProf4B.rds')
        if(rm.files == TRUE) rm(wProf4B)
      }
      
      if('wProf5A.rds' %in% dir('./data')) {
        if(all(c('obs', 'dym.theta') %in% names(readRDS('./data/wProf5A.rds')))) {
          ## --------------- wProf5A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf5A <- leagueRiskProf(dat, type = 'weight', weight.type = 'dynamic')
            wProf5A <- saveRDS(wProf5A, file = './data/wProf5A.rds')
            if(rm.files == TRUE) rm(wProf5A)
          }
        }
      } else {
        wProf5A <- leagueRiskProf(dat, type = 'weight', weight.type = 'dynamic')
        wProf5A <- saveRDS(wProf5A, file = './data/wProf5A.rds')
        if(rm.files == TRUE) rm(wProf5A)
      }
      
      if('wProf5B.rds' %in% dir('./data')) {
        if(all(c('obs', 'hdpC', 'dym.hdpW') %in% names(readRDS('./data/wProf5B.rds')))) {
          ## --------------- wProf5B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            wProf5B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'dynamic')
            wProf5B <- saveRDS(wProf5B, file = './data/wProf5B.rds')
            if(rm.files == TRUE) rm(wProf5B)
          }
        }
      } else {
        wProf5B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'dynamic')
        wProf5B <- saveRDS(wProf5B, file = './data/wProf5B.rds')
        if(rm.files == TRUE) rm(wProf5B)
      }
      
    } else if(type == 'weight.stakes') {
      ## --------------- league weight parameters --------------------
      
      if('lRProf1A.rds' %in% dir('./data')) {
        if(all(c('League', 'all.lg') %in% names(readRDS('./data/lRProf1A.rds')))){
          ## --------------- lRProf1A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf1A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'all')
            lRProf1A <- saveRDS(lRProf1A, file = './data/lRProf1A.rds')
            if(rm.files == TRUE) rm(lRProf1A)
          }
        }
      } else {
        lRProf1A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'all')
        lRProf1A <- saveRDS(lRProf1A, file = './data/lRProf1A.rds')
        if(rm.files == TRUE) rm(lRProf1A)
      }
      
      if('lRProf1B.rds' %in% dir('./data')) {
        if(all(c('League', 'hdpC', 'all.lhdp') %in% names(readRDS('./data/lRProf1B.rds')))){
          ## --------------- lRProf1B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf1B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'all')
            lRProf1B <- saveRDS(lRProf1B, file = './data/lRProf1B.rds')
            if(rm.files == TRUE) rm(lRProf1B)
          }
        }
      } else {
        lRProf1B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'all')
        lRProf1B <- saveRDS(lRProf1B, file = './data/lRProf1B.rds')
        if(rm.files == TRUE) rm(lRProf1B)
      }
      
      if('lRProf2A.rds' %in% dir('./data')) {
        if(all(c('Sess', 'League', 'annual.lg') %in% names(readRDS('./data/lRProf2A.rds')))){
          ## --------------- lRProf2A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf2A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'annual')
            lRProf2A <- saveRDS(lRProf2A, file = './data/lRProf2A.rds')
            if(rm.files == TRUE) rm(lRProf2A)
          }
        }
      } else {
        lRProf2A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'annual')
        lRProf2A <- saveRDS(lRProf2A, file = './data/lRProf2A.rds')
        if(rm.files == TRUE) rm(lRProf2A)
      }
      
      if('lRProf2B.rds' %in% dir('./data')) {
        if(all(c('Sess', 'League', 'hdpC', 'annual.lhdp') %in% names(readRDS('./data/lRProf2B.rds')))){
          ## --------------- lRProf2B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf2B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual')
            lRProf2B <- saveRDS(lRProf2B, file = './data/lRProf2B.rds')
            if(rm.files == TRUE) rm(lRProf2B)
          }
        }
      } else {
        lRProf2B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual')
        lRProf2B <- saveRDS(lRProf2B, file = './data/lRProf2B.rds')
        if(rm.files == TRUE) rm(lRProf2B)
      }
      
      if('lRProf3A.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'League', 'daily.lg') %in% names(readRDS('./data/lRProf3A.rds')))){
          ## --------------- lRProf3A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf3A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'daily')
            lRProf3A <- saveRDS(lRProf3A, file = './data/lRProf3A.rds')
            if(rm.files == TRUE) rm(lRProf3A)
          }
        }
      } else {
        lRProf3A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'daily')
        lRProf3A <- saveRDS(lRProf3A, file = './data/lRProf3A.rds')
        if(rm.files == TRUE) rm(lRProf3A)
      }
      
      if('lRProf3B.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'League', 'hdpC', 'daily.lhdp') %in% names(readRDS('./data/lRProf3B.rds')))){
          ## --------------- lRProf3B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf3B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'daily')
            lRProf3B <- saveRDS(lRProf3B, file = './data/lRProf3B.rds')
            if(rm.files == TRUE) rm(lRProf3B)
          }
        }
      } else {
        lRProf3B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'daily')
        lRProf3B <- saveRDS(lRProf3B, file = './data/lRProf3B.rds')
        if(rm.files == TRUE) rm(lRProf3B)
      }
      
      if('lRProf4A.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'League', 'time.lg') %in% names(readRDS('./data/lRProf4A.rds')))){
          ## --------------- lRProf4A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf4A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'time')
            lRProf4A <- saveRDS(lRProf4A, file = './data/lRProf4A.rds')
            if(rm.files == TRUE) rm(lRProf4A)
          }
        }
      } else {
        lRProf4A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'time')
        lRProf4A <- saveRDS(lRProf4A, file = './data/lRProf4A.rds')
        if(rm.files == TRUE) rm(lRProf4A)
      }
      
      if('lRProf4B.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'League', 'hdpC', 'time.lhdp') %in% names(readRDS('./data/lRProf4B.rds')))){
          ## --------------- lRProf4B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf4B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'time')
            lRProf4B <- saveRDS(lRProf4B, file = './data/lRProf4B.rds')
            if(rm.files == TRUE) rm(lRProf4B)
          }
        }
      } else {
        lRProf4B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'time')
        lRProf4B <- saveRDS(lRProf4B, file = './data/lRProf4B.rds')
        if(rm.files == TRUE) rm(lRProf4B)
      }
      
      if('lRProf5A.rds' %in% dir('./data')) {
        if(all(c('obs', 'League', 'dym.lg') %in% names(readRDS('./data/lRProf5A.rds')))){
          ## --------------- lRProf5A --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf5A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'dynamic')
            lRProf5A <- saveRDS(lRProf5A, file = './data/lRProf5A.rds')
            if(rm.files == TRUE) rm(lRProf5A)
          }
        }
      } else {
        lRProf5A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'dynamic')
        lRProf5A <- saveRDS(lRProf5A, file = './data/lRProf5A.rds')
        if(rm.files == TRUE) rm(lRProf5A)
      }
      
      if('lRProf5B.rds' %in% dir('./data')) {
        if(all(c('obs', 'League', 'hdpC', 'dym.lhdp') %in% names(readRDS('./data/lRProf5B.rds')))){
          ## --------------- lRProf4B --------------------
          if(overwrite == FALSE) {
            stop('The file has exist in the directory.')
          } else {
            lRProf5B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'dynamic')
            lRProf5B <- saveRDS(lRProf5B, file = './data/lRProf5B.rds')
            if(rm.files == TRUE) rm(lRProf5B)
          }
        }
      } else {
        lRProf5B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'dynamic')
        lRProf5B <- saveRDS(lRProf5B, file = './data/lRProf5B.rds')
        if(rm.files == TRUE) rm(lRProf5B)
      }
      
    } else {
      stop('Kindly choose type = "weight" or type = "weight.stakes".')
    }
    
  } else if(action == 'load') {
    ## ================== Load Profiles ================================
    
    if(type == 'weight') {
      ## -------------------- result based weight parameters --------------------
      
      if('wProf1A.rds' %in% dir('./data')) {
        if('all.theta' %in% names(readRDS('./data/wProf1A.rds'))) {
          ## --------------- wProf1A --------------------
          wProf1A <- readRDS('./data/wProf1A.rds')
        }
      } else {
        wProf1A <- leagueRiskProf(dat, type = 'weight', weight.type = 'all')
        wProf1A <- saveRDS(wProf1A, file = './data/wProf1A.rds')
      }
      
      if('wProf1B.rds' %in% dir('./data')) {
        if(all(c('hdpC', 'all.hdpW') %in% names(readRDS('./data/wProf1B.rds')))) {
          ## --------------- wProf1B --------------------
          wProf1B <- readRDS('./data/wProf1B.rds')
        }
      } else {
        wProf1B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'all')
        wProf1B <- saveRDS(wProf1B, file = './data/wProf1B.rds')
      }
      
      if('wProf2A.rds' %in% dir('./data')) {
        if(all(c('Sess', 'annual.theta') %in% names(readRDS('./data/wProf2A.rds')))) {
          ## --------------- wProf2A --------------------
          wProf2A <- readRDS('./data/wProf2A.rds')
        }
      } else {
        wProf2A <- leagueRiskProf(dat, type = 'weight', weight.type = 'annual')
        wProf2A <- saveRDS(wProf2A, file = './data/wProf2A.rds')
      }
      
      if('wProf2B.rds' %in% dir('./data')) {
        if(all(c('Sess', 'hdpC', 'annual.hdpW') %in% names(readRDS('./data/wProf2B.rds')))) {
          ## --------------- wProf2B --------------------
          wProf2B <- readRDS('./data/wProf2B.rds')
        }
      } else {
        wProf2B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'annual')
        wProf2B <- saveRDS(wProf2B, file = './data/wProf2B.rds')
      }
      
      if('wProf3A.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'daily.theta') %in% names(readRDS('./data/wProf3A.rds')))) {
          ## --------------- wProf3A --------------------
          wProf3A <- readRDS('./data/wProf3A.rds')
        }
      } else {
        wProf3A <- leagueRiskProf(dat, type = 'weight', weight.type = 'daily')
        wProf3A <- saveRDS(wProf3A, file = './data/wProf3A.rds')
      }
      
      if('wProf3B.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'hdpC', 'daily.hdpW') %in% names(readRDS('./data/wProf3B.rds')))) {
          ## --------------- wProf3B --------------------
          wProf3B <- readRDS('./data/wProf3B.rds')
        }
      } else {
        wProf3B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'daily')
        wProf3B <- saveRDS(wProf3B, file = './data/wProf3B.rds')
      }
      
      if('wProf4A.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'time.theta') %in% names(readRDS('./data/wProf4A.rds')))) {
          ## --------------- wProf4A --------------------
          wProf4A <- readRDS('./data/wProf4A.rds')
        }
      } else {
        wProf4A <- leagueRiskProf(dat, type = 'weight', weight.type = 'time')
        wProf4A <- saveRDS(wProf4A, file = './data/wProf4A.rds')
      }
      
      if('wProf4B.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'hdpC', 'time.hdpW') %in% names(readRDS('./data/wProf4B.rds')))) {
          ## --------------- wProf4B --------------------
          wProf4B <- readRDS('./data/wProf4B.rds')
        }
      } else {
        wProf4B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'time')
        wProf4B <- saveRDS(wProf4B, file = './data/wProf4B.rds')
      }
      
      if('wProf5A.rds' %in% dir('./data')) {
        if(all(c('obs', 'dym.theta') %in% names(readRDS('./data/wProf5A.rds')))) {
          ## --------------- wProf5A --------------------
          wProf5A <- readRDS('./data/wProf5A.rds')
        }
      } else {
        wProf5A <- leagueRiskProf(dat, type = 'weight', weight.type = 'dynamic')
        wProf5A <- saveRDS(wProf5A, file = './data/wProf5A.rds')
      }
      
      if('wProf5B.rds' %in% dir('./data')) {
        if(all(c('obs', 'hdpC', 'dym.hdpW') %in% names(readRDS('./data/wProf5B.rds')))) {
          ## --------------- wProf5B --------------------
          wProf5B <- readRDS('./data/wProf5B.rds')
        }
      } else {
        wProf5B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'dynamic')
        wProf5B <- saveRDS(wProf5B, file = './data/wProf5B.rds')
      }
      
      wProf = list(wProf1A = wProf1A, wProf1B = wProf1B, 
                   wProf2A = wProf2A, wProf2B = wProf2B, 
                   wProf3A = wProf3A, wProf3B = wProf3B, 
                   wProf4A = wProf4A, wProf4B = wProf4B, 
                   wProf5A = wProf5A, wProf5B = wProf5B)
      
      return(list(data = dat, prof = wProf))
      
    } else if(type == 'weight.stakes') {
      ## --------------- league weight parameters --------------------
      
      if('lRProf1A.rds' %in% dir('./data')) {
        if(all(c('League', 'all.lg') %in% names(readRDS('./data/lRProf1A.rds')))){
          ## --------------- lRProf1A --------------------
          lRProf1A <- readRDS('./data/lRProf1A.rds')
        }
      } else {
        lRProf1A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'all')
        lRProf1A <- saveRDS(lRProf1A, file = './data/lRProf1A.rds')
      }
      
      if('lRProf1B.rds' %in% dir('./data')) {
        if(all(c('League', 'hdpC', 'all.lhdp') %in% names(readRDS('./data/lRProf1B.rds')))){
          ## --------------- lRProf1B --------------------
          lRProf1B <- readRDS('./data/lRProf1B.rds')
        }
      } else {
        lRProf1B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'all')
        lRProf1B <- saveRDS(lRProf1B, file = './data/lRProf1B.rds')
      }
      
      if('lRProf2A.rds' %in% dir('./data')) {
        if(all(c('Sess', 'League', 'annual.lg') %in% names(readRDS('./data/lRProf2A.rds')))){
          ## --------------- lRProf2A --------------------
          lRProf2A <- readRDS('./data/lRProf2A.rds')
        }
      } else {
        lRProf2A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'annual')
        lRProf2A <- saveRDS(lRProf2A, file = './data/lRProf2A.rds')
      }
      
      if('lRProf2B.rds' %in% dir('./data')) {
        if(all(c('Sess', 'League', 'hdpC', 'annual.lhdp') %in% names(readRDS('./data/lRProf2B.rds')))){
          ## --------------- lRProf2B --------------------
          lRProf2B <- readRDS('./data/lRProf2B.rds')
        }
      } else {
        lRProf2B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual')
        lRProf2B <- saveRDS(lRProf2B, file = './data/lRProf2B.rds')
      }
      
      if('lRProf3A.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'League', 'daily.lg') %in% names(readRDS('./data/lRProf3A.rds')))){
          ## --------------- lRProf3A --------------------
          lRProf3A <- readRDS('./data/lRProf3A.rds')
        }
      } else {
        lRProf3A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'daily')
        lRProf3A <- saveRDS(lRProf3A, file = './data/lRProf3A.rds')
      }
      
      if('lRProf3B.rds' %in% dir('./data')) {
        if(all(c('DateUS', 'League', 'hdpC', 'daily.lhdp') %in% names(readRDS('./data/lRProf3B.rds')))){
          ## --------------- lRProf3B --------------------
          lRProf3B <- readRDS('./data/lRProf3B.rds')
        }
      } else {
        lRProf3B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'daily')
        lRProf3B <- saveRDS(lRProf3B, file = './data/lRProf3B.rds')
      }
      
      if('lRProf4A.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'League', 'time.lg') %in% names(readRDS('./data/lRProf4A.rds')))){
          ## --------------- lRProf4A --------------------
          lRProf4A <- readRDS('./data/lRProf4A.rds')
        }
      } else {
        lRProf4A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'time')
        lRProf4A <- saveRDS(lRProf4A, file = './data/lRProf4A.rds')
      }
      
      if('lRProf4B.rds' %in% dir('./data')) {
        if(all(c('TimeUS', 'League', 'hdpC', 'time.lhdp') %in% names(readRDS('./data/lRProf4B.rds')))){
          ## --------------- lRProf4B --------------------
          lRProf4B <- readRDS('./data/lRProf4B.rds')
        }
      } else {
        lRProf4B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'time')
        lRProf4B <- saveRDS(lRProf4B, file = './data/lRProf4B.rds')
      }
      
      if('lRProf5A.rds' %in% dir('./data')) {
        if(all(c('obs', 'League', 'dym.lg') %in% names(readRDS('./data/lRProf5A.rds')))){
          ## --------------- lRProf5A --------------------
          lRProf5A <- readRDS('./data/lRProf5A.rds')
        }
      } else {
        lRProf5A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'dynamic')
        lRProf5A <- saveRDS(lRProf5A, file = './data/lRProf5A.rds')
      }
      
      if('lRProf5B.rds' %in% dir('./data')) {
        if(all(c('obs', 'League', 'hdpC', 'dym.lhdp') %in% names(readRDS('./data/lRProf5B.rds')))){
          ## --------------- lRProf4B --------------------
          lRProf5B <- readRDS('./data/lRProf5B.rds')
        }
      } else {
        lRProf5B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'dynamic')
        lRProf5B <- saveRDS(lRProf5B, file = './data/lRProf5B.rds')
      }
      
      lRProf = list(lRProf1A = lRProf1A, lRProf1B = lRProf1B, 
                    lRProf2A = lRProf2A, lRProf2B = lRProf2B, 
                    lRProf3A = lRProf3A, lRProf3B = lRProf3B, 
                    lRProf4A = lRProf4A, lRProf4B = lRProf4B, 
                    lRProf5A = lRProf5A, lRProf5B = lRProf5B)
      
      return(list(data = dat, prof = lRProf))
      
    } else {
      stop('Kindly choose type = "weight" or type = "weight.stakes".')
    }
    
  } else {
    stop('Kindly select action = "save" or action = "load".')
  }
}