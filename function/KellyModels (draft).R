## ================== Load Data ================================
## Simulate and save Kelly models for easily loading
#'@ source('./function/libs.R')
#'@ load('./KellyApps/shinyData.RData', envir = .GlobalEnv)

## ================== League Profile ===========================
## Simulate and save the league profiling : all, annual, daily, time or dynamic.
## You just need to run once to complete whole weighted parameters measurement, 
##   and then you can just execute the === Kelly Models === section since it will auto read saved 
##   profiles which will be more efficient compare to use vKelly() to calculate the weighted parameters 
##   and get the result from start to end will take few minutes time to complete.
#'@ source('./function/leagueRiskProf.R')

## --------------- league weight parameters --------------------
#'@ lRProf1A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'all')
#'@ lRProf1B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'all')
#'@ lRProf2A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'annual')
#'@ lRProf2B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual')
#'@ lRProf3A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'daily')
#'@ lRProf3B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'daily')
#'@ lRProf4A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'time')
#'@ lRProf4B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'time')
#'@ lRProf5A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'dynamic')
#'@ lRProf5B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'dynamic')

## In order to save the loading time but also the space in folder data, here I omit the 
##   join data and combination section but directly fit it into the function vKelly() and vKelly2(). 
##   For example : vKelly(dat, type = 'WS1'). The function will auto detect of the profile data set 
##   is exist or not and auto adjust and fit the values into the data set prior to calculate.
## 
## fit league weight parameters into dataset.
#'@ lRProf1A %<>% join(dat, .) %>% tbl_df
#'@ lRProf1B %<>% join(dat, .) %>% tbl_df
#'@ lRProf2A %<>% join(dat, .) %>% tbl_df
#'@ lRProf2B %<>% join(dat, .) %>% tbl_df
#'@ lRProf3A %<>% join(dat, .) %>% tbl_df
#'@ lRProf3B %<>% join(dat, .) %>% tbl_df
#'@ lRProf4A %<>% join(dat, .) %>% tbl_df
#'@ lRProf4B %<>% join(dat, .) %>% tbl_df
#'@ lRProf5A %<>% join(dat, .) %>% tbl_df
#'@ lRProf5B %<>% join(dat, .) %>% tbl_df

## combination of the weight values.
#'@ lRiskProf <- join_all(list(lRProf1A, lRProf1B, lRProf2A, lRProf2B, lRProf3A, lRProf3B, 
#'@                            lRProf4A, lRProf4B, lRProf5A, lRProf5B)) %>% tbl_df %>% 
#'@   select(No.x, Sess, DateUS, TimeUS, ...) # need to modify
#'@ names(lRiskProf) <- c('No.x', 'Sess', 'DateUS', 'TimeUS', 'all.1', 'all.2', 'ann.1', 'ann.2', 'day.1', 'day.2', 'time.1', 
#'@                       'time.2', 'dym.1', 'dym.2')

## we can also compare the sd of the weight value and doing further adjustment.

#'@ saveRDS(lRProf1A, file = './data/lRProf1A.rds')
#'@ saveRDS(lRProf1B, file = './data/lRProf1B.rds')
#'@ saveRDS(lRProf2A, file = './data/lRProf2A.rds')
#'@ saveRDS(lRProf2B, file = './data/lRProf2B.rds')
#'@ saveRDS(lRProf3A, file = './data/lRProf3A.rds')
#'@ saveRDS(lRProf3B, file = './data/lRProf3B.rds')
#'@ saveRDS(lRProf4A, file = './data/lRProf4A.rds')
#'@ saveRDS(lRProf4B, file = './data/lRProf4B.rds')
#'@ saveRDS(lRProf5A, file = './data/lRProf5A.rds')
#'@ saveRDS(lRProf5B, file = './data/lRProf5B.rds')

## -------------------- result based weight parameters --------------------
#'@ wProf1A <- leagueRiskProf(dat, type = 'weight', weight.type = 'all')
#'@ wProf1B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'all')
#'@ wProf2A <- leagueRiskProf(dat, type = 'weight', weight.type = 'annual')
#'@ wProf2B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'annual')
#'@ wProf3A <- leagueRiskProf(dat, type = 'weight', weight.type = 'daily')
#'@ wProf3B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'daily')
#'@ wProf4A <- leagueRiskProf(dat, type = 'weight', weight.type = 'time')
#'@ wProf4B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'time')
#'@ wProf5A <- leagueRiskProf(dat, type = 'weight', weight.type = 'dynamic')
#'@ wProf5B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'dynamic')

#'@ saveRDS(wProf1A, file = './data/wProf1A.rds')
#'@ saveRDS(wProf1B, file = './data/wProf1B.rds')
#'@ saveRDS(wProf2A, file = './data/wProf2A.rds')
#'@ saveRDS(wProf2B, file = './data/wProf2B.rds')
#'@ saveRDS(wProf3A, file = './data/wProf3A.rds')
#'@ saveRDS(wProf3B, file = './data/wProf3B.rds')
#'@ saveRDS(wProf4A, file = './data/wProf4A.rds')
#'@ saveRDS(wProf4B, file = './data/wProf4B.rds')
#'@ saveRDS(wProf5A, file = './data/wProf5A.rds')
#'@ saveRDS(wProf5B, file = './data/wProf5B.rds')

## I wrote a function for league risk profiles and weight profiles handling. You can just easily 
##   load below function to save. load and auto calculate + save/load (if the files are not exist).

source('./function/profHandling.R')
wProf = profHandling()
lRProf = profHandling(type = 'weight.stakes')
rm(profHandling)

## Combination and analyse the weight values. You can analyse the sd and mean (if any). It is not 
##  related to the research but you can analyse as further knowledge and know the effects of weight 
##  and know the optimal weight values by mean. You can know evey single 0.01 diff among the weight 
##  parameters affect the result of the returns. I might leave it as the research on section 2 inside 
##  `https://github.com/scibrokes/kelly-criterion` due to analyse on 110 Kelly models waste the resource. 
##  Will get the best fit from the Bank Roll growth and variance (risk) and analyse it.
#'@ join(wProf) %>% tbl_df
#'@ join(lRProf) %>% tbl_df

## ================== Load Data ================================
## When we saved data by execute above codes, then we just load the data from here onwards. 
## Simulate and save Kelly models for easily loading
source('./function/libs.R')
load('./KellyApps/shinyData.RData')

## ================== Kelly Models =============================
## --------------- 01. K1 ------------------------
## Reserved Stakes Kelly Models.
if(exists('K1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1))) {
    saveRDS(K1 , file = './data/K1.rds'); rm(K1)
  } else {
    stop('Kindly apply K1 <- vKelly(dat) to measure the Kelly model.')
  }
} else {
  K1 <- vKelly(dat)
  saveRDS(K1 , file = './data/K1.rds'); rm(K1)
}

## --------------- 02. K2 ------------------------
## Reserved EM Probabilities Kelly Models.
if(exists('K2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2))) {
    saveRDS(K2 , file = './data/K2.rds'); rm(K2)
  } else {
    stop('Kindly apply K2 <- vKelly2(dat) to measure the Kelly model.')
  }
} else {
  K2 <- vKelly2(dat)
  saveRDS(K2, file = './data/K2.rds'); rm(K2)
}

## --------------- 03. K1W1 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual theta constant value.
##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
if(exists('K1W1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W1))) {
    saveRDS(K1W1 , file = './data/K1W1.rds'); rm(K1W1)
  } else {
    stop('Kindly apply K1W1 <- vKelly(dat, type = "W1") to measure the Kelly model.')
  }
} else {
  K1W1 <- vKelly(dat, type = 'W1')
  saveRDS(K1W1, file = './data/K1W1.rds'); rm(K1W1)
}

## --------------- 04. K1W2 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual weighted hdp parameter.
##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
if(exists('K1W2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W2))) {
    saveRDS(K1W2, file = './data/K1W2.rds'); rm(K1W2)
  } else {
    stop('Kindly apply K1W2 <- vKelly(dat, type = "W2") to measure the Kelly model.')
  }
} else {  
  K1W2 <- vKelly(dat, type = 'W2')
  saveRDS(K1W2, file = './data/K1W2.rds'); rm(K1W2)
}

## --------------- 05. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value.
##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
if(exists('K2W1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W1))) {
    saveRDS(K2W1, file = './data/K2W1.rds'); rm(K2W1)
  } else {
    stop('Kindly apply K2W1 <- vKelly2(dat, type = "W1") to measure the Kelly model.')
  }
} else {
  K2W1 <- vKelly2(dat, type = 'W1')
  saveRDS(K2W1, file = './data/K2W1.rds'); rm(K2W1)
}

## --------------- 06. K2W2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual weighted hdp parameter.
##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
if(exists('K2W2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W2))) {
    saveRDS(K2W2, file = './data/K2W2.rds'); rm(K2W2)
  } else {
    stop('Kindly apply K2W2 <- vKelly2(dat, type = "W2") to measure the Kelly model.')
  }
} else {
  K2W2 <- vKelly2(dat, type = 'W2')
  saveRDS(K2W2, file = './data/K2W2.rds'); rm(K2W2)
}

## --------------- 07. K1W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
if(exists('K1W1WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W1WS1))) {
    saveRDS(K1W1WS1, file = './data/K1W1WS1.rds'); rm(K1W1WS1)
  } else {
    stop('Kindly apply K1W1WS1 <- vKelly(dat, type = "W1WS1") to measure the Kelly model.')
  }
} else {
  K1W1WS1 <- vKelly(dat, type = 'W1WS1')
  saveRDS(K1W1WS1, file = './data/K1W1WS1.rds'); rm(K1W1WS1)
}

## --------------- 08. K1W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
if(exists('K1W1WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W1WS2))) {
    saveRDS(K1W1WS2, file = './data/K1W1WS2.rds'); rm(K1W1WS2)
  } else {
    stop('Kindly apply K1W1WS2 <- vKelly(dat, type = "W1WS2") to measure the Kelly model.')
  }
} else {
  K1W1WS2 <- vKelly(dat, type = 'W1WS2')
  saveRDS(K1W1WS2, file = './data/K1W1WS2.rds'); rm(K1W1WS2)
}

## --------------- 09. K1W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
if(exists('K1W2WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W2WS1))) {
    saveRDS(K1W2WS1, file = './data/K1W2WS1.rds'); rm(K1W2WS1)
  } else {
    stop('Kindly apply K1W2WS1 <- vKelly(dat, type = "W2WS1") to measure the Kelly model.')
  }
} else {
  K1W2WS1 <- vKelly(dat, type = 'W2WS1')
  saveRDS(K1W2WS1, file = './data/K1W2WS1.rds'); rm(K1W2WS1)
}

## --------------- 10. K1W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
if(exists('K1W2WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W2WS2))) {
    saveRDS(K1W2WS2, file = './data/K1W2WS2.rds'); rm(K1W2WS2)
  } else {
    stop('Kindly apply K1W2WS2 <- vKelly(dat, type = "W2WS2") to measure the Kelly model.')
  }
} else {
  K1W2WS2 <- vKelly(dat, type = 'W2WS2')
  saveRDS(K1W2WS2, file = './data/K1W2WS2.rds'); rm(K1W2WS2)
}

## --------------- 11. K2W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
if(exists('K2W1WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W1WS1))) {
    saveRDS(K2W1WS1, file = './data/K2W1WS1.rds'); rm(K2W1WS1)
  } else {
    stop('Kindly apply K2W1WS1 <- vKelly2(dat, type = "W1WS1") to measure the Kelly model.')
  }
} else {
  K2W1WS1 <- vKelly2(dat, type = 'W1WS1')
  saveRDS(K2W1WS1, file = './data/K2W1WS1.rds'); rm(K2W1WS1)
}

## --------------- 12. K2W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
if(exists('K2W1WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W1WS2))) {
    saveRDS(K2W1WS2, file = './data/K2W1WS2.rds'); rm(K2W1WS2)
  } else {
    stop('Kindly apply K2W1WS2 <- vKelly2(dat, type = "W1WS2") to measure the Kelly model.')
  }
} else {
  K2W1WS2 <- vKelly2(dat, type = 'W1WS2')
  saveRDS(K2W1WS2, file = './data/K2W1WS2.rds'); rm(K2W1WS2)
}

## --------------- 13. K2W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
if(exists('K2W2WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W2WS1))) {
    saveRDS(K2W2WS1, file = './data/K2W2WS1.rds'); rm(K2W2WS1)
  } else {
    stop('Kindly apply K2W2WS1 <- vKelly2(dat, type = "W2WS1") to measure the Kelly model.')
  }
} else {
  K2W2WS1 <- vKelly2(dat, type = 'W2WS1')
  saveRDS(K2W2WS1, file = './data/K2W2WS1.rds'); rm(K2W2WS1)
}

## --------------- 14. K2W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
if(exists('K2W2WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W2WS2))) {
    saveRDS(K2W2WS2, file = './data/K2W2WS2.rds'); rm(K2W2WS2)
  } else {
    stop('Kindly apply K2W2WS2 <- vKelly2(dat, type = "W2WS2") to measure the Kelly model.')
  }
} else {
  K2W2WS2 <- vKelly2(dat, type = 'W2WS2')
  saveRDS(K2W2WS2, file = './data/K2W2WS2.rds'); rm(K2W2WS2)
}

## --------------- Default Dynamic ------------------------
## --------------- 15. K1D1 ------------------------
if(exists('K1D1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1))) {
    saveRDS(K1D1, file = './data/K1D1.rds'); rm(K1D1)
  } else {
    stop('Kindly apply K1D1 <- vKelly(dat, type = "D1") to measure the Kelly model.')
  }
} else {
  K1D1 <- vKelly(dat, type = 'D1')
  saveRDS(K1D1, file = './data/K1D1.rds'); rm(K1D1)
}

## --------------- 16. K1D2 ------------------------
if(exists('K1D2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2))) {
    saveRDS(K1D2, file = './data/K1D2.rds'); rm(K1D2)
  } else {
    stop('Kindly apply K1D2 <- vKelly(dat, type = "D2") to measure the Kelly model.')
  }
} else {
  K1D2 <- vKelly(dat, type = 'D2')
  saveRDS(K1D2, file = './data/K1D2.rds'); rm(K1D2)
}

## --------------- 17. K2D1 ------------------------
if(exists('K2D1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1))) {
    saveRDS(K2D1, file = './data/K2D1.rds'); rm(K2D1)
  } else {
    stop('Kindly apply K2D1 <- vKelly2(dat, type = "D1") to measure the Kelly model.')
  }
} else {
  K2D1 <- vKelly2(dat, type = 'D1')
  saveRDS(K2D1, file = './data/K2D1.rds'); rm(K2D1)
}

## --------------- 18. K2D2 ------------------------
if(exists('K2D2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2))) {
    saveRDS(K2D2, file = './data/K2D2.rds'); rm(K2D2)
  } else {
    stop('Kindly apply K2D2 <- vKelly2(dat, type = "D2") to measure the Kelly model.')
  }
} else {
  K2D2 <- vKelly2(dat, type = 'D2')
  saveRDS(K2D2, file = './data/K2D2.rds'); rm(K2D2)
}

## --------------- 19. K1DWS1 ------------------------
if(exists('K1DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1DWS1))) {
    saveRDS(K1DWS1, file = './data/K1DWS1.rds'); rm(K1DWS1)
  } else {
    stop('Kindly apply K1DWS1 <- vKelly(dat, type = "DWS1") to measure the Kelly model.')
  }
} else {
  K1DWS1 <- vKelly(dat, type = 'DWS1')
  saveRDS(K1DWS1, file = './data/K1DWS1.rds'); rm(K1DWS1)
}

## --------------- 20. K1DWS2 ------------------------
if(exists('K1DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1DWS2))) {
    saveRDS(K1DWS2, file = './data/K1DWS2.rds'); rm(K1DWS2)
  } else {
    stop('Kindly apply K1DWS2 <- vKelly(dat, type = "DWS2") to measure the Kelly model.')
  }
} else {
  K1DWS2 <- vKelly(dat, type = 'DWS2')
  saveRDS(K1DWS2, file = './data/K1DWS2.rds'); rm(K1DWS2)
}

## --------------- 21. K2DWS1 ------------------------
if(exists('K2DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2DWS1))) {
    saveRDS(K2DWS1, file = './data/K2DWS1.rds'); rm(K2DWS1)
  } else {
    stop('Kindly apply K2DWS1 <- vKelly2(dat, type = "DWS1") to measure the Kelly model.')
  }
} else {
  K2DWS1 <- vKelly2(dat, type = 'DWS1')
  saveRDS(K2DWS1, file = './data/K2DWS1.rds'); rm(K2DWS1)
}

## --------------- 22. K2DWS2 ------------------------
if(exists('K2DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2DWS2))) {
    saveRDS(K2DWS2, file = './data/K2DWS2.rds'); rm(K2DWS2)
  } else {
    stop('Kindly apply K2DWS2 <- vKelly2(dat, type = "DWS2") to measure the Kelly model.')
  }
} else {
  K2DWS2 <- vKelly2(dat, type = 'DWS2')
  saveRDS(K2DWS2, file = './data/K2DWS2.rds'); rm(K2DWS2)
}

## --------------- 23. K1D1WS1 ------------------------
if(exists('K1D1WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1WS1))) {
    saveRDS(K1D1WS1, file = './data/K1D1WS1.rds'); rm(K1D1WS1)
  } else {
    stop('Kindly apply K1D1WS1 <- vKelly(dat, type = "D1WS1") to measure the Kelly model.')
  }
} else {
  K1D1WS1 <- vKelly(dat, type = 'D1WS1')
  saveRDS(K1D1WS1, file = './data/K1D1WS1.rds'); rm(K1D1WS1)
}

## --------------- 24. K1D1WS2 ------------------------
if(exists('K1D1WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1WS2))) {
    saveRDS(K1D1WS2, file = './data/K1D1WS2.rds'); rm(K1D1WS2)
  } else {
    stop('Kindly apply K1D1WS2 <- vKelly(dat, type = "D1WS2") to measure the Kelly model.')
  }
} else {
  K1D1WS2 <- vKelly(dat, type = 'D1WS2')
  saveRDS(K1D1WS2, file = './data/K1D1WS2.rds'); rm(K1D1WS2)
}

## --------------- 25. K1D2WS1 ------------------------
if(exists('K1D2WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2WS1))) {
    saveRDS(K1D2WS1, file = './data/K1D2WS1.rds'); rm(K1D2WS1)
  } else {
    stop('Kindly apply K1D2WS1 <- vKelly(dat, type = "D2WS1") to measure the Kelly model.')
  }
} else {
  K1D2WS1 <- vKelly(dat, type = 'D2WS1')
  saveRDS(K1D2WS1, file = './data/K1D2WS1.rds'); rm(K1D2WS1)
}

## --------------- 26. K1D2WS2 ------------------------
if(exists('K1D2WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2WS2))) {
    saveRDS(K1D2WS2, file = './data/K1D2WS2.rds'); rm(K1D2WS2)
  } else {
    stop('Kindly apply K1D2WS2 <- vKelly(dat, type = "D2WS2") to measure the Kelly model.')
  }
} else {
  K1D2WS2 <- vKelly(dat, type = 'D2WS2')
  saveRDS(K1D2WS2, file = './data/K1D2WS2.rds'); rm(K1D2WS2)
}

## --------------- 27. K2D1WS1 ------------------------
if(exists('K2D1WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1WS1))) {
    saveRDS(K2D1WS1, file = './data/K2D1WS1.rds'); rm(K2D1WS1)
  } else {
    stop('Kindly apply K2D1WS1 <- vKelly2(dat, type = "D1WS1") to measure the Kelly model.')
  }
} else {
  K2D1WS1 <- vKelly2(dat, type = 'D1WS1')
  saveRDS(K2D1WS1, file = './data/K2D1WS1.rds'); rm(K2D1WS1)
}

## --------------- 28. K2D1WS2 ------------------------
if(exists('K2D1WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1WS2))) {
    saveRDS(K2D1WS2, file = './data/K2D1WS2.rds'); rm(K2D1WS2)
  } else {
    stop('Kindly apply K2D1WS2 <- vKelly2(dat, type = "D1WS2") to measure the Kelly model.')
  }
} else {
  K2D1WS2 <- vKelly2(dat, type = 'D1WS2')
  saveRDS(K2D1WS2, file = './data/K2D1WS2.rds'); rm(K2D1WS2)
}

## --------------- 29. K2D2WS1 ------------------------
if(exists('K2D2WS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2WS1))) {
    saveRDS(K2D2WS1, file = './data/K2D2WS1.rds'); rm(K2D2WS1)
  } else {
    stop('Kindly apply K2D2WS1 <- vKelly2(dat, type = "D2WS1") to measure the Kelly model.')
  }
} else {
  K2D2WS1 <- vKelly2(dat, type = 'D2WS1')
  saveRDS(K2D2WS1, file = './data/K2D2WS1.rds'); rm(K2D2WS1)
}

## --------------- 30. K2D2WS2 ------------------------
if(exists('K2D2WS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2WS2))) {
    saveRDS(K2D2WS2, file = './data/K2D2WS2.rds'); rm(K2D2WS2)
  } else {
    stop('Kindly apply K2D2WS2 <- vKelly2(dat, type = "D2WS2") to measure the Kelly model.')
  }
} else {
  K2D2WS2 <- vKelly2(dat, type = 'D2WS2')
  saveRDS(K2D2WS2, file = './data/K2D2WS2.rds'); rm(K2D2WS2)
}

## --------------- 31. K1W1DWS1 ------------------------
if(exists('K1W1DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W1DWS1))) {
    saveRDS(K1W1DWS1, file = './data/K1W1DWS1.rds'); rm(K1W1DWS1)
  } else {
    stop('Kindly apply K1W1DWS1 <- vKelly(dat, type = "W1DWS1") to measure the Kelly model.')
  }
} else {
  K1W1DWS1 <- vKelly(dat, type = 'W1DWS1')
  saveRDS(K1W1DWS1, file = './data/K1W1DWS1.rds'); rm(K1W1DWS1)
}

## --------------- 32. K1W1DWS2 ------------------------
if(exists('K1W1DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W1DWS2))) {
    saveRDS(K1W1DWS2, file = './data/K1W1DWS2.rds'); rm(K1W1DWS2)
  } else {
    stop('Kindly apply K1W1DWS2 <- vKelly(dat, type = "W1WS2") to measure the Kelly model.')
  }
} else {
  K1W1DWS2 <- vKelly(dat, type = 'W1WS2')
  saveRDS(K1W1DWS2, file = './data/K1W1DWS2.rds'); rm(K1W1DWS2)
}

## --------------- 33. K1W2DWS1 ------------------------
if(exists('K1W2DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W2DWS1))) {
    saveRDS(K1W2DWS1, file = './data/K1W2DWS1.rds'); rm(K1W2DWS1)
  } else {
    stop('Kindly apply K1W2DWS1 <- vKelly(dat, type = "W2DWS1") to measure the Kelly model.')
  }
} else {
  K1W2DWS1 <- vKelly(dat, type = 'W2DWS1')
  saveRDS(K1W2DWS1, file = './data/K1W2DWS1.rds'); rm(K1W2DWS1)
}

## --------------- 34. K1W2DWS2 ------------------------
if(exists('K1W2DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1W2DWS2))) {
    saveRDS(K1W2DWS2, file = './data/K1W2DWS2.rds'); rm(K1W2DWS2)
  } else {
    stop('Kindly apply K1W2DWS2 <- vKelly(dat, type = "W2DWS2") to measure the Kelly model.')
  }
} else {
  K1W2DWS2 <- vKelly(dat, type = 'W2DWS2')
  saveRDS(K1W2DWS2, file = './data/K1W2DWS2.rds'); rm(K1W2DWS2)
}

## --------------- 35. K2W1DWS1 ------------------------
if(exists('K2W1DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W1DWS1))) {
    saveRDS(K2W1DWS1, file = './data/K2W1DWS1.rds'); rm(K2W1DWS1)
  } else {
    stop('Kindly apply K2W1DWS1 <- vKelly2(dat, type = "W1DWS1") to measure the Kelly model.')
  }
} else {
  K2W1DWS1 <- vKelly2(dat, type = 'W1DWS1')
  saveRDS(K2W1DWS1, file = './data/K2W1DWS1.rds'); rm(K2W1DWS1)
}

## --------------- 36. K2W1DWS2 ------------------------
if(exists('K2W1DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W1DWS2))) {
    saveRDS(K2W1DWS2, file = './data/K2W1DWS2.rds'); rm(K2W1DWS2)
  } else {
    stop('Kindly apply K2W1DWS2 <- vKelly2(dat, type = "W1DWS2") to measure the Kelly model.')
  }
} else {
  K2W1DWS2 <- vKelly2(dat, type = 'W1DWS2')
  saveRDS(K2W1DWS2, file = './data/K2W1DWS2.rds'); rm(K2W1DWS2)
}

## --------------- 37. K2W2DWS1 ------------------------
if(exists('K2W2DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W2DWS1))) {
    saveRDS(K2W2DWS1, file = './data/K2W2DWS1.rds'); rm(K2W2DWS1)
  } else {
    stop('Kindly apply K2W2DWS1 <- vKelly2(dat, type = "W2DWS1") to measure the Kelly model.')
  }
} else {
  K2W2DWS1 <- vKelly2(dat, type = 'W2DWS1')
  saveRDS(K2W2DWS1, file = './data/K2W2DWS1.rds'); rm(K2W2DWS1)
}

## --------------- 38. K2W2DWS2 ------------------------
if(exists('K2W2DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2W2DWS2))) {
    saveRDS(K2W2DWS2, file = './data/K2W2DWS2.rds'); rm(K2W2DWS2)
  } else {
    stop('Kindly apply K2W2DWS2 <- vKelly2(dat, type = "W2DWS2") to measure the Kelly model.')
  }
} else {
  K2W2DWS2 <- vKelly2(dat, type = 'W2DWS2')
  saveRDS(K2W2DWS2, file = './data/K2W2DWS2.rds'); rm(K2W2DWS2)
}

## --------------- 39. K1D1DWS1 ------------------------
if(exists('K1D1DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1))) {
    saveRDS(K1D1DWS1, file = './data/K1D1DWS1.rds'); rm(K1D1DWS1)
  } else {
    stop('Kindly apply K1D1DWS1 <- vKelly(dat, type = "D1DWS1") to measure the Kelly model.')
  }
} else {
  K1D1DWS1 <- vKelly(dat, type = 'D1DWS1')
  saveRDS(K1D1DWS1, file = './data/K1D1DWS1.rds'); rm(K1D1DWS1)
}

## --------------- 40. K1D1DWS2 ------------------------
if(exists('K1D1DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2))) {
    saveRDS(K1D1DWS2, file = './data/K1D1DWS2.rds'); rm(K1D1DWS2)
  } else {
    stop('Kindly apply K1D1DWS2 <- vKelly(dat, type = "D1DWS2") to measure the Kelly model.')
  }
} else {
  K1D1DWS2 <- vKelly(dat, type = 'D1DWS2')
  saveRDS(K1D1DWS2, file = './data/K1D1DWS2.rds'); rm(K1D1DWS2)
}

## --------------- 41. K1D2DWS1 ------------------------
if(exists('K1D2DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1))) {
    saveRDS(K1D2DWS1, file = './data/K1D2DWS1.rds'); rm(K1D2DWS1)
  } else {
    stop('Kindly apply K1D2DWS1 <- vKelly(dat, type = "D2DWS1") to measure the Kelly model.')
  }
} else {
  K1D2DWS1 <- vKelly(dat, type = 'D2DWS1')
  saveRDS(K1D2DWS1, file = './data/K1D2DWS1.rds'); rm(K1D2DWS1)
}

## --------------- 42. K1D2DWS2 ------------------------
if(exists('K1D2DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2))) {
    saveRDS(K1D2DWS2, file = './data/K1D2DWS2.rds'); rm(K1D2DWS2)
  } else {
    stop('Kindly apply K1D2DWS2 <- vKelly(dat, type = "D2DWS2") to measure the Kelly model.')
  }
} else {
  K1D2DWS2 <- vKelly(dat, type = 'D2DWS2')
  saveRDS(K1D2DWS2, file = './data/K1D2DWS2.rds'); rm(K1D2DWS2)
}

## --------------- 43. K2D1DWS1 ------------------------
if(exists('K2D1DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1))) {
    saveRDS(K2D1DWS1, file = './data/K2D1DWS1.rds'); rm(K2D1DWS1)
  } else {
    stop('Kindly apply K2D1DWS1 <- vKelly2(dat, type = "D1DWS1") to measure the Kelly model.')
  }
} else {
  K2D1DWS1 <- vKelly2(dat, type = 'D1DWS1')
  saveRDS(K2D1DWS1, file = './data/K2D1DWS1.rds'); rm(K2D1DWS1)
}

## --------------- 44. K2D1DWS2 ------------------------
if(exists('K2D1DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2))) {
    saveRDS(K2D1DWS2, file = './data/K2D1DWS2.rds'); rm(K2D1DWS2)
  } else {
    stop('Kindly apply K2D1DWS2 <- vKelly2(dat, type = "D1DWS2") to measure the Kelly model.')
  }
} else {
  K2D1DWS2 <- vKelly2(dat, type = 'D1DWS2')
  saveRDS(K2D1DWS2, file = './data/K2D1DWS2.rds'); rm(K2D1DWS2)
}

## --------------- 45. K2D2DWS1 ------------------------
if(exists('K2D2DWS1')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1))) {
    saveRDS(K2D2DWS1, file = './data/K2D2DWS1.rds'); rm(K2D2DWS1)
  } else {
    stop('Kindly apply K2D2DWS1 <- vKelly2(dat, type = "D2DWS1") to measure the Kelly model.')
  }
} else {
  K2D2DWS1 <- vKelly2(dat, type = 'D2DWS1')
  saveRDS(K2D2DWS1, file = './data/K2D2DWS1.rds'); rm(K2D2DWS1)
}

## --------------- 46. K2D2DWS2 ------------------------
if(exists('K2D2DWS2')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2))) {
    saveRDS(K2D2DWS2, file = './data/K2D2DWS2.rds'); rm(K2D2DWS2)
  } else {
    stop('Kindly apply K2D2DWS2 <- vKelly2(dat, type = "D2DWS2") to measure the Kelly model.')
  }
} else {
  K2D2DWS2 <- vKelly2(dat, type = 'D2DWS2')
  saveRDS(K2D2DWS2, file = './data/K2D2DWS2.rds'); rm(K2D2DWS2)
}

## --------------- Dynamic Mixed : Kick-Off Time + Kick-Off Time ------------------------
## --------------- 47. K1D1DWS1TT ------------------------
if(exists('K1D1DWS1TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1TT))) {
    saveRDS(K1D1DWS1TT, file = './data/K1D1DWS1TT.rds'); rm(K1D1DWS1TT)
  } else {
    stop('Kindly apply K1D1DWS1TT <- vKelly(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D1DWS1TT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K1D1DWS1TT, file = './data/K1D1DWS1TT.rds'); rm(K1D1DWS1TT)
}

## --------------- 48. K1D1DWS2TT ------------------------
if(exists('K1D1DWS2TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2TT))) {
    saveRDS(K1D1DWS2TT, file = './data/K1D1DWS2TT.rds'); rm(K1D1DWS2TT)
  } else {
    stop('Kindly apply K1D1DWS2TT <- vKelly(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D1DWS2TT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K1D1DWS2TT, file = './data/K1D1DWS2TT.rds'); rm(K1D1DWS2TT)
}

## --------------- 49. K1D2DWS1TT ------------------------
if(exists('K1D2DWS1TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1TT))) {
    saveRDS(K1D2DWS1TT, file = './data/K1D2DWS1TT.rds'); rm(K1D2DWS1TT)
  } else {
    stop('Kindly apply K1D2DWS1TT <- vKelly(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D2DWS1TT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K1D2DWS1TT, file = './data/K1D2DWS1TT.rds'); rm(K1D2DWS1TT)
}

## --------------- 50. K1D2DWS2TT ------------------------
if(exists('K1D2DWS2TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2TT))) {
    saveRDS(K1D2DWS2TT, file = './data/K1D2DWS2TT.rds'); rm(K1D2DWS2TT)
  } else {
    stop('Kindly apply K1D2DWS2TT <- vKelly(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D2DWS2TT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K1D2DWS2TT, file = './data/K1D2DWS2TT.rds'); rm(K1D2DWS2TT)
}

## --------------- 51. K2D1DWS1TT ------------------------
if(exists('K2D1DWS1TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1TT))) {
    saveRDS(K2D1DWS1TT, file = './data/K2D1DWS1TT.rds'); rm(K2D1DWS1TT)
  } else {
    stop('Kindly apply K2D1DWS1TT <- vKelly2(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D1DWS1TT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K2D1DWS1TT, file = './data/K2D1DWS1TT.rds'); rm(K2D1DWS1TT)
}

## --------------- 52. K2D1DWS2TT ------------------------
if(exists('K2D1DWS2TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2TT))) {
    saveRDS(K2D1DWS2TT, file = './data/K2D1DWS2TT.rds'); rm(K2D1DWS2TT)
  } else {
    stop('Kindly apply K2D1DWS2TT <- vKelly2(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D1DWS2TT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K2D1DWS2TT, file = './data/K2D1DWS2TT.rds'); rm(K2D1DWS2TT)
}

## --------------- 53. K2D2DWS1TT ------------------------
if(exists('K2D2DWS1TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1TT))) {
    saveRDS(K2D2DWS1TT, file = './data/K2D2DWS1TT.rds'); rm(K2D2DWS1TT)
  } else {
    stop('Kindly apply K2D2DWS1TT <- vKelly2(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D2DWS1TT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K2D2DWS1TT, file = './data/K2D2DWS1TT.rds'); rm(K2D2DWS1TT)
}

## --------------- 54. K2D2DWS2TT ------------------------
if(exists('K2D2DWS2TT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2TT))) {
    saveRDS(K2D2DWS2TT, file = './data/K2D2DWS2TT.rds'); rm(K2D2DWS2TT)
  } else {
    stop('Kindly apply K2D2DWS2TT <- vKelly2(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D2DWS2TT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
  saveRDS(K2D2DWS2TT, file = './data/K2D2DWS2TT.rds'); rm(K2D2DWS2TT)
}

## --------------- Dynamic Mixed : Kick-Off Time + Dynamic ------------------------
## --------------- 55. K1D1DWS1TO ------------------------
if(exists('K1D1DWS1TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1TO))) {
    saveRDS(K1D1DWS1TO, file = './data/K1D1DWS1TO.rds'); rm(K1D1DWS1TO)
  } else {
    stop('Kindly apply K1D1DWS1TO <- vKelly(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D1DWS1TO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K1D1DWS1TO, file = './data/K1D1DWS1TO.rds'); rm(K1D1DWS1TO)
}

## --------------- 56. K1D1DWS2TO ------------------------
if(exists('K1D1DWS2TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2TO))) {
    saveRDS(K1D1DWS2TO, file = './data/K1D1DWS2TO.rds'); rm(K1D1DWS2TO)
  } else {
    stop('Kindly apply K1D1DWS2TO <- vKelly(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D1DWS2TO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K1D1DWS2TO, file = './data/K1D1DWS2TO.rds'); rm(K1D1DWS2TO)
}

## --------------- 57. K1D2DWS1TO ------------------------
if(exists('K1D2DWS1TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1TO))) {
    saveRDS(K1D2DWS1TO, file = './data/K1D2DWS1TO.rds'); rm(K1D2DWS1TO)
  } else {
    stop('Kindly apply K1D2DWS1TO <- vKelly(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D2DWS1TO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K1D2DWS1TO, file = './data/K1D2DWS1TO.rds'); rm(K1D2DWS1TO)
}

## --------------- 58. K1D2DWS2TO ------------------------
if(exists('K1D2DWS2TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2TO))) {
    saveRDS(K1D2DWS2TO, file = './data/K1D2DWS2TO.rds'); rm(K1D2DWS2TO)
  } else {
    stop('Kindly apply K1D2DWS2TO <- vKelly(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D2DWS2TO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K1D2DWS2TO, file = './data/K1D2DWS2TO.rds'); rm(K1D2DWS2TO)
}

## --------------- 59. K2D1DWS1TO ------------------------
if(exists('K2D1DWS1TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1TO))) {
    saveRDS(K2D1DWS1TO, file = './data/K2D1DWS1TO.rds'); rm(K2D1DWS1TO)
  } else {
    stop('Kindly apply K2D1DWS1TO <- vKelly2(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D1DWS1TO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K2D1DWS1TO, file = './data/K2D1DWS1TO.rds'); rm(K2D1DWS1TO)
}

## --------------- 60. K2D1DWS2TO ------------------------
if(exists('K2D1DWS2TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2TO))) {
    saveRDS(K2D1DWS2TO, file = './data/K2D1DWS2TO.rds'); rm(K2D1DWS2TO)
  } else {
    stop('Kindly apply K2D1DWS2TO <- vKelly2(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D1DWS2TO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K2D1DWS2TO, file = './data/K2D1DWS2TO.rds'); rm(K2D1DWS2TO)
}

## --------------- 61. K2D2DWS1TO ------------------------
if(exists('K2D2DWS1TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1TO))) {
    saveRDS(K2D2DWS1TO, file = './data/K2D2DWS1TO.rds'); rm(K2D2DWS1TO)
  } else {
    stop('Kindly apply K2D2DWS1TO <- vKelly2(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D2DWS1TO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K2D2DWS1TO, file = './data/K2D2DWS1TO.rds'); rm(K2D2DWS1TO)
}

## --------------- 62. K2D2DWS2TO ------------------------
if(exists('K2D2DWS2TO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2TO))) {
    saveRDS(K2D2DWS2TO, file = './data/K2D2DWS2TO.rds'); rm(K2D2DWS2TO)
  } else {
    stop('Kindly apply K2D2DWS2TO <- vKelly2(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D2DWS2TO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
  saveRDS(K2D2DWS2TO, file = './data/K2D2DWS2TO.rds'); rm(K2D2DWS2TO)
}

## --------------- Dynamic Mixed : Daily + Daily ------------------------
## --------------- 63. K1D1DWS1DD ------------------------
if(exists('K1D1DWS1DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1DD))) {
    saveRDS(K1D1DWS1DD, file = './data/K1D1DWS1DD.rds'); rm(K1D1DWS1DD)
  } else {
    stop('Kindly apply K1D1DWS1DD <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D1DWS1DD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K1D1DWS1DD, file = './data/K1D1DWS1DD.rds'); rm(K1D1DWS1DD)
}

## --------------- 64. K1D1DWS2DD ------------------------
if(exists('K1D1DWS2DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2DD))) {
    saveRDS(K1D1DWS2DD, file = './data/K1D1DWS2DD.rds'); rm(K1D1DWS2DD)
  } else {
    stop('Kindly apply K1D1DWS2DD <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D1DWS2DD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K1D1DWS2DD, file = './data/K1D1DWS2DD.rds'); rm(K1D1DWS2DD)
}

## --------------- 65. K1D2DWS1DD ------------------------
if(exists('K1D2DWS1DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1DD))) {
    saveRDS(K1D2DWS1DD, file = './data/K1D2DWS1DD.rds'); rm(K1D2DWS1DD)
  } else {
    stop('Kindly apply K1D2DWS1DD <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D2DWS1DD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K1D2DWS1DD, file = './data/K1D2DWS1DD.rds'); rm(K1D2DWS1DD)
}

## --------------- 66. K1D2DWS2DD ------------------------
if(exists('K1D2DWS2DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2DD))) {
    saveRDS(K1D2DWS2DD, file = './data/K1D2DWS2DD.rds'); rm(K1D2DWS2DD)
  } else {
    stop('Kindly apply K1D2DWS2DD <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D2DWS2DD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K1D2DWS2DD, file = './data/K1D2DWS2DD.rds'); rm(K1D2DWS2DD)
}

## --------------- 67. K2D1DWS1DD ------------------------
if(exists('K2D1DWS1DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1DD))) {
    saveRDS(K2D1DWS1DD, file = './data/K2D1DWS1DD.rds'); rm(K2D1DWS1DD)
  } else {
    stop('Kindly apply K2D1DWS1DD <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D1DWS1DD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K2D1DWS1DD, file = './data/K2D1DWS1DD.rds'); rm(K2D1DWS1DD)
}

## --------------- 68. K2D1DWS2DD ------------------------
if(exists('K2D1DWS2DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2DD))) {
    saveRDS(K2D1DWS2DD, file = './data/K2D1DWS2DD.rds'); rm(K2D1DWS2DD)
  } else {
    stop('Kindly apply K2D1DWS2DD <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D1DWS2DD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K2D1DWS2DD, file = './data/K2D1DWS2DD.rds'); rm(K2D1DWS2DD)
}

## --------------- 69. K2D2DWS1DD ------------------------
if(exists('K2D2DWS1DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1DD))) {
    saveRDS(K2D2DWS1DD, file = './data/K2D2DWS1DD.rds'); rm(K2D2DWS1DD)
  } else {
    stop('Kindly apply K2D2DWS1DD <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D2DWS1DD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K2D2DWS1DD, file = './data/K2D2DWS1DD.rds'); rm(K2D2DWS1DD)
}

## --------------- 70. K2D2DWS2DD ------------------------
if(exists('K2D2DWS2DD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2DD))) {
    saveRDS(K2D2DWS2DD, file = './data/K2D2DWS2DD.rds'); rm(K2D2DWS2DD)
  } else {
    stop('Kindly apply K2D2DWS2DD <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D2DWS2DD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
  saveRDS(K2D2DWS2DD, file = './data/K2D2DWS2DD.rds'); rm(K2D2DWS2DD)
}

## --------------- Dynamic Mixed : Daily + Kick-Off Time ------------------------
## --------------- 71. K1D1DWS1DT ------------------------
if(exists('K1D1DWS1DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1DT))) {
    saveRDS(K1D1DWS1DT, file = './data/K1D1DWS1DT.rds'); rm(K1D1DWS1DT)
  } else {
    stop('Kindly apply K1D1DWS1DT <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D1DWS1DT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K1D1DWS1DT, file = './data/K1D1DWS1DT.rds'); rm(K1D1DWS1DT)
}

## --------------- 72. K1D1DWS2DT ------------------------
if(exists('K1D1DWS2DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2DT))) {
    saveRDS(K1D1DWS2DT, file = './data/K1D1DWS2DT.rds'); rm(K1D1DWS2DT)
  } else {
    stop('Kindly apply K1D1DWS2DT <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D1DWS2DT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K1D1DWS2DT, file = './data/K1D1DWS2DT.rds'); rm(K1D1DWS2DT)
}

## --------------- 73. K1D2DWS1DT ------------------------
if(exists('K1D2DWS1DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1DT))) {
    saveRDS(K1D2DWS1DT, file = './data/K1D2DWS1DT.rds'); rm(K1D2DWS1DT)
  } else {
    stop('Kindly apply K1D2DWS1DT <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D2DWS1DT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K1D2DWS1DT, file = './data/K1D2DWS1DT.rds'); rm(K1D2DWS1DT)
}

## --------------- 74. K1D2DWS2DT ------------------------
if(exists('K1D2DWS2DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2DT))) {
    saveRDS(K1D2DWS2DT, file = './data/K1D2DWS2DT.rds'); rm(K1D2DWS2DT)
  } else {
    stop('Kindly apply K1D2DWS2DT <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D2DWS2DT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K1D2DWS2DT, file = './data/K1D2DWS2DT.rds'); rm(K1D2DWS2DT)
}

## --------------- 75. K2D1DWS1DT ------------------------
if(exists('K2D1DWS1DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1DT))) {
    saveRDS(K2D1DWS1DT, file = './data/K2D1DWS1DT.rds'); rm(K2D1DWS1DT)
  } else {
    stop('Kindly apply K2D1DWS1DT <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D1DWS1DT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K2D1DWS1DT, file = './data/K2D1DWS1DT.rds'); rm(K2D1DWS1DT)
}

## --------------- 76. K2D1DWS2DT ------------------------
if(exists('K2D1DWS2DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2DT))) {
    saveRDS(K2D1DWS2DT, file = './data/K2D1DWS2DT.rds'); rm(K2D1DWS2DT)
  } else {
    stop('Kindly apply K2D1DWS2DT <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D1DWS2DT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K2D1DWS2DT, file = './data/K2D1DWS2DT.rds'); rm(K2D1DWS2DT)
}

## --------------- 77. K2D2DWS1DT ------------------------
if(exists('K2D2DWS1DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1DT))) {
    saveRDS(K2D2DWS1DT, file = './data/K2D2DWS1DT.rds'); rm(K2D2DWS1DT)
  } else {
    stop('Kindly apply K2D2DWS1DT <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D2DWS1DT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K2D2DWS1DT, file = './data/K2D2DWS1DT.rds'); rm(K2D2DWS1DT)
}

## --------------- 78. K2D2DWS2DT ------------------------
if(exists('K2D2DWS2DT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2DT))) {
    saveRDS(K2D2DWS2DT, file = './data/K2D2DWS2DT.rds'); rm(K2D2DWS2DT)
  } else {
    stop('Kindly apply K2D2DWS2DT <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D2DWS2DT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
  saveRDS(K2D2DWS2DT, file = './data/K2D2DWS2DT.rds'); rm(K2D2DWS2DT)
}

## --------------- Dynamic Mixed : Daily + Dynamic ------------------------
## --------------- 79. K1D1DWS1DO ------------------------
if(exists('K1D1DWS1DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1DO))) {
    saveRDS(K1D1DWS1DO, file = './data/K1D1DWS1DO.rds'); rm(K1D1DWS1DO)
  } else {
    stop('Kindly apply K1D1DWS1DO <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D1DWS1DO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K1D1DWS1DO, file = './data/K1D1DWS1DO.rds'); rm(K1D1DWS1DO)
}

## --------------- 80. K1D1DWS2DO ------------------------
if(exists('K1D1DWS2DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2DO))) {
    saveRDS(K1D1DWS2DO, file = './data/K1D1DWS2DO.rds'); rm(K1D1DWS2DO)
  } else {
    stop('Kindly apply K1D1DWS2DO <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D1DWS2DO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K1D1DWS2DO, file = './data/K1D1DWS2DO.rds'); rm(K1D1DWS2DO)
}

## --------------- 81. K1D2DWS1DO ------------------------
if(exists('K1D2DWS1DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1DO))) {
    saveRDS(K1D2DWS1DO, file = './data/K1D2DWS1DO.rds'); rm(K1D2DWS1DO)
  } else {
    stop('Kindly apply K1D2DWS1DO <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D2DWS1DO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K1D2DWS1DO, file = './data/K1D2DWS1DO.rds'); rm(K1D2DWS1DO)
}

## --------------- 82. K1D2DWS2DO ------------------------
if(exists('K1D2DWS2DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2DO))) {
    saveRDS(K1D2DWS2DO, file = './data/K1D2DWS2DO.rds'); rm(K1D2DWS2DO)
  } else {
    stop('Kindly apply K1D2DWS2DO <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D2DWS2DO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K1D2DWS2DO, file = './data/K1D2DWS2DO.rds'); rm(K1D2DWS2DO)
}

## --------------- 83. K2D1DWS1DO ------------------------
if(exists('K2D1DWS1DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1DO))) {
    saveRDS(K2D1DWS1DO, file = './data/K2D1DWS1DO.rds'); rm(K2D1DWS1DO)
  } else {
    stop('Kindly apply K2D1DWS1DO <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D1DWS1DO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K2D1DWS1DO, file = './data/K2D1DWS1DO.rds'); rm(K2D1DWS1DO)
}

## --------------- 84. K2D1DWS2DO ------------------------
if(exists('K2D1DWS2DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2DO))) {
    saveRDS(K2D1DWS2DO, file = './data/K2D1DWS2DO.rds'); rm(K2D1DWS2DO)
  } else {
    stop('Kindly apply K2D1DWS2DO <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D1DWS2DO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K2D1DWS2DO, file = './data/K2D1DWS2DO.rds'); rm(K2D1DWS2DO)
}

## --------------- 85. K2D2DWS1DO ------------------------
if(exists('K2D2DWS1DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1DO))) {
    saveRDS(K2D2DWS1DO, file = './data/K2D2DWS1DO.rds'); rm(K2D2DWS1DO)
  } else {
    stop('Kindly apply K2D2DWS1DO <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D2DWS1DO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K2D2DWS1DO, file = './data/K2D2DWS1DO.rds'); rm(K2D2DWS1DO)
}

## --------------- 86. K2D2DWS2DO ------------------------
if(exists('K2D2DWS2DO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2DO))) {
    saveRDS(K2D2DWS2DO, file = './data/K2D2DWS2DO.rds'); rm(K2D2DWS2DO)
  } else {
    stop('Kindly apply K2D2DWS2DO <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D2DWS2DO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
  saveRDS(K2D2DWS2DO, file = './data/K2D2DWS2DO.rds'); rm(K2D2DWS2DO)
}

## --------------- Dynamic Mixed : Dynamic + Daily ------------------------
## --------------- 87. K1D1DWS1OD ------------------------
if(exists('K1D1DWS1OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1OD))) {
    saveRDS(K1D1DWS1OD, file = './data/K1D1DWS1OD.rds'); rm(K1D1DWS1OD)
  } else {
    stop('Kindly apply K1D1DWS1OD <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D1DWS1OD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K1D1DWS1OD, file = './data/K1D1DWS1OD.rds'); rm(K1D1DWS1OD)
}

## --------------- 88. K1D1DWS2OD ------------------------
if(exists('K1D1DWS2OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2OD))) {
    saveRDS(K1D1DWS2OD, file = './data/K1D1DWS2OD.rds'); rm(K1D1DWS2OD)
  } else {
    stop('Kindly apply K1D1DWS2OD <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D1DWS2OD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K1D1DWS2OD, file = './data/K1D1DWS2OD.rds'); rm(K1D1DWS2OD)
}

## --------------- 89. K1D2DWS1OD ------------------------
if(exists('K1D2DWS1OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1OD))) {
    saveRDS(K1D2DWS1OD, file = './data/K1D2DWS1OD.rds'); rm(K1D2DWS1OD)
  } else {
    stop('Kindly apply K1D2DWS1OD <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D2DWS1OD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K1D2DWS1OD, file = './data/K1D2DWS1OD.rds'); rm(K1D2DWS1OD)
}

## --------------- 90. K1D2DWS2OD ------------------------
if(exists('K1D2DWS2OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2OD))) {
    saveRDS(K1D2DWS2OD, file = './data/K1D2DWS2OD.rds'); rm(K1D2DWS2OD)
  } else {
    stop('Kindly apply K1D2DWS2OD <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K1D2DWS2OD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K1D2DWS2OD, file = './data/K1D2DWS2OD.rds'); rm(K1D2DWS2OD)
}

## --------------- 91. K2D1DWS1OD ------------------------
if(exists('K2D1DWS1OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1OD))) {
    saveRDS(K2D1DWS1OD, file = './data/K2D1DWS1OD.rds'); rm(K2D1DWS1OD)
  } else {
    stop('Kindly apply K2D1DWS1OD <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D1DWS1OD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K2D1DWS1OD, file = './data/K2D1DWS1OD.rds'); rm(K2D1DWS1OD)
}

## --------------- 92. K2D1DWS2OD ------------------------
if(exists('K2D1DWS2OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2OD))) {
    saveRDS(K2D1DWS2OD, file = './data/K2D1DWS2OD.rds'); rm(K2D1DWS2OD)
  } else {
    stop('Kindly apply K2D1DWS2OD <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D1DWS2OD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K2D1DWS2OD, file = './data/K2D1DWS2OD.rds'); rm(K2D1DWS2OD)
}

## --------------- 93. K2D2DWS1OD ------------------------
if(exists('K2D2DWS1OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1OD))) {
    saveRDS(K2D2DWS1OD, file = './data/K2D2DWS1OD.rds'); rm(K2D2DWS1OD)
  } else {
    stop('Kindly apply K2D2DWS1OD <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D2DWS1OD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K2D2DWS1OD, file = './data/K2D2DWS1OD.rds'); rm(K2D2DWS1OD)
}

## --------------- 94. K2D2DWS2OD ------------------------
if(exists('K2D2DWS2OD')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2OD))) {
    saveRDS(K2D2DWS2OD, file = './data/K2D2DWS2OD.rds'); rm(K2D2DWS2OD)
  } else {
    stop('Kindly apply K2D2DWS2OD <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
  }
} else {
  K2D2DWS2OD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
  saveRDS(K2D2DWS2OD, file = './data/K2D2DWS2OD.rds'); rm(K2D2DWS2OD)
}

## --------------- Dynamic Mixed : Dynamic + Kick-Off Time ------------------------
## --------------- 95. K1D1DWS1OT ------------------------
if(exists('K1D1DWS1OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1OT))) {
    saveRDS(K1D1DWS1OT, file = './data/K1D1DWS1OT.rds'); rm(K1D1DWS1OT)
  } else {
    stop('Kindly apply K1D1DWS1OT <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D1DWS1OT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K1D1DWS1OT, file = './data/K1D1DWS1OT.rds'); rm(K1D1DWS1OT)
}

## --------------- 96. K1D1DWS2OT ------------------------
if(exists('K1D1DWS2OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2OT))) {
    saveRDS(K1D1DWS2OT, file = './data/K1D1DWS2OT.rds'); rm(K1D1DWS2OT)
  } else {
    stop('Kindly apply K1D1DWS2OT <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D1DWS2OT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K1D1DWS2OT, file = './data/K1D1DWS2OT.rds'); rm(K1D1DWS2OT)
}

## --------------- 97. K1D2DWS1OT ------------------------
if(exists('K1D2DWS1OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1OT))) {
    saveRDS(K1D2DWS1OT, file = './data/K1D2DWS1OT.rds'); rm(K1D2DWS1OT)
  } else {
    stop('Kindly apply K1D2DWS1OT <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D2DWS1OT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K1D2DWS1OT, file = './data/K1D2DWS1OT.rds'); rm(K1D2DWS1OT)
}

## --------------- 98. K1D2DWS2OT ------------------------
if(exists('K1D2DWS2OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2OT))) {
    saveRDS(K1D2DWS2OT, file = './data/K1D2DWS2OT.rds'); rm(K1D2DWS2OT)
  } else {
    stop('Kindly apply K1D2DWS2OT <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K1D2DWS2OT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K1D2DWS2OT, file = './data/K1D2DWS2OT.rds'); rm(K1D2DWS2OT)
}

## --------------- 99. K2D1DWS1OT ------------------------
if(exists('K2D1DWS1OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1OT))) {
    saveRDS(K2D1DWS1OT, file = './data/K2D1DWS1OT.rds'); rm(K2D1DWS1OT)
  } else {
    stop('Kindly apply K2D1DWS1OT <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D1DWS1OT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K2D1DWS1OT, file = './data/K2D1DWS1OT.rds'); rm(K2D1DWS1OT)
}

## --------------- 100. K2D1DWS2OT ------------------------
if(exists('K2D1DWS2OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2OT))) {
    saveRDS(K2D1DWS2OT, file = './data/K2D1DWS2OT.rds'); rm(K2D1DWS2OT)
  } else {
    stop('Kindly apply K2D1DWS2OT <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D1DWS2OT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K2D1DWS2OT, file = './data/K2D1DWS2OT.rds'); rm(K2D1DWS2OT)
}

## --------------- 101. K2D2DWS1OT ------------------------
if(exists('K2D2DWS1OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1OT))) {
    saveRDS(K2D2DWS1OT, file = './data/K2D2DWS1OT.rds'); rm(K2D2DWS1OT)
  } else {
    stop('Kindly apply K2D2DWS1OT <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D2DWS1OT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K2D2DWS1OT, file = './data/K2D2DWS1OT.rds'); rm(K2D2DWS1OT)
}

## --------------- 102. K2D2DWS2OT ------------------------
if(exists('K2D2DWS2OT')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2OT))) {
    saveRDS(K2D2DWS2OT, file = './data/K2D2DWS2OT.rds'); rm(K2D2DWS2OT)
  } else {
    stop('Kindly apply K2D2DWS2OT <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
  }
} else {
  K2D2DWS2OT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
  saveRDS(K2D2DWS2OT, file = './data/K2D2DWS2OT.rds'); rm(K2D2DWS2OT)
}

## --------------- Dynamic Mixed : Dynamic + Dynamic ------------------------
## --------------- 103. K1D1DWS1OO ------------------------
if(exists('K1D1DWS1OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS1OO))) {
    saveRDS(K1D1DWS1OO, file = './data/K1D1DWS1OO.rds'); rm(K1D1DWS1OO)
  } else {
    stop('Kindly apply K1D1DWS1OO <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D1DWS1OO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K1D1DWS1OO, file = './data/K1D1DWS1OO.rds'); rm(K1D1DWS1OO)
}

## --------------- 104. K1D1DWS2OO ------------------------
if(exists('K1D1DWS2OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D1DWS2OO))) {
    saveRDS(K1D1DWS2OO, file = './data/K1D1DWS2OO.rds'); rm(K1D1DWS2OO)
  } else {
    stop('Kindly apply K1D1DWS2OO <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D1DWS2OO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K1D1DWS2OO, file = './data/K1D1DWS2OO.rds'); rm(K1D1DWS2OO)
}

## --------------- 105. K1D2DWS1OO ------------------------
if(exists('K1D2DWS1OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS1OO))) {
    saveRDS(K1D2DWS1OO, file = './data/K1D2DWS1OO.rds'); rm(K1D2DWS1OO)
  } else {
    stop('Kindly apply K1D2DWS1OO <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D2DWS1OO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K1D2DWS1OO, file = './data/K1D2DWS1OO.rds'); rm(K1D2DWS1OO)
}

## --------------- 106. K1D2DWS2OO ------------------------
if(exists('K1D2DWS2OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K1D2DWS2OO))) {
    saveRDS(K1D2DWS2OO, file = './data/K1D2DWS2OO.rds'); rm(K1D2DWS2OO)
  } else {
    stop('Kindly apply K1D2DWS2OO <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K1D2DWS2OO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K1D2DWS2OO, file = './data/K1D2DWS2OO.rds'); rm(K1D2DWS2OO)
}

## --------------- 107. K2D1DWS1OO ------------------------
if(exists('K2D1DWS1OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS1OO))) {
    saveRDS(K2D1DWS1OO, file = './data/K2D1DWS1OO.rds'); rm(K2D1DWS1OO)
  } else {
    stop('Kindly apply K2D1DWS1OO <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D1DWS1OO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K2D1DWS1OO, file = './data/K2D1DWS1OO.rds'); rm(K2D1DWS1OO)
}

## --------------- 108. K2D1DWS2OO ------------------------
if(exists('K2D1DWS2OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D1DWS2OO))) {
    saveRDS(K2D1DWS2OO, file = './data/K2D1DWS2OO.rds'); rm(K2D1DWS2OO)
  } else {
    stop('Kindly apply K2D1DWS2OO <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D1DWS2OO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K2D1DWS2OO, file = './data/K2D1DWS2OO.rds'); rm(K2D1DWS2OO)
}

## --------------- 109. K2D2DWS1OO ------------------------
if(exists('K2D2DWS1OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS1OO))) {
    saveRDS(K2D2DWS1OO, file = './data/K2D2DWS1OO.rds'); rm(K2D2DWS1OO)
  } else {
    stop('Kindly apply K2D2DWS1OO <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D2DWS1OO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K2D2DWS1OO, file = './data/K2D2DWS1OO.rds'); rm(K2D2DWS1OO)
}

## --------------- 110. K2D2DWS2OO ------------------------
if(exists('K2D2DWS2OO')) {
  if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% names(K2D2DWS2OO))) {
    saveRDS(K2D2DWS2OO, file = './data/K2D2DWS2OO.rds'); rm(K2D2DWS2OO)
  } else {
    stop('Kindly apply K2D2DWS2OO <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
  }
} else {
  K2D2DWS2OO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
  saveRDS(K2D2DWS2OO, file = './data/K2D2DWS2OO.rds'); rm(K2D2DWS2OO)
}

## ================== Monte-Carlo Simulation =============================
source('./function/simulateKelly.R')



## ================== Comparison Models =============================
## list the Kelly models for easier handling.
Kmodels <- c(K1, K2, K1W1, K1W2, K2W1, K2W2, #6
             K1W1WS1, K1W1WS2, K1W2WS1, K1W2WS2, #4 #10
             K2W1WS1, K2W1WS2, K2W2WS1, K2W2WS2, #4 #14
             K1D1, K1D2, K2D1, K2D2, #4 #18
             K1DWS1, K1DWS2, K2DWS1, K2DWS2, #4 #22
             K1D1WS1, K1D1WS2, K1D2WS1, K1D2WS2, #4 #26
             K2D1WS1, K2D1WS2, K2D2WS1, K2D2WS2, #4 #30
             K1W1DWS1, K1W1DWS2, K1W2DWS1, K1W2DWS2, #4 #34
             K2W1DWS1, K2W1DWS2, K2W2DWS1, K2W2DWS2, #4 #38
             K1D1DWS1, K1D1DWS2, K1D2DWS1, K1D2DWS2, #4 #42
             K2D1DWS1, K2D1DWS2, K2D2DWS1, K2D2DWS2, #4 #46
             K1D1DWS1TT, K1D1DWS2TT, K1D2DWS1TT, K1D2DWS2TT, #4 #50
             K2D1DWS1TT, K2D1DWS2TT, K2D2DWS1TT, K2D2DWS2TT, #4 #54
             K1D1DWS1TO, K1D1DWS2TO, K1D2DWS1TO, K1D2DWS2TO, #4 #58
             K2D1DWS1TO, K2D1DWS2TO, K2D2DWS1TO, K2D2DWS2TO, #4 #62
             K1D1DWS1DD, K1D1DWS2DD, K1D2DWS1DD, K1D2DWS2DD, #4 #66
             K2D1DWS1DD, K2D1DWS2DD, K2D2DWS1DD, K2D2DWS2DD, #4 #70
             K1D1DWS1DT, K1D1DWS2DT, K1D2DWS1DT, K1D2DWS2DT, #4 #74
             K2D1DWS1DT, K2D1DWS2DT, K2D2DWS1DT, K2D2DWS2DT, #4 #78
             K1D1DWS1DO, K1D1DWS2DO, K1D2DWS1DO, K1D2DWS2DO, #4 #82
             K2D1DWS1DO, K2D1DWS2DO, K2D2DWS1DO, K2D2DWS2DO, #4 #86
             K1D1DWS1OD, K1D1DWS2OD, K1D2DWS1OD, K1D2DWS2OD, #4 #90
             K2D1DWS1OD, K2D1DWS2OD, K2D2DWS1OD, K2D2DWS2OD, #4 #94
             K1D1DWS1OT, K1D1DWS2OT, K1D2DWS1OT, K1D2DWS2OT, #4 #98
             K2D1DWS1OT, K2D1DWS2OT, K2D2DWS1OT, K2D2DWS2OT, #4 #102
             K1D1DWS1OO, K1D1DWS2OO, K1D2DWS1OO, K1D2DWS2OO, #4 #106
             K2D1DWS1OO, K2D1DWS2OO, K2D2DWS1OO, K2D2DWS2OO) #4 #110

## Similar with profHandling() for load or save profiles at once. You can just using below 
##   function to load all Kelly models, includes save or load.
source('./function/KModels.R', local = TRUE)
KMods <- KModels()

## A lite version function of KModels() due to size allocation issues.
source('./function/KModelslite.R', local = TRUE)
KModslite <- KModelslite()

## 
## Due to both KModels() and KModelslite() unable read 110 files completely. The 
##   size of the objects have over the limit. Here I use below codes to direct read 
##   only necessary bankroll and initial fund size dataset.
## 
source('./function/readKelly.R', local = TRUE)
BRSummary <- readKelly()
saveRDS(BRSummary, file = './data/BRSummary.rds')

## ================== Load Data ================================
## When we saved data by execute above codes, then we just load the data from here onwards. 
## Simulate and save Kelly models for easily loading
source('./function/libs.R')
load('./KellyApps/shinyData.RData')

## Test the efficiency of truncated normal distribution.
#'@ source('./function/rScores.R', local = TRUE)
#'@ scores <- dat[c('FTHG', 'FTAG')]
#'@ scores1 <- rScores(dat, type = 'option1')
#'@ scores2 <- rScores(dat, type = 'option2')
#'@ scores3 <- rScores(dat, type = 'option3')
#'@ scores4 <- rScores(dat, type = 'option4')
#'@ scores5 <- rScores(dat, type = 'option5')
#'@ scores6 <- rScores(dat, type = 'option6')
#'@ scores7 <- rScores(dat, type = 'option7')
#'@ scores8 <- rScores(dat, type = 'option8')
#'@ scores9 <- rScores(dat, type = 'option9')
#'@ scores10 <- rScores(dat, type = 'option10')

## save random scoring models.
#'@ saveRDS(scores, file = './data/scores.rds')
#'@ saveRDS(scores1, file = './data/scores1.rds')
#'@ saveRDS(scores2, file = './data/scores2.rds')
#'@ saveRDS(scores3, file = './data/scores3.rds')
#'@ saveRDS(scores4, file = './data/scores4.rds')
#'@ saveRDS(scores5, file = './data/scores5.rds')
#'@ saveRDS(scores6, file = './data/scores6.rds')
#'@ saveRDS(scores7, file = './data/scores7.rds')
#'@ saveRDS(scores8, file = './data/scores8.rds')
#'@ saveRDS(scores9, file = './data/scores9.rds')
#'@ saveRDS(scores10, file = './data/scores10.rds')

## read random scoring models.
scores <- read_rds(path = './data/scores.rds')
scores1 <- read_rds(path = './data/scores1.rds')
scores2 <- read_rds(path = './data/scores2.rds')
scores3 <- read_rds(path = './data/scores3.rds')
scores4 <- read_rds(path = './data/scores4.rds')
scores5 <- read_rds(path = './data/scores5.rds')
scores6 <- read_rds(path = './data/scores6.rds')
scores7 <- read_rds(path = './data/scores7.rds')
scores8 <- read_rds(path = './data/scores8.rds')
scores9 <- read_rds(path = './data/scores9.rds')
scores10 <- read_rds(path = './data/scores10.rds')

par(mfrow = c(3, 4))
plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
plot(lm(scores1$pred$FTHG ~ scores1$pred$FTAG), main = 'option1', pch = '+')
plot(lm(scores2$pred$FTHG ~ scores2$pred$FTAG), main = 'option2', pch = '+')

par(mfrow = c(3, 4))
plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
plot(lm(scores3$pred$FTHG ~ scores3$pred$FTAG), main = 'option3', pch = '+')
plot(lm(scores4$pred$FTHG ~ scores4$pred$FTAG), main = 'option4', pch = '+')

par(mfrow = c(3, 4))
plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
plot(lm(scores5$pred$FTHG ~ scores5$pred$FTAG), main = 'option5', pch = '+')
plot(lm(scores6$pred$FTHG ~ scores6$pred$FTAG), main = 'option6', pch = '+')

par(mfrow = c(3, 4))
plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
plot(lm(scores7$pred$FTHG ~ scores7$pred$FTAG), main = 'option7', pch = '+')
plot(lm(scores8$pred$FTHG ~ scores8$pred$FTAG), main = 'option8', pch = '+')

par(mfrow = c(3, 4))
plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
plot(lm(scores9$pred$FTHG ~ scores9$pred$FTAG), main = 'option9', pch = '+')
plot(lm(scores10$pred$FTHG ~ scores10$pred$FTAG), main = 'option10', pch = '+')

## ================== Plot Investment Fund =============================
## Convert the various Kelly models into quantmod's xts format for ploting. Compare the growth of 
##   investment funds.
source('./function/compareKelly.R')
compareKelly






