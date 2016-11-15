## ================== Load Data ================================
## Simulate and save Kelly models for easily loading
source('./function/libs.R')
load('./KellyApps/shinyData.RData', envir = .GlobalEnv)

## ================== League Profile ===========================
## Simulate and save the league profiling : all, annual, daily, time or dynamic.
source('./function/leagueRiskProf.R')

## --------------- league weight parameters --------------------
lRProf1A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'all')
lRProf1B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'all')
lRProf2A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'annual')
lRProf2B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual')
lRProf3A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'daily')
lRProf3B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'daily')
lRProf4A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'time')
lRProf4B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'time') #more than 6millions observations, need to take few hours.
lRProf5A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'dynamic') #more than 5millions observations, need to take few hours.
lRProf5B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'dynamic') #more than 10 millions observations, take more than 5 hours.

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

## combination of the weitgh values.
#'@ lRiskProf <- join_all(list(lRProf1A, lRProf1B, lRProf2A, lRProf2B, lRProf3A, lRProf3B, 
#'@                            lRProf4A, lRProf4B, lRProf5A, lRProf5B)) %>% tbl_df %>% 
#'@   select(No.x, Sess, DateUS, TimeUS, ...) # need to modify
#'@ names(lRiskProf) <- c('No.x', 'Sess', 'DateUS', 'TimeUS', 'all.1', 'all.2', 'ann.1', 'ann.2', 'day.1', 'day.2', 'time.1', 
#'@                       'time.2', 'dym.1', 'dym.2')

saveRDS(lRProf1A, file = './data/lRProf1A.rds')
saveRDS(lRProf1B, file = './data/lRProf1B.rds')
saveRDS(lRProf2A, file = './data/lRProf2A.rds')
saveRDS(lRProf2B, file = './data/lRProf2B.rds')
saveRDS(lRProf3A, file = './data/lRProf3A.rds')
saveRDS(lRProf3B, file = './data/lRProf3B.rds')
saveRDS(lRProf4A, file = './data/lRProf4A.rds')
saveRDS(lRProf4B, file = './data/lRProf4B.rds')
saveRDS(lRProf5A, file = './data/lRProf5A.rds')
saveRDS(lRProf5B, file = './data/lRProf5B.rds')

## -------------------- result based weight parameters --------------------
wProf1A <- leagueRiskProf(dat, type = 'weight', weight.type = 'all')
wProf1B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'all')
wProf2A <- leagueRiskProf(dat, type = 'weight', weight.type = 'annual')
wProf2B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'annual')
wProf3A <- leagueRiskProf(dat, type = 'weight', weight.type = 'daily')
wProf3B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'daily')
wProf4A <- leagueRiskProf(dat, type = 'weight', weight.type = 'time')
wProf4B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'time')
wProf5A <- leagueRiskProf(dat, type = 'weight', weight.type = 'dynamic')
wProf5B <- leagueRiskProf(dat, type = 'weight', breakdown = TRUE, weight.type = 'dynamic')

saveRDS(wProf1A, file = './data/wProf1A.rds')
saveRDS(wProf1B, file = './data/wProf1B.rds')
saveRDS(wProf2A, file = './data/wProf2A.rds')
saveRDS(wProf2B, file = './data/wProf2B.rds')
saveRDS(wProf3A, file = './data/wProf3A.rds')
saveRDS(wProf3B, file = './data/wProf3B.rds')
saveRDS(wProf4A, file = './data/wProf4A.rds')
saveRDS(wProf4B, file = './data/wProf4B.rds')
saveRDS(wProf5A, file = './data/wProf5A.rds')
saveRDS(wProf5B, file = './data/wProf5B.rds')

## ================== Kelly Models =============================
## --------------- 1. K1 ------------------------
## Reserved Stakes Kelly Models.
K1 <- vKelly(dat)

## --------------- 2. K2 ------------------------
## Reserved EM Probabilities Kelly Models.
K2 <- vKelly2(dat)

## --------------- 3. K1W1 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L)
K1W1 <- vKelly(dat, type = 'W1')

## --------------- 4. K1W2 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L)
K1W2 <- vKelly(dat, type = 'W2')

## --------------- 5. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L)
K2W1 <- vKelly2(dat, type = 'W1')

## --------------- 6. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L)
K2W2 <- vKelly2(dat, type = 'W2')

## --------------- 7. K1W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1WS1 <- vKelly(dat, type = 'W1', weight.stakes = .....)

## --------------- 8. K1W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1WS2 <- vKelly()

## --------------- 9. K1W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2WS1 <- vKelly()

## --------------- 10. K1W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2WS2 <- vKelly2()

## --------------- 11. K2W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1WS1 <- vKelly2()

## --------------- 12. K2W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1WS2 <- vKelly2()

## --------------- 13. K2W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2WS1 <- vKelly2()

## --------------- 14. K2W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2WS2 <- vKelly2()

## --------------- Dynamic Daily ------------------------
## --------------- 15. K1D1 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K1D1 <- vKelly(dat, type = 'D1')

## --------------- 16. K1D2 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly(dat, type = 'D2')

## --------------- 17. K2D1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K2D1 <- vKelly2(dat, type = 'D1')

## --------------- 18. K2D2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly2(dat, type = 'D2')

## --------------- 19. K1D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1WS1 <- vKelly()

## --------------- 20. K1D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1WS2 <- vKelly()

## --------------- 21. K1D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2WS1 <- vKelly()

## --------------- 22. K1D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2WS2 <- vKelly()

## --------------- 23. K2D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1WS1 <- vKelly2()

## --------------- 24. K2D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1WS2 <- vKelly2()

## --------------- 25. K2D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2WS1 <- vKelly2()

## --------------- 26. K2D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2WS2 <- vKelly2()

## --------------- 27. K1W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1DWS1 <- vKelly()

## --------------- 28. K1W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1DWS2 <- vKelly()

## --------------- 29. K1W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2DWS1 <- vKelly()

## --------------- 30. K1W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2DWS2 <- vKelly()

## --------------- 31. K2W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1DWS1 <- vKelly2()

## --------------- 32. K2W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1DWS2 <- vKelly2()

## --------------- 33. K2W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2DWS1 <- vKelly2()

## --------------- 34. K2W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2DWS2 <- vKelly2()

## --------------- 35. K1D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1DWS1 <- vKelly()

## --------------- 36. K1D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1DWS2 <- vKelly()

## --------------- 37. K1D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2DWS1 <- vKelly()

## --------------- 38. K1D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2DWS2 <- vKelly()

## --------------- 39. K2D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1DWS1 <- vKelly2()

## --------------- 40. K2D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1DWS2 <- vKelly2()

## --------------- 41. K2D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2DWS1 <- vKelly2()

## --------------- 42. K2D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2DWS2 <- vKelly2()

## --------------- Dynamic Kick-Off Time ------------------------
## --------------- 15. K1D1 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K1D1 <- vKelly(dat, type = 'D1')

## --------------- 16. K1D2 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly(dat, type = 'D2')

## --------------- 17. K2D1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K2D1 <- vKelly2(dat, type = 'D1')

## --------------- 18. K2D2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly2(dat, type = 'D2')

## --------------- 19. K1D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1WS1 <- vKelly()

## --------------- 20. K1D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1WS2 <- vKelly()

## --------------- 21. K1D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2WS1 <- vKelly()

## --------------- 22. K1D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2WS2 <- vKelly()

## --------------- 23. K2D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1WS1 <- vKelly2()

## --------------- 24. K2D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1WS2 <- vKelly2()

## --------------- 25. K2D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2WS1 <- vKelly2()

## --------------- 26. K2D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2WS2 <- vKelly2()

## --------------- 27. K1W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1DWS1 <- vKelly()

## --------------- 28. K1W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1DWS2 <- vKelly()

## --------------- 29. K1W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2DWS1 <- vKelly()

## --------------- 30. K1W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2DWS2 <- vKelly()

## --------------- 31. K2W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1DWS1 <- vKelly2()

## --------------- 32. K2W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1DWS2 <- vKelly2()

## --------------- 33. K2W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2DWS1 <- vKelly2()

## --------------- 34. K2W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2DWS2 <- vKelly2()

## --------------- 35. K1D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1DWS1 <- vKelly()

## --------------- 36. K1D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1DWS2 <- vKelly()

## --------------- 37. K1D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2DWS1 <- vKelly()

## --------------- 38. K1D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2DWS2 <- vKelly()

## --------------- 39. K2D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1DWS1 <- vKelly2()

## --------------- 40. K2D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1DWS2 <- vKelly2()

## --------------- 41. K2D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2DWS1 <- vKelly2()

## --------------- 42. K2D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2DWS2 <- vKelly2()

## --------------- Dynamic Dynamic ------------------------
## --------------- 15. K1D1 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K1D1 <- vKelly(dat, type = 'D1')

## --------------- 16. K1D2 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly(dat, type = 'D2')

## --------------- 17. K2D1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K2D1 <- vKelly2(dat, type = 'D1')

## --------------- 18. K2D2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly2(dat, type = 'D2')

## --------------- 19. K1D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1WS1 <- vKelly()

## --------------- 20. K1D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1WS2 <- vKelly()

## --------------- 21. K1D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2WS1 <- vKelly()

## --------------- 22. K1D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2WS2 <- vKelly()

## --------------- 23. K2D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1WS1 <- vKelly2()

## --------------- 24. K2D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1WS2 <- vKelly2()

## --------------- 25. K2D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2WS1 <- vKelly2()

## --------------- 26. K2D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2WS2 <- vKelly2()

## --------------- 27. K1W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1DWS1 <- vKelly()

## --------------- 28. K1W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1DWS2 <- vKelly()

## --------------- 29. K1W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2DWS1 <- vKelly()

## --------------- 30. K1W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2DWS2 <- vKelly()

## --------------- 31. K2W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1DWS1 <- vKelly2()

## --------------- 32. K2W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1DWS2 <- vKelly2()

## --------------- 33. K2W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2DWS1 <- vKelly2()

## --------------- 34. K2W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2DWS2 <- vKelly2()

## --------------- 35. K1D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1DWS1 <- vKelly()

## --------------- 36. K1D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1DWS2 <- vKelly()

## --------------- 37. K1D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2DWS1 <- vKelly()

## --------------- 38. K1D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2DWS2 <- vKelly()

## --------------- 39. K2D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1DWS1 <- vKelly2()

## --------------- 40. K2D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1DWS2 <- vKelly2()

## --------------- 41. K2D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2DWS1 <- vKelly2()

## --------------- 42. K2D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2DWS2 <- vKelly2()

## --------------- Dynamic Mixed Daily + Kick-Off Time ------------------------
## --------------- 19. K1D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
weight <- suppressAll(join_all(pRprof3A, pRprof4A)$weight %>% mutate(lag(unique(DateUS)))
weight.stakes <- suppressAll(join_all(, lRprof3A, lRprof4A)$daily.1 %>% mutate(lag(unique(DateUS)))

K1Mixed <- vKelly(weight = , weight.stakes = suppressAll(, ), type = 'flat')

## --------------- 20. K1D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1WS2 <- vKelly()

## --------------- 21. K1D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2WS1 <- vKelly()

## --------------- 22. K1D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2WS2 <- vKelly()

## --------------- 23. K2D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1WS1 <- vKelly2()

## --------------- 24. K2D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1WS2 <- vKelly2()

## --------------- 25. K2D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2WS1 <- vKelly2()

## --------------- 26. K2D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2WS2 <- vKelly2()

## --------------- 27. K1W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1DWS1 <- vKelly()

## --------------- 28. K1W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1DWS2 <- vKelly()

## --------------- 29. K1W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2DWS1 <- vKelly()

## --------------- 30. K1W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2DWS2 <- vKelly()

## --------------- 31. K2W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1DWS1 <- vKelly2()

## --------------- 32. K2W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1DWS2 <- vKelly2()

## --------------- 33. K2W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2DWS1 <- vKelly2()

## --------------- 34. K2W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2DWS2 <- vKelly2()

## --------------- 35. K1D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1DWS1 <- vKelly()

## --------------- 36. K1D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1DWS2 <- vKelly()

## --------------- 37. K1D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2DWS1 <- vKelly()

## --------------- 38. K1D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2DWS2 <- vKelly()

## --------------- 39. K2D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1DWS1 <- vKelly2()

## --------------- 40. K2D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1DWS2 <- vKelly2()

## --------------- 41. K2D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2DWS1 <- vKelly2()

## --------------- 42. K2D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2DWS2 <- vKelly2()
## ================== Plot Investment Fund =============================
## convert the various Kelly models into quantmod's xts format for ploting. Compare the growth of 
##   investment funds.
source('./function/compareKelly.R')
compareKelly














