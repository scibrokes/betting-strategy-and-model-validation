## ================== Load Data ================================
## Simulate and save Kelly models for easily loading
source('./function/libs.R')
load('./KellyApps/shinyData.RData', envir = .GlobalEnv)

## ================== League Profile ===========================
## Simulate and save the league profiling : all, annual, daily, time or dynamic.
## You just need to run once to complete whole weighted parameters measurement, 
##   and then you can just execute the === Kelly Models === section since it will auto read saved 
##   profiles which will be more efficient compare to use vKelly() to calculate the weighted parameters 
##   and get the result from start to end will take few minutes time to complete.
source('./function/leagueRiskProf.R')

## --------------- league weight parameters --------------------
lRProf1A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'all')
lRProf1B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'all')
lRProf2A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'annual')
lRProf2B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'annual')
lRProf3A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'daily')
lRProf3B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'daily')
lRProf4A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'time')
lRProf4B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'time')
lRProf5A <- leagueRiskProf(dat, type = 'weight.stakes', weight.type = 'dynamic')
lRProf5B <- leagueRiskProf(dat, type = 'weight.stakes', breakdown = TRUE, weight.type = 'dynamic')

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

## we can also compare the sd of the weight value and doing further adjustment.

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

## ================== Load Data ================================
## When we saved data by execute above codes, then we just load the data from here onwards. 
## Simulate and save Kelly models for easily loading
source('./function/libs.R')
load('./KellyApps/shinyData.RData', envir = .GlobalEnv)

## ================== Kelly Models =============================
## --------------- 1. K1 ------------------------
## Reserved Stakes Kelly Models.
K1 <- vKelly(dat)

## --------------- 2. K2 ------------------------
## Reserved EM Probabilities Kelly Models.
K2 <- vKelly2(dat)

## --------------- 3. K1W1 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual theta constant value.
##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
K1W1 <- vKelly(dat, type = 'W1')

## --------------- 4. K1W2 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual weighted hdp parameter.
##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
K1W2 <- vKelly(dat, type = 'W2')

## --------------- 5. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value.
##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
K2W1 <- vKelly2(dat, type = 'W1')

## --------------- 6. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual weighted hdp parameter.
##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
K2W2 <- vKelly2(dat, type = 'W2')

## --------------- 7. K1W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1WS1 <- vKelly(dat, type = 'W1WS1')

## --------------- 8. K1W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1WS2 <- vKelly(dat, type = 'W1WS2')

## --------------- 9. K1W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2WS1 <- vKelly(dat, type = 'W2WS1')

## --------------- 10. K1W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2WS2 <- vKelly2(dat, type = 'W2WS2')

## --------------- 11. K2W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1WS1 <- vKelly2(dat, type = 'W1WS1')

## --------------- 12. K2W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1WS2 <- vKelly2(dat, type = 'W1WS2')

## --------------- 13. K2W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2WS1 <- vKelly2(dat, type = 'W2WS1')

## --------------- 14. K2W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2WS2 <- vKelly2(dat, type = 'W2WS2')

## --------------- Default Dynamic ------------------------
## --------------- 15. K1D1 ------------------------
K1D1 <- vKelly(dat, type = 'D1')

## --------------- 16. K1D2 ------------------------
K2D2 <- vKelly(dat, type = 'D2')

## --------------- 17. K2D1 ------------------------
K2D1 <- vKelly2(dat, type = 'D1')

## --------------- 18. K2D2 ------------------------
K2D2 <- vKelly2(dat, type = 'D2')

## --------------- 19. K1D1 ------------------------
K1DWS1 <- vKelly(dat, type = 'DWS1')

## --------------- 20. K1D2 ------------------------
K2DWS2 <- vKelly(dat, type = 'DWS2')

## --------------- 21. K2D1 ------------------------
K2DWS1 <- vKelly2(dat, type = 'DWS1')

## --------------- 22. K2D2 ------------------------
K2DWS2 <- vKelly2(dat, type = 'DWS2')

## --------------- 23. K1D1WS1 ------------------------
K1D1WS1 <- vKelly(dat, type = 'D1WS1')

## --------------- 24. K1D1WS2 ------------------------
K1D1WS2 <- vKelly(dat, type = 'D1WS2')

## --------------- 25. K1D2WS1 ------------------------
K1D2WS1 <- vKelly(dat, type = 'D2WS1')

## --------------- 26. K1D2WS2 ------------------------
K1D2WS2 <- vKelly(dat, type = 'D2WS2')

## --------------- 27. K2D1WS1 ------------------------
K2D1WS1 <- vKelly2(dat, type = 'D1WS1')

## --------------- 28. K2D1WS2 ------------------------
K2D1WS2 <- vKelly2(dat, type = 'D1WS2')

## --------------- 29. K2D2WS1 ------------------------
K2D2WS1 <- vKelly2(dat, type = 'D2WS1')

## --------------- 30. K2D2WS2 ------------------------
K2D2WS2 <- vKelly2(dat, type = 'D2WS2')

## --------------- 31. K1W1DWS1 ------------------------
K1W1DWS1 <- vKelly(dat, type = 'W1DWS1')

## --------------- 32. K1W1DWS2 ------------------------
K1W1DWS2 <- vKelly(dat, type = 'W1WS2')

## --------------- 33. K1W2DWS1 ------------------------
K1W2DWS1 <- vKelly(dat, type = 'W2DWS1')

## --------------- 34. K1W2DWS2 ------------------------
K1W2DWS2 <- vKelly(dat, type = 'W2DWS2')

## --------------- 35. K2W1DWS1 ------------------------
K2W1DWS1 <- vKelly2(dat, type = 'W1DWS1')

## --------------- 36. K2W1DWS2 ------------------------
K2W1DWS2 <- vKelly2(dat, type = 'W1DWS2')

## --------------- 37. K2W2DWS1 ------------------------
K2W2DWS1 <- vKelly2(dat, type = 'W2DWS1')

## --------------- 38. K2W2DWS2 ------------------------
K2W2DWS2 <- vKelly2(dat, type = 'W2DWS2')

## --------------- 39. K1D1DWS1 ------------------------
K1D1DWS1 <- vKelly(dat, type = 'D1DWS1')

## --------------- 40. K1D1DWS2 ------------------------
K1D1DWS2 <- vKelly(dat, type = 'D1DWS2')

## --------------- 41. K1D2DWS1 ------------------------
K1D2DWS1 <- vKelly(dat, type ='D2DWS1')

## --------------- 42. K1D2DWS2 ------------------------
K1D2DWS2 <- vKelly(dat, type = 'D2DWS2')

## --------------- 43. K2D1DWS1 ------------------------
K2D1DWS1 <- vKelly2(dat, type = 'D1DWS1')

## --------------- 44. K2D1DWS2 ------------------------
K2D1DWS2 <- vKelly2(dat, type = 'D1DWS2')

## --------------- 45. K2D2DWS1 ------------------------
K2D2DWS1 <- vKelly2(dat, type = 'D2DWS1')

## --------------- 46. K2D2DWS2 ------------------------
K2D2DWS2 <- vKelly2(dat, type = 'D2DWS2')

## --------------- Dynamic Mixed : Kick-Off Time + Kick-Off Time ------------------------
## --------------- 47. K1D1DWS1 ------------------------
K1D1DWS1TT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 48. K1D1DWS2 ------------------------
K1D1DWS2TT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 49. K1D2DWS1 ------------------------
K1D2DWS1TT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 50. K1D2DWS2 ------------------------
K1D2DWS2TT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 51. K2D1DWS1 ------------------------
K2D1DWS1TT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 52. K2D1DWS2 ------------------------
K2D1DWS2TT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 53. K2D2DWS1 ------------------------
K2D2DWS1TT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- 54. K2D2DWS2 ------------------------
K2D2DWS2TT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')

## --------------- Dynamic Mixed : Kick-Off Time + Dynamic ------------------------
## --------------- 55. K1D1DWS1 ------------------------
K1D1DWS1TO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 56. K1D1DWS2 ------------------------
K1D1DWS2TO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 57. K1D2DWS1 ------------------------
K1D2DWS1TO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 58. K1D2DWS2 ------------------------
K1D2DWS2TO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 59. K2D1DWS1 ------------------------
K2D1DWS1TO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 60. K2D1DWS2 ------------------------
K2D1DWS2TO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 61. K2D2DWS1 ------------------------
K2D2DWS1TO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- 62. K2D2DWS2 ------------------------
K2D2DWS2TO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')

## --------------- Dynamic Mixed : Daily + Daily ------------------------
## --------------- 63. K1D1DWS1 ------------------------
K1D1DWS1DD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 64. K1D1DWS2 ------------------------
K1D1DWS2DD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 65. K1D2DWS1 ------------------------
K1D2DWS1DD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 66. K1D2DWS2 ------------------------
K1D2DWS2DD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 67. K2D1DWS1 ------------------------
K2D1DWS1DD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 68. K2D1DWS2 ------------------------
K2D1DWS2DD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 69. K2D2DWS1 ------------------------
K2D2DWS1DD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- 70. K2D2DWS2 ------------------------
K2D2DWS2DD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')

## --------------- Dynamic Mixed : Daily + Kick-Off Time ------------------------
## --------------- 71. K1D1DWS1 ------------------------
K1D1DWS1DT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 72. K1D1DWS2 ------------------------
K1D1DWS2DT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 73. K1D2DWS1 ------------------------
K1D2DWS1DT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 74. K1D2DWS2 ------------------------
K1D2DWS2DT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 75. K2D1DWS1 ------------------------
K2D1DWS1DT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 76. K2D1DWS2 ------------------------
K2D1DWS2DT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 77. K2D2DWS1 ------------------------
K2D2DWS1DT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- 78. K2D2DWS2 ------------------------
K2D2DWS2DT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')

## --------------- Dynamic Mixed : Daily + Dynamic ------------------------
## --------------- 79. K1D1DWS1 ------------------------
K1D1DWS1DO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 80. K1D1DWS2 ------------------------
K1D1DWS2DO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 81. K1D2DWS1 ------------------------
K1D2DWS1DO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 82. K1D2DWS2 ------------------------
K1D2DWS2DO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 83. K2D1DWS1 ------------------------
K2D1DWS1DO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 84. K2D1DWS2 ------------------------
K2D1DWS2DO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 85. K2D2DWS1 ------------------------
K2D2DWS1DO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- 86. K2D2DWS2 ------------------------
K2D2DWS2DO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')

## --------------- Dynamic Mixed : Dynamic + Daily ------------------------
## --------------- 87. K1D1DWS1 ------------------------
K1D1DWS1OD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 88. K1D1DWS2 ------------------------
K1D1DWS2OD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 89. K1D2DWS1 ------------------------
K1D2DWS1OD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 90. K1D2DWS2 ------------------------
K1D2DWS2OD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 91. K2D1DWS1 ------------------------
K2D1DWS1OD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 92. K2D1DWS2 ------------------------
K2D1DWS2OD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 93. K2D2DWS1 ------------------------
K2D2DWS1OD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- 94. K2D2DWS2 ------------------------
K2D2DWS2OD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')

## --------------- Dynamic Mixed : Dynamic + Kick-Off Time ------------------------
## --------------- 95. K1D1DWS1 ------------------------
K1D1DWS1OT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 96. K1D1DWS2 ------------------------
K1D1DWS2OT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 97. K1D2DWS1 ------------------------
K1D2DWS1OT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 98. K1D2DWS2 ------------------------
K1D2DWS2OT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 99. K2D1DWS1 ------------------------
K2D1DWS1OT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 100. K2D1DWS2 ------------------------
K2D1DWS2OT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 101. K2D2DWS1 ------------------------
K2D2DWS1OT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- 102. K2D2DWS2 ------------------------
K2D2DWS2OT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')

## --------------- Dynamic Mixed : Dynamic + Dynamic ------------------------
## --------------- 103. K1D1DWS1 ------------------------
K1D1DWS1OO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 104. K1D1DWS2 ------------------------
K1D1DWS2OO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 105. K1D2DWS1 ------------------------
K1D2DWS1OO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 106. K1D2DWS2 ------------------------
K1D2DWS2OO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 107. K2D1DWS1 ------------------------
K2D1DWS1OO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 108. K2D1DWS2 ------------------------
K2D1DWS2OO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 109. K2D2DWS1 ------------------------
K2D2DWS1OO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## --------------- 110. K2D2DWS2 ------------------------
K2D2DWS2OO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')

## ================== Comparison Models =============================
## 


## ================== Plot Investment Fund =============================
## Convert the various Kelly models into quantmod's xts format for ploting. Compare the growth of 
##   investment funds.
source('./function/compareKelly.R')
compareKelly














