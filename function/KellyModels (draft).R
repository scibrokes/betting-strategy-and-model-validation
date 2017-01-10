## ================== Load Data ================================
## Simulate and save Kelly models for easily loading
#'@ source('./function/libs.R', local = TRUE)
#'@ load('./regressionApps/shinyData.RData', envir = .GlobalEnv)

## ================== League Profile ===========================
## Simulate and save the league profiling : all, annual, daily, time or dynamic.
## You just need to run once to complete whole weighted parameters measurement, 
##   and then you can just execute the === Kelly Models === section since it will auto read saved 
##   profiles which will be more efficient compare to use vKelly() to calculate the weighted parameters 
##   and get the result from start to end will take few minutes time to complete.
#'@ source('./function/leagueRiskProf.R', local = TRUE)

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

source('./function/profHandling.R', local = TRUE)
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
## Due to both KModels() and KModelslite() will take longer time to process. The 
##   size of the objects have over the limit. Here I use below codes to direct read 
##   only necessary bankroll and initial fund size dataset.
## 
## 
## simulate Kelly 
## - united initial fund size
## - portion arrangement as baseline stakes for league profile
source('./function/readKelly.R', local = TRUE)

## read initial fund size of bank roll.
if(file.exists('./data/initial.rds')) {
  initial <- read_rds(path = './data/initial.rds')
} else {
  initial <- readKelly(details = 'initial-fund-size', .progress = 'text')
  saveRDS(initial, file = './data/initial.rds')
}

## read summary of bank roll.
if(file.exists('./data/BRSummary.rds')) {
  BRSummary <- read_rds(path = './data/BRSummary.rds')
} else {
  BRSummary <- readKelly(.progress = 'text')
  saveRDS(BRSummary, file = './data/BRSummary.rds')
}

if(file.exists('./data/BR.rds')) {
  BR <- read_rds(path = './data/BR.rds')
} else {
  BR <- readKelly(.summary = FALSE, .progress = 'text')
  saveRDS(BR, file = './data/BR.rds')
}

## read bankroll for plot chart.
#'@ iniVal <- initial$KM[6] %>% as.character %>% as.numeric

## save every funds independently in order to easier load/read.
source('./function/splitFund.R', local = TRUE)
splitFund(.print = TRUE, parallel = TRUE, progress = 'text')
## tried to run few times splitFund() but due to object size allocation 
##   issue and cause the pc forced to restart. Here I run the codes 
##   inside the function and working fine.

SOFund <- read_rds(path = './KellyApps/SOFund.rds')
list.files('./KellyApps', pattern = '.rds$') %>% length
#[1] 6195


## ================== Set initial Fund size ================================


## ================== Load Truncated BivNormDist ================================
## When we saved data by execute above codes, then we just load the data from here onwards. 
## Simulate and save Kelly models for easily loading
#'@ source('./function/libs.R', local = TRUE)
#'@ load('./regressionApps/shinyData.RData')

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
#'@ scores <- read_rds(path = './data/scores.rds')
#'@ scores1 <- read_rds(path = './data/scores1.rds')
#'@ scores2 <- read_rds(path = './data/scores2.rds')
#'@ scores3 <- read_rds(path = './data/scores3.rds')
#'@ scores4 <- read_rds(path = './data/scores4.rds')
#'@ scores5 <- read_rds(path = './data/scores5.rds')
#'@ scores6 <- read_rds(path = './data/scores6.rds')
#'@ scores7 <- read_rds(path = './data/scores7.rds')
#'@ scores8 <- read_rds(path = './data/scores8.rds')
#'@ scores9 <- read_rds(path = './data/scores9.rds')
#'@ scores10 <- read_rds(path = './data/scores10.rds')

#'@ par(mfrow = c(3, 4))
#'@ plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
#'@ plot(lm(scores1$pred$FTHG ~ scores1$pred$FTAG), main = 'option1', pch = '+')
#'@ plot(lm(scores2$pred$FTHG ~ scores2$pred$FTAG), main = 'option2', pch = '+')
#'@ 
#'@ par(mfrow = c(3, 4))
#'@ plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
#'@ plot(lm(scores3$pred$FTHG ~ scores3$pred$FTAG), main = 'option3', pch = '+')
#'@ plot(lm(scores4$pred$FTHG ~ scores4$pred$FTAG), main = 'option4', pch = '+')
#'@ 
#'@ par(mfrow = c(3, 4))
#'@ plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
#'@ plot(lm(scores5$pred$FTHG ~ scores5$pred$FTAG), main = 'option5', pch = '+')
#'@ plot(lm(scores6$pred$FTHG ~ scores6$pred$FTAG), main = 'option6', pch = '+')
#'@ 
#'@ par(mfrow = c(3, 4))
#'@ plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
#'@ plot(lm(scores7$pred$FTHG ~ scores7$pred$FTAG), main = 'option7', pch = '+')
#'@ plot(lm(scores8$pred$FTHG ~ scores8$pred$FTAG), main = 'option8', pch = '+')
#'@ 
#'@ par(mfrow = c(3, 4))
#'@ plot(lm(dat$FTHG ~ dat$FTAG), main = 'raw1', pch = '+')
#'@ plot(lm(scores9$pred$FTHG ~ scores9$pred$FTAG), main = 'option9', pch = '+')
#'@ plot(lm(scores10$pred$FTHG ~ scores10$pred$FTAG), main = 'option10', pch = '+')

source('./function/libs.R', local = TRUE)
source('./function/rScores.R', local = TRUE)
load('./regressionApps/shinyData.RData')

scoresall <- rScores(dat, type = 'all')
saveRDS(scoresall, file = './data/scoresall.rds')
scoresall <- read_rds(path = './data/scoresall.rds')

## ================== Plot Investment Fund ===============================
## Convert the various Kelly models into quantmod's xts format for ploting. Compare the growth of 
##   investment funds.
source('./function/compareKelly.R', local = TRUE)
compareKelly



## ================== Monte-Carlo Simulation =============================
source('./function/simulateKelly.R', local = TRUE)




