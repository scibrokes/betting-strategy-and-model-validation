Kprof <- function(K, summarisev = subsitute(summarisev)) {
  ## Input 'K' must be a data frame which applied readfirmdata() and arrfirmData() but also the vKelly(),
  ##   the use of Kprof() is that summarise the sessions (soccer session is not equal to calander year or 
  ##   yearly since it is not start from 1st-Jan until 31-Dec.), yearly, monthly, weekly or daily bankroll 
  ##   of investment, you can also choose a nested breakdown like :
  ##     - League breakdown
  ##     - Handicap breakdown
  ##     - price range breakdown
  ##     - result breakdown
  
  ## --------------------- Load packages ----------------------------------------
  options(warn = -1)
  
  suppressMessages(library('formattable'))
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  
  ## load various Kelly function to measure the staking methods and return.
  #'@ source('./function/vKelly.R') #kindly refer to the function.
  
  ## --------------------- Data validation --------------------------------------
  if(!is.data.frame(mbase)) {
    stop('Kindly apply the readfirmData() and arrfirmData() in order to turn the data into a fittable data frame.')
  }
  
  
  
  
  portf1 <- ddply(K1, .(summarise), summarise, 
                  PropHKPriceEdge = mean(PropHKPriceEdge), 
                  Stakes = sum(Stakes), Return = sum(Return), PL = sum(PL), n = length(Sess), rRates = Return / Stakes) %>% mutate(CumStakes = cumsum(Stakes), SPL = cumsum(PL), BR = ifelse(4600 + SPL > 0, 4600 + SPL, -9999), gRates = c(1, exp(diff(log(BR), lag = 1))), gRates2 = BR/BR[1]) %>% tbl_df
  
  
}