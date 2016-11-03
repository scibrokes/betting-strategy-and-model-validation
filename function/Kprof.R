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
  suppressMessages(library('BBmisc'))
  suppresAll(source('./function/decode.R'))
  
  ## load various Kelly function to measure the staking methods and return.
  #'@ source('./function/vKelly.R') #kindly refer to the function.
  
  ## --------------------- Data validation --------------------------------------
  if(!is.data.frame(mbase)) {
    stop('Kindly apply the readfirmData() and arrfirmData() in order to turn the data into a fittable data frame.')
  }
  
  ## --------------------- Bank-roll --------------------------------------
  ## Bank roll flow statement.
  
  K %<>% mutate(KBRHKPriceEdge = cumsum(KReturnHKPriceEdge - KStakesHKPriceEdge), 
                KBRnetProbBEdge = cumsum(KReturnnetProbBEdge - KStakesnetProbBEdge), 
                KBRHKPrice = cumsum(KReturnHKPrice - KStakesHKPrice), 
                KBRnetProbB = cumsum(KReturnnetProbB - KStakesnetProbB), 
                KBRFixed = cumsum(KReturnFixed - KStakesFixed), 
                KBRFixednetProbB = cumsum(KReturnFixednetProbB - KStakesFixednetProbB), 
                KBREMProb = cumsum(KReturnEMProb - KStakesEMProb), 
                KBREMProbnetProbB = cumsum(KReturnEMProbnetProbB - KStakesEMProbnetProbB), 
                KBRHalf = cumsum(KReturnHalf - KStakesHalf), 
                KBRHalfnetProbB = cumsum(KReturnHalfnetProbB - KStakesHalfnetProbB), 
                KBRQuarter = cumsum(KReturnQuarter - KStakesQuarter), 
                KBRQuarternetProbB = cumsum(KReturnQuarternetProbB - KStakesQuarternetProbB), 
                KBRAdj = cumsum(KReturnAdj - KStakesAdj), 
                KBRAdjnetProbB = cumsum(KReturnAdjnetProbB - KStakesAdjnetProbB), 
                KBRHalfAdj = cumsum(KReturnHalfAdj - KStakesHalfAdj), 
                KBRHalfAdjnetProbB = cumsum(KReturnHalfAdjnetProbB - KStakesHalfAdjnetProbB), 
                KBREMQuarterAdj = cumsum(KReturnEMQuarterAdj - KStakesEMQuarterAdj), 
                KBREMQuarterAdjnetProbB = cumsum(KReturnEMQuarterAdjnetProbB - KStakesEMQuarterAdjnetProbB))
  
  
  
  
  portf1 <- ddply(K1, .(summarise), summarise, 
                  PropHKPriceEdge = mean(PropHKPriceEdge), 
                  Stakes = sum(Stakes), Return = sum(Return), PL = sum(PL), n = length(Sess), 
                  rRates = Return / Stakes) %>% 
    mutate(CumStakes = cumsum(Stakes), SPL = cumsum(PL), BR = ifelse(4600 + SPL > 0, 4600 + SPL, -9999), 
           gRates = c(1, exp(diff(log(BR), lag = 1))), gRates2 = BR/BR[1]) %>% tbl_df
  
  ddply(K$Kelly1, .(Sess), summarise, Stakes = sum(Stakes), HKPrice = mean(HKPrice), EUPrice = mean(EUPrice), Return = sum(Return), PL = sum(PL), rRates = mean(rRates), netEMEdge = mean(netEMEdge), netProbB = mean(netProbB), rEMProbB = mean(rEMProbB), PropHKPriceEdge = mean(PropHKPriceEdge), PropnetProbBEdge = mean(PropnetProbBEdge), KProbHKPrice = mean(KProbHKPrice), KProbnetProbB = mean(KProbnetProbB), KProbFixed = mean(KProbFixed), KProbFixednetProbB = mean(KProbFixednetProbB), KEMProb = mean(KEMProb), KEMProbnetProbB = mean(KEMProbnetProbB), KProbHalf = mean(KProbHalf), KProbHalfnetProbB = mean(KProbHalfnetProbB), KProbQuarter = mean(KProbQuarter), KProbQuarternetProbB = mean(KProbQuarternetProbB), KProbAdj = mean(KProbAdj), KProbAdjnetProbB = mean(KProbAdjnetProbB), KHalfAdj = mean(KHalfAdj), KProbHalfnetProbB = mean(KProbHalfnetProbB), )
  
  ## [4] "Stakes"                      "HCap"                        "HKPrice"                    
  ## [7] "EUPrice"                     "Result"                      "Return"                     
  ## [10] "PL"                          "rRates"                      "netEMEdge"                  
  ## [13] "netProbB"                    "rEMProbB"                    "weight.stakes"              
  ## [16] "weight"                      "PropHKPriceEdge"             "PropnetProbBEdge"           
  ## [19] "KProbHKPrice"                "KProbnetProbB"               "KProbFixed"                 
  ## [22] "KProbFixednetProbB"          "KEMProb"                     "KEMProbnetProbB"            
  ## [25] "KProbHalf"                   "KProbHalfnetProbB"           "KProbQuarter"               
  ## [28] "KProbQuarternetProbB"        "KProbAdj"                    "KProbAdjnetProbB"           
  ## [31] "KHalfAdj"                    "KHalfAdjnetProbB"            "KEMQuarterAdj"              
  ## [34] "KEMQuarterAdjnetProbB"       "KStakesHKPriceEdge"          "KStakesnetProbBEdge"        
  ## [37] "KStakesHKPrice"              "KStakesnetProbB"             "KStakesFixed"               
  ## [40] "KStakesFixednetProbB"        "KStakesEMProb"               "KStakesEMProbnetProbB"      
  ## [43] "KStakesHalf"                 "KStakesHalfnetProbB"         "KStakesQuarter"             
  ## [46] "KStakesQuarternetProbB"      "KStakesAdj"                  "KStakesAdjnetProbB"         
  ## [49] "KStakesHalfAdj"              "KStakesHalfAdjnetProbB"      "KStakesEMQuarterAdj"        
  ## [52] "KStakesEMQuarterAdjnetProbB" "KReturnHKPriceEdge"          "KReturnnetProbBEdge"        
  ## [55] "KReturnHKPrice"              "KReturnnetProbB"             "KReturnFixed"               
  ## [58] "KReturnFixednetProbB"        "KReturnEMProb"               "KReturnEMProbnetProbB"      
  ## [61] "KReturnHalf"                 "KReturnHalfnetProbB"         "KReturnQuarter"             
  ## [64] "KReturnQuarternetProbB"      "KReturnAdj"                  "KReturnAdjnetProbB"         
  ## [67] "KReturnHalfAdj"              "KReturnHalfAdjnetProbB"      "KReturnEMQuarterAdj"        
  ## [70] "KReturnEMQuarterAdjnetProbB"
  
  }