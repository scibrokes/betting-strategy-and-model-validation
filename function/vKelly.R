vKelly <- function(mbase, weight.stakes = 1, weight = 1) {
  ## Comparison of various fractional Kelly models
  ## 
  ## Kindly apply readfirmDate() and arrfirmData() prior to measure the various 
  ##   Kelly models.
  ## 
  ## weight.stakes = a numeric weight parameter in single or vector format.
  ## weight = a numeric weight parameter in single or vector format.
  
  ## --------------------- Load packages ----------------------------------------
  options(warn = -1)
  
  suppressMessages(library('formattable'))
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  
  ## --------------------- Convert probabilities -------------------------------- 
  m <- ddply(mbase, .(Sess), summarise, rRates = percent(mean(rRates))) %>% tbl_df
  
  if(any(!c('rRates', 'rEMProbB', 'rEMProbL', 'netEMEdge') %in% names(dat))){
    mbase$rRates <- rep(as.numeric(m$rRates + 1), ddply(dat, .(Sess), summarise, 
                                                        n = length(Sess))$n)
    mbase %<>% mutate(rEMProbB = rRates * netProbB, rEMProbL = 1 - dat$rEMProbB, 
                      netEMEdge = rEMProbB / netProbB)
  }
  rm(m)
  
  ## --------------------- Data validation --------------------------------------
  if(!is.data.frame(mbase)) {
    stop('Kindly apply the readfirmData() and arrfirmData() in order to turn the data into a fittable data frame.')
  }
  
  wt <- data_frame(No = seq(nrow(mbase)))
  
  if(!is.numeric(weight.stakes)) {
    wt$weight.stakes <- 1
    } else {
      if(!is.vector(weight.stakes)) {
        stop('Kindly insert a range of vector or single numeric value as weight.stakes parameter.')
        } else {
          if(is.vector(weight.stakes)) wt$weight.stakes <- weight.stakes
        }
    }
  
  if(!is.numeric(weight)) {
    wt$weight <- 1
    } else {
      if(!is.vector(weight)) {
        stop('Kindly insert a range of vector or single numeric value as weight parameter.')
        } else {
          if(is.vector(weight)) wt$weight <- weight
        }
    }
  
  ## Re-categorise the soccer financial settlement date. Due to I have no the 
  ##   history matches dataset from bookmakers. The scrapped spbo time is not 
  ##   stable (always change, moreover there just an information website) where 
  ##   firm A is the firm who placed bets with millions HKD (although the 
  ##   kick-off time might also changed after placed that particular bet), 
  ##   therefore I follow the kick-off time of the firm A.
  mbase <- mbase[order(mbase$DateUK),] %>% mutate(
    DateUS = as.Date(format(DateUK, tz = 'EST', usetz = TRUE, 
                            format = '%Y-%m-%d %H:%M:%S')))
  
  ## ====================== Kelly weight 1 prob ===========================
  ## The weight.stakes and weight parameters will be equal to 1 if there has no 
  ##   any value insert or insert as 1.
  
  ## Weight for probabilities and weight.stakes for stakes adjustment both 
  ##   applied within fraction.
  ## 
  K1 <- mbase %>% select(DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                         Result, Return, PL, rRates, netEMEdge, netProbB, 
                         rEMProbB) %>% cbind(wt) %>% select(-No)
  
  K1 %<>% mutate(
    PropHKPriceEdge = ((weight * Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
    
    PropnetProbBEdge = ((weight * Stakes * netEMEdge) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
    
    KProbHKPrice = (weight * (Stakes) * HKPrice + 1) / EUPrice, 
    
    KProbnetProbB = (weight * (Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
    
    KProbFixed = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
    
    KProbFixednetProbB = exp(((log(weight * Stakes)) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)),
    
    KEMProb = (weight * rEMProbB * HKPrice + 1) / EUPrice, 
    
    KEMProbnetProbB = (weight * rEMProbB *  (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
    
    KProbHalf = (weight * (0.5 * Stakes) * HKPrice + 1) / EUPrice, 
    
    KProbHalfnetProbB = (weight * (0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
    
    KProbQuarter = (weight * (0.25 * Stakes) * HKPrice + 1) / EUPrice, 
    
    KProbQuarternetProbB = (weight * (0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
    
    KProbAdj = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
    
    KProbAdjnetProbB = exp((log(weight * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
    
    KHalfAdj = exp((log(weight * 0.5 * Stakes) * HKPrice + 1) / EUPrice), 
    
    KHalfAdjnetProbB = exp((log(weight * 0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
    
    KEMQuarterAdj = exp((log(weight * 0.25 * Stakes) * HKPrice + 1) / EUPrice), 
    
    KEMQuarterAdjnetProbB = exp((log(weight * 0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 

    ## --------------------- Kelly weight 1 stakes ---------------------------------
    KStakesHKPriceEdge = ((weight.stakes * PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
    
    KStakesnetProbBEdge = ((weight.stakes * PropnetProbBEdge * netEMEdge) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
    
    KStakesHKPrice = ((weight.stakes * KProbHKPrice) * EUPrice - 1) / HKPrice, 
    
    KStakesnetProbB = ((weight.stakes * KProbnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
    
    KStakesFixed = exp((log(weight.stakes * KProbFixed) * EUPrice - 1) / HKPrice), 
    
    KStakesFixednetProbB = exp(((log(weight.stakes * KProbFixednetProbB)) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)),
    
    KStakesEMProb = (weight.stakes * KEMProb * EUPrice - 1) / HKPrice, 
    
    KStakesEMProbnetProbB = (weight.stakes * KEMProbnetProbB *  (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
    
    KStakesHalf = ((weight.stakes * 0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
    
    KStakesHalfnetProbB = (weight.stakes * (0.5 * KProbHalfnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
    
    KStakesQuarter = (weight.stakes * (0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
    
    KStakesQuarternetProbB = ((0.25 * KProbQuarternetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
    
    KStakesAdj = exp((log(weight.stakes * KProbAdj) * EUPrice - 1) / HKPrice), 
    
    KStakesAdjnetProbB = exp((log(weight.stakes * KProbAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
    
    KStakesHalfAdj = exp((log(weight.stakes * 0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
    
    KStakesHalfAdjnetProbB = exp((log(weight.stakes * 0.5 * KHalfAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
    
    KStakesEMQuarterAdj = exp((log(weight.stakes * 0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
    
    KStakesEMQuarterAdjnetProbB = exp((log(weight.stakes * 0.25 * KEMQuarterAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
    
    ## --------------------- Kelly weight 1 return ---------------------------------
    KReturnHKPriceEdge = ifelse(Result == 'Win', KStakesHKPriceEdge * EUPrice, ifelse(Result == 'Half Win', KStakesHKPriceEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPriceEdge, ifelse(Result == 'Half Loss', KStakesHKPriceEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))),  
    
    KReturnnetProbBEdge = ifelse(Result == 'Win', KStakesnetProbBEdge * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbBEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbBEdge, ifelse(Result == 'Half Loss', KStakesnetProbBEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
    
    KReturnHKPrice = ifelse(Result == 'Win', KStakesHKPrice * EUPrice, ifelse(Result == 'Half Win', KStakesHKPrice * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPrice, ifelse(Result == 'Half Loss', KStakesHKPrice * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnnetProbB = ifelse(Result == 'Win', KStakesnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbB, ifelse(Result == 'Half Loss', KStakesnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnFixed = ifelse(Result == 'Win', KStakesFixed * EUPrice, ifelse(Result == 'Half Win', KStakesFixed * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixed, ifelse(Result == 'Half Loss', KStakesFixed * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
    
    KReturnFixednetProbB = ifelse(Result == 'Win', KStakesFixednetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesFixednetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixednetProbB, ifelse(Result == 'Half Loss', KStakesFixednetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
    
    KReturnEMProb = ifelse(Result == 'Win', KStakesEMProb * EUPrice, ifelse(Result == 'Half Win', KStakesEMProb * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProb, ifelse(Result == 'Half Loss', KStakesEMProb * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
    
    KReturnEMProbnetProbB = ifelse(Result == 'Win', KStakesEMProbnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProbnetProbB, ifelse(Result == 'Half Loss', KStakesEMProbnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),

    KReturnHalf = ifelse(Result == 'Win', KStakesHalf * EUPrice, ifelse(Result == 'Half Win', KStakesHalf * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalf, ifelse(Result == 'Half Loss', KStakesHalf * 0.5, ifelse(Result == 'Loss', 0, NA))))),

    KReturnHalfnetProbB = ifelse(Result == 'Win', KStakesHalfnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfnetProbB, ifelse(Result == 'Half Loss', KStakesHalfnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),

    KReturnQuarter = ifelse(Result == 'Win', KStakesQuarter * EUPrice, ifelse(Result == 'Half Win', KStakesQuarter * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarter, ifelse(Result == 'Half Loss', KStakesQuarter * 0.5, ifelse(Result == 'Loss', 0, NA))))),

    KReturnQuarternetProbB = ifelse(Result == 'Win', KStakesQuarternetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesQuarternetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarternetProbB, ifelse(Result == 'Half Loss', KStakesQuarternetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),

    KReturnAdj = ifelse(Result == 'Win', KStakesAdj * EUPrice, ifelse(Result == 'Half Win', KStakesAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdj, ifelse(Result == 'Half Loss', KStakesAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnAdjnetProbB = ifelse(Result == 'Win', KStakesAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdjnetProbB, ifelse(Result == 'Half Loss', KStakesAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnHalfAdj = ifelse(Result == 'Win', KStakesHalfAdj * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdj, ifelse(Result == 'Half Loss', KStakesHalfAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnHalfAdjnetProbB = ifelse(Result == 'Win', KStakesHalfAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdjnetProbB, ifelse(Result == 'Half Loss', KStakesHalfAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnEMQuarterAdj = ifelse(Result == 'Win', KStakesEMQuarterAdj * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdj, ifelse(Result == 'Half Loss', KStakesEMQuarterAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
    
    KReturnEMQuarterAdjnetProbB = ifelse(Result == 'Win', KStakesEMQuarterAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdjnetProbB, ifelse(Result == 'Half Loss', KStakesEMQuarterAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA)))))
    
    ) %>% tbl_df
  
  ## ==================== Kelly weight 2 prob ========================================
  ## The models will be execute only when all weight.stakes and all weight 
  ##   parameters or vectors value not equal to 1.
  ## 
  
  ## Weight for probabilities applied within fraction but weight.stakes for stakes 
  ##   adjustment applied outside the fraction.
  ## 
  if(all(wt$weight.stakes) != 1 & all(wt$weight) != 1) {
    K2 <- mbase %>% select(DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, Result, Return, PL, 
                           rRates, netEMEdge, netProbB, rEMProbB) %>% cbind(wt) %>% select(-No)
    
    K2 %<>% mutate(
      PropHKPriceEdge = ((weight * Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
      
      PropnetProbBEdge = ((weight * Stakes * netEMEdge) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbHKPrice = (weight * (Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbnetProbB = (weight * (Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbFixed = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbFixednetProbB = exp(((log(weight * Stakes)) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)),
      
      KEMProb = (weight * rEMProbB * HKPrice + 1) / EUPrice, 
      
      KEMProbnetProbB = (weight * rEMProbB *  (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbHalf = (weight * (0.5 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbHalfnetProbB = (weight * (0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
      
      KProbQuarter = (weight * (0.25 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbQuarternetProbB = (weight * (0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
      
      KProbAdj = exp((log(weight * Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbAdjnetProbB = exp((log(weight * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      KHalfAdj = exp((log(weight * 0.5 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KHalfAdjnetProbB = exp((log(weight * 0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      KEMQuarterAdj = exp((log(weight * 0.25 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KEMQuarterAdjnetProbB = exp((log(weight * 0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      ## --------------------- Kelly weight 2 stakes ---------------------------------
      KStakesHKPriceEdge = weight.stakes * ((PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
      
      KStakesnetProbBEdge = weight.stakes * ((PropnetProbBEdge * netEMEdge) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesHKPrice = weight.stakes * ((KProbHKPrice) * EUPrice - 1) / HKPrice, 
      
      KStakesnetProbB = weight.stakes * ((KProbnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesFixed = weight.stakes * exp((log(KProbFixed) * EUPrice - 1) / HKPrice), 
      
      KStakesFixednetProbB = weight.stakes * exp(((log(KProbFixednetProbB)) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)),
      
      KStakesEMProb = weight.stakes * (KEMProb * EUPrice - 1) / HKPrice, 
      
      KStakesEMProbnetProbB = weight.stakes * (KEMProbnetProbB *  (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesHalf = weight.stakes * ((0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
      
      KStakesHalfnetProbB = weight.stakes * ((0.5 * KProbHalfnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
      
      KStakesQuarter = weight.stakes * ((0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
      
      KStakesQuarternetProbB = weight.stakes * ((0.25 * KProbQuarternetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
      
      KStakesAdj = weight.stakes * exp((log(KProbAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesAdjnetProbB = weight.stakes * exp((log(KProbAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      KStakesHalfAdj = weight.stakes * exp((log(0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesHalfAdjnetProbB = weight.stakes * exp((log(0.5 * KHalfAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      KStakesEMQuarterAdj = weight.stakes * exp((log(0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesEMQuarterAdjnetProbB = weight.stakes * exp((log(0.25 * KEMQuarterAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      ## --------------------- Kelly weight 2 return ---------------------------------
      KReturnHKPriceEdge = ifelse(Result == 'Win', KStakesHKPriceEdge * EUPrice, ifelse(Result == 'Half Win', KStakesHKPriceEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPriceEdge, ifelse(Result == 'Half Loss', KStakesHKPriceEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))),  
      
      KReturnnetProbBEdge = ifelse(Result == 'Win', KStakesnetProbBEdge * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbBEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbBEdge, ifelse(Result == 'Half Loss', KStakesnetProbBEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnHKPrice = ifelse(Result == 'Win', KStakesHKPrice * EUPrice, ifelse(Result == 'Half Win', KStakesHKPrice * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPrice, ifelse(Result == 'Half Loss', KStakesHKPrice * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnnetProbB = ifelse(Result == 'Win', KStakesnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbB, ifelse(Result == 'Half Loss', KStakesnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnFixed = ifelse(Result == 'Win', KStakesFixed * EUPrice, ifelse(Result == 'Half Win', KStakesFixed * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixed, ifelse(Result == 'Half Loss', KStakesFixed * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnFixednetProbB = ifelse(Result == 'Win', KStakesFixednetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesFixednetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixednetProbB, ifelse(Result == 'Half Loss', KStakesFixednetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProb = ifelse(Result == 'Win', KStakesEMProb * EUPrice, ifelse(Result == 'Half Win', KStakesEMProb * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProb, ifelse(Result == 'Half Loss', KStakesEMProb * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProbnetProbB = ifelse(Result == 'Win', KStakesEMProbnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProbnetProbB, ifelse(Result == 'Half Loss', KStakesEMProbnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalf = ifelse(Result == 'Win', KStakesHalf * EUPrice, ifelse(Result == 'Half Win', KStakesHalf * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalf, ifelse(Result == 'Half Loss', KStakesHalf * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfnetProbB = ifelse(Result == 'Win', KStakesHalfnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfnetProbB, ifelse(Result == 'Half Loss', KStakesHalfnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarter = ifelse(Result == 'Win', KStakesQuarter * EUPrice, ifelse(Result == 'Half Win', KStakesQuarter * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarter, ifelse(Result == 'Half Loss', KStakesQuarter * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarternetProbB = ifelse(Result == 'Win', KStakesQuarternetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesQuarternetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarternetProbB, ifelse(Result == 'Half Loss', KStakesQuarternetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdj = ifelse(Result == 'Win', KStakesAdj * EUPrice, ifelse(Result == 'Half Win', KStakesAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdj, ifelse(Result == 'Half Loss', KStakesAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdjnetProbB = ifelse(Result == 'Win', KStakesAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdjnetProbB, ifelse(Result == 'Half Loss', KStakesAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdj = ifelse(Result == 'Win', KStakesHalfAdj * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdj, ifelse(Result == 'Half Loss', KStakesHalfAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdjnetProbB = ifelse(Result == 'Win', KStakesHalfAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdjnetProbB, ifelse(Result == 'Half Loss', KStakesHalfAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdj = ifelse(Result == 'Win', KStakesEMQuarterAdj * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdj, ifelse(Result == 'Half Loss', KStakesEMQuarterAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdjnetProbB = ifelse(Result == 'Win', KStakesEMQuarterAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdjnetProbB, ifelse(Result == 'Half Loss', KStakesEMQuarterAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA)))))
      
    ) %>% tbl_df

    ## ==================== Kelly weight 3 prob ========================================
    ## The models will be execute only when all weight.stakes and all weight 
    ##   parameters or vectors value not equal to 1.
    ## 

    ## Weight for probabilities applied outside the fraction but weight.stakes for 
    ## stakes adjustment applied within the fraction.
    ## 
    K3 <- mbase %>% select(DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, Result, Return, PL, 
                           rRates, netEMEdge, netProbB, rEMProbB) %>% cbind(wt) %>% select(-No)
    
    K3 %<>% mutate(
      PropHKPriceEdge = weight * ((Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
      
      PropnetProbBEdge = weight * ((Stakes * netEMEdge) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbHKPrice = weight * ((Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbnetProbB = weight * ((Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbFixed = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbFixednetProbB = weight * exp(((log(Stakes)) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)),
      
      KEMProb = weight * (rEMProbB * HKPrice + 1) / EUPrice, 
      
      KEMProbnetProbB = weight * (rEMProbB *  (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbHalf = weight * ((0.5 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbHalfnetProbB = weight * ((0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
      
      KProbQuarter = weight * ((0.25 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbQuarternetProbB = weight * ((0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
      
      KProbAdj = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbAdjnetProbB = weight * exp((log(Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      KHalfAdj = weight * exp((log(0.5 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KHalfAdjnetProbB = weight * exp((log(0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      KEMQuarterAdj = weight * exp((log(0.25 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KEMQuarterAdjnetProbB = weight * exp((log(0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      ## --------------------- Kelly weight 3 stakes ---------------------------------
      KStakesHKPriceEdge = ((weight.stakes * PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
      
      KStakesnetProbBEdge = ((weight.stakes * PropnetProbBEdge * netEMEdge) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesHKPrice = ((weight.stakes * KProbHKPrice) * EUPrice - 1) / HKPrice, 
      
      KStakesnetProbB = ((weight.stakes * KProbnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesFixed = exp((log(weight.stakes * KProbFixed) * EUPrice - 1) / HKPrice), 
      
      KStakesFixednetProbB = exp(((log(weight.stakes * KProbFixednetProbB)) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)),
      
      KStakesEMProb = (weight.stakes * KEMProb * EUPrice - 1) / HKPrice, 
      
      KStakesEMProbnetProbB = (weight.stakes * KEMProbnetProbB *  (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesHalf = (weight.stakes * (0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
      
      KStakesHalfnetProbB = (weight.stakes * (0.5 * KProbHalfnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
      
      KStakesQuarter = (weight.stakes * (0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
      
      KStakesQuarternetProbB = (weight.stakes * (0.25 * KProbQuarternetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
      
      KStakesAdj = exp((log(weight.stakes * KProbAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesAdjnetProbB = exp((log(weight.stakes * KProbAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      KStakesHalfAdj = exp((log(weight.stakes * 0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesHalfAdjnetProbB = exp((log(weight.stakes * 0.5 * KHalfAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      KStakesEMQuarterAdj = exp((log(weight.stakes * 0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesEMQuarterAdjnetProbB = exp((log(weight.stakes * 0.25 * KEMQuarterAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      ## --------------------- Kelly weight 3 return ---------------------------------
      KReturnHKPriceEdge = ifelse(Result == 'Win', KStakesHKPriceEdge * EUPrice, ifelse(Result == 'Half Win', KStakesHKPriceEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPriceEdge, ifelse(Result == 'Half Loss', KStakesHKPriceEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))),  
      
      KReturnnetProbBEdge = ifelse(Result == 'Win', KStakesnetProbBEdge * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbBEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbBEdge, ifelse(Result == 'Half Loss', KStakesnetProbBEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnHKPrice = ifelse(Result == 'Win', KStakesHKPrice * EUPrice, ifelse(Result == 'Half Win', KStakesHKPrice * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPrice, ifelse(Result == 'Half Loss', KStakesHKPrice * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnnetProbB = ifelse(Result == 'Win', KStakesnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbB, ifelse(Result == 'Half Loss', KStakesnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnFixed = ifelse(Result == 'Win', KStakesFixed * EUPrice, ifelse(Result == 'Half Win', KStakesFixed * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixed, ifelse(Result == 'Half Loss', KStakesFixed * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnFixednetProbB = ifelse(Result == 'Win', KStakesFixednetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesFixednetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixednetProbB, ifelse(Result == 'Half Loss', KStakesFixednetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProb = ifelse(Result == 'Win', KStakesEMProb * EUPrice, ifelse(Result == 'Half Win', KStakesEMProb * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProb, ifelse(Result == 'Half Loss', KStakesEMProb * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProbnetProbB = ifelse(Result == 'Win', KStakesEMProbnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProbnetProbB, ifelse(Result == 'Half Loss', KStakesEMProbnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalf = ifelse(Result == 'Win', KStakesHalf * EUPrice, ifelse(Result == 'Half Win', KStakesHalf * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalf, ifelse(Result == 'Half Loss', KStakesHalf * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfnetProbB = ifelse(Result == 'Win', KStakesHalfnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfnetProbB, ifelse(Result == 'Half Loss', KStakesHalfnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarter = ifelse(Result == 'Win', KStakesQuarter * EUPrice, ifelse(Result == 'Half Win', KStakesQuarter * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarter, ifelse(Result == 'Half Loss', KStakesQuarter * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarternetProbB = ifelse(Result == 'Win', KStakesQuarternetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesQuarternetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarternetProbB, ifelse(Result == 'Half Loss', KStakesQuarternetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdj = ifelse(Result == 'Win', KStakesAdj * EUPrice, ifelse(Result == 'Half Win', KStakesAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdj, ifelse(Result == 'Half Loss', KStakesAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdjnetProbB = ifelse(Result == 'Win', KStakesAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdjnetProbB, ifelse(Result == 'Half Loss', KStakesAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdj = ifelse(Result == 'Win', KStakesHalfAdj * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdj, ifelse(Result == 'Half Loss', KStakesHalfAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdjnetProbB = ifelse(Result == 'Win', KStakesHalfAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdjnetProbB, ifelse(Result == 'Half Loss', KStakesHalfAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdj = ifelse(Result == 'Win', KStakesEMQuarterAdj * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdj, ifelse(Result == 'Half Loss', KStakesEMQuarterAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdjnetProbB = ifelse(Result == 'Win', KStakesEMQuarterAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdjnetProbB, ifelse(Result == 'Half Loss', KStakesEMQuarterAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA)))))
      
    ) %>% tbl_df
    
    ## ==================== Kelly weight 4 prob ========================================
    ## The models will be execute only when all weight.stakes and all weight 
    ##   parameters or vectors value not equal to 1.
    ## 
    
    ## Weight for probabilities and weight.stakes for stakes adjustment both applied 
    ##   outside the fraction.
    ## 
    K4 <- mbase %>% select(DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, Result, Return, PL, 
                           rRates, netEMEdge, netProbB, rEMProbB) %>% cbind(wt) %>% select(-No)
    
    K4 %<>% mutate(
      PropHKPriceEdge = weight * ((Stakes * netEMEdge) * HKPrice + 1) / EUPrice,
      
      PropnetProbBEdge = weight * ((Stakes * netEMEdge) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbHKPrice = weight * ((Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbnetProbB = weight * ((Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbFixed = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbFixednetProbB = weight * exp(((log(Stakes)) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)),
      
      KEMProb = weight * (rEMProbB * HKPrice + 1) / EUPrice, 
      
      KEMProbnetProbB = weight * (rEMProbB *  (1 / rEMProbB - 1) + 1) / (1 / rEMProbB),
      
      KProbHalf = weight * ((0.5 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbHalfnetProbB = weight * ((0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
      
      KProbQuarter = weight * ((0.25 * Stakes) * HKPrice + 1) / EUPrice, 
      
      KProbQuarternetProbB = weight * ((0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB), 
      
      KProbAdj = weight * exp((log(Stakes) * HKPrice + 1) / EUPrice), 
      
      KProbAdjnetProbB = weight * exp((log(Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      KHalfAdj = weight * exp((log(0.5 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KHalfAdjnetProbB = weight * exp((log(0.5 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      KEMQuarterAdj = weight * exp((log(0.25 * Stakes) * HKPrice + 1) / EUPrice), 
      
      KEMQuarterAdjnetProbB = weight * exp((log(0.25 * Stakes) * (1 / rEMProbB - 1) + 1) / (1 / rEMProbB)), 
      
      ## --------------------- Kelly weight 4 stakes ---------------------------------
      KStakesHKPriceEdge = weight.stakes * ((PropHKPriceEdge * netEMEdge) * EUPrice - 1) / HKPrice,
      
      KStakesnetProbBEdge = weight.stakes * ((PropnetProbBEdge * netEMEdge) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesHKPrice = weight.stakes * ((KProbHKPrice) * EUPrice - 1) / HKPrice, 
      
      KStakesnetProbB = weight.stakes * ((KProbnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesFixed = weight.stakes * exp((log(KProbFixed) * EUPrice - 1) / HKPrice), 
      
      KStakesFixednetProbB = weight.stakes * exp(((log(KProbFixednetProbB)) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)),
      
      KStakesEMProb = weight.stakes * (KEMProb * EUPrice - 1) / HKPrice, 
      
      KStakesEMProbnetProbB = weight.stakes * (KEMProbnetProbB *  (1 / rEMProbB) - 1) / (1 / rEMProbB - 1),
      
      KStakesHalf = weight.stakes * ((0.5 * KProbHalf) * EUPrice - 1) / HKPrice, 
      
      KStakesHalfnetProbB = weight.stakes * ((0.5 * KProbHalfnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
      
      KStakesQuarter = weight.stakes * ((0.25 * KProbQuarter) * EUPrice - 1) / HKPrice, 
      
      KStakesQuarternetProbB = weight.stakes * ((0.25 * KProbQuarternetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1), 
      
      KStakesAdj = weight.stakes * exp((log(KProbAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesAdjnetProbB = weight.stakes * exp((log(KProbAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      KStakesHalfAdj = weight.stakes * exp((log(0.5 * KHalfAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesHalfAdjnetProbB = weight.stakes * exp((log(0.5 * KHalfAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      KStakesEMQuarterAdj = weight.stakes * exp((log(0.25 * KEMQuarterAdj) * EUPrice - 1) / HKPrice), 
      
      KStakesEMQuarterAdjnetProbB = weight.stakes * exp((log(0.25 * KEMQuarterAdjnetProbB) * (1 / rEMProbB) - 1) / (1 / rEMProbB - 1)), 
      
      ## --------------------- Kelly weight 4 return ---------------------------------
      KReturnHKPriceEdge = ifelse(Result == 'Win', KStakesHKPriceEdge * EUPrice, ifelse(Result == 'Half Win', KStakesHKPriceEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPriceEdge, ifelse(Result == 'Half Loss', KStakesHKPriceEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))),  
      
      KReturnnetProbBEdge = ifelse(Result == 'Win', KStakesnetProbBEdge * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbBEdge * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbBEdge, ifelse(Result == 'Half Loss', KStakesnetProbBEdge * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnHKPrice = ifelse(Result == 'Win', KStakesHKPrice * EUPrice, ifelse(Result == 'Half Win', KStakesHKPrice * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPrice, ifelse(Result == 'Half Loss', KStakesHKPrice * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnnetProbB = ifelse(Result == 'Win', KStakesnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbB, ifelse(Result == 'Half Loss', KStakesnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnFixed = ifelse(Result == 'Win', KStakesFixed * EUPrice, ifelse(Result == 'Half Win', KStakesFixed * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixed, ifelse(Result == 'Half Loss', KStakesFixed * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnFixednetProbB = ifelse(Result == 'Win', KStakesFixednetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesFixednetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixednetProbB, ifelse(Result == 'Half Loss', KStakesFixednetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProb = ifelse(Result == 'Win', KStakesEMProb * EUPrice, ifelse(Result == 'Half Win', KStakesEMProb * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProb, ifelse(Result == 'Half Loss', KStakesEMProb * 0.5, ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProbnetProbB = ifelse(Result == 'Win', KStakesEMProbnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProbnetProbB, ifelse(Result == 'Half Loss', KStakesEMProbnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalf = ifelse(Result == 'Win', KStakesHalf * EUPrice, ifelse(Result == 'Half Win', KStakesHalf * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalf, ifelse(Result == 'Half Loss', KStakesHalf * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfnetProbB = ifelse(Result == 'Win', KStakesHalfnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfnetProbB, ifelse(Result == 'Half Loss', KStakesHalfnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarter = ifelse(Result == 'Win', KStakesQuarter * EUPrice, ifelse(Result == 'Half Win', KStakesQuarter * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarter, ifelse(Result == 'Half Loss', KStakesQuarter * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarternetProbB = ifelse(Result == 'Win', KStakesQuarternetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesQuarternetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarternetProbB, ifelse(Result == 'Half Loss', KStakesQuarternetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdj = ifelse(Result == 'Win', KStakesAdj * EUPrice, ifelse(Result == 'Half Win', KStakesAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdj, ifelse(Result == 'Half Loss', KStakesAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdjnetProbB = ifelse(Result == 'Win', KStakesAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdjnetProbB, ifelse(Result == 'Half Loss', KStakesAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdj = ifelse(Result == 'Win', KStakesHalfAdj * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdj, ifelse(Result == 'Half Loss', KStakesHalfAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdjnetProbB = ifelse(Result == 'Win', KStakesHalfAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdjnetProbB, ifelse(Result == 'Half Loss', KStakesHalfAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdj = ifelse(Result == 'Win', KStakesEMQuarterAdj * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdj, ifelse(Result == 'Half Loss', KStakesEMQuarterAdj * 0.5, ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdjnetProbB = ifelse(Result == 'Win', KStakesEMQuarterAdjnetProbB * EUPrice, ifelse(Result == 'Half Win', KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdjnetProbB, ifelse(Result == 'Half Loss', KStakesEMQuarterAdjnetProbB * 0.5, ifelse(Result == 'Loss', 0, NA)))))
      
    ) %>% tbl_df
  }
  
  ## ==================== Return function ========================================
  if(all(wt$weight.stakes) != 1 & all(wt$weight) != 1) {
    tmp <- list(data = mbase, Kelly1 = K1, Kelly2 = K2, Kelly3 = K3, Kelly4 = K4, weight.stakes = weight.stakes, weight = weight)
    return(tmp)
    
  } else {
    tmp <- list(data = mbase, Kelly1 = K1, weight.stakes = weight.stakes, weight = weight)
    return(tmp)
  }
}

