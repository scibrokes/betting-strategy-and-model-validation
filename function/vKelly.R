vKelly <- function(mbase, weight.stakes = 1, weight = 1) {
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
  
  ###############################################################################
  ## Due to the I need to use mean value of reversed probability "rEMProb" as the \pho,
  ##   therefore need to skip the probabilities section and use stakes section.
  ##   Reverse probabilities from stakes via different fractional Kelly models might 
  ##   cause the bias to reverse the real probabiliies similar to rEMProb (due to 
  ##   rEMProb has already the mean value).
  ##   Here I rewrote vKelly2() which skip the prob section and use rEMProb in 
  ##  staking section.
  ###############################################################################
  
  ## --------------------- Load packages ----------------------------------------
  options(warn = -1)
  
  suppressMessages(library('formattable'))
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  
  ## --------------------- Convert probabilities -------------------------------- 
  m <- ddply(mbase, .(Sess), summarise, rRates = percent(mean(rRates))) %>% tbl_df
  
  if(any(!c('rRates', 'rEMProbB', 'rEMProbL', 'netEMEdge') %in% names(mbase))){
    mbase$rRates <- rep(as.numeric(m$rRates + 1), ddply(mbase, .(Sess), summarise, 
                                                        n = length(Sess))$n)
    mbase %<>% mutate(rEMProbB = rRates * netProbB, rEMProbL = 1 - rEMProbB, 
                      netEMEdge = rEMProbB / netProbB)
  }
  
  ## --------------------- Data validation --------------------------------------
  if(!is.data.frame(mbase)) {
    stop('Kindly apply the readfirmData() and arrfirmData() in order to turn the data into a fittable data frame.')
  }
  
  if(mbase$rEMProbB <= 0) stop('Invalid rEMProbB value, probabilities must be greater than 0 due to odds price cannot be 0.')
  if(mbase$rEMProbL <= 0) stop('Invalid rEMProbL value, probabilities must be greater than 0 due to odds price cannot be 0.')
  if(mbase$netProbB <= 0) stop('Invalid netProbB value, probabilities must be greater than 0 due to odds price cannot be 0.')
  if(mbase$netProbL <= 0) stop('Invalid netProbL value, probabilities must be greater than 0 due to odds price cannot be 0.')
  if(mbase$HKPrice <= 0) stop('Invalid HKPrice value, probabilities must be greater than 0 due to odds price cannot be 0.')
  if(mbase$EUPrice <= 1) stop('Invalid EUPrice value, probabilities must be greater than 0 due to odds price cannot be 0.')
  
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
    TimeUS = format(DateUK, tz = 'EST', usetz = TRUE, 
                    format = '%Y-%m-%d %H:%M:%S'), 
    DateUS = as.Date(TimeUS), PL.R = PL / Stakes, RebatesS = Stakes * Rebates)
  
  ## ====================== Kelly weight 1 prob ===========================
  ## The weight.stakes and weight parameters will be equal to 1 if there has no 
  ##   any value insert or insert as 1.
  
  ## Weight for probabilities and weight.stakes for stakes adjustment both 
  ##   applied within fraction.
  ## 
  K1 <- mbase %>% select(TimeUS, DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                         Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, netProbB, 
                         rEMProbB) %>% cbind(wt) %>% select(-No)
  
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
    
    KStakesQuarternetProbB = ((0.25 * KProbQuarternetProbB) * (1 / netProbB) - 1) / (1 / netProbB - 1), 
    
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
  if(all(wt$weight.stakes) != 1 & all(wt$weight) != 1) {
    K2 <- mbase %>% select(TimeUS, DateUS, Sess, League, Stakes, HCap, HKPrice, EUPrice, 
                           Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, netProbB, 
                           rEMProbB) %>% cbind(wt) %>% select(-No)
    
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
                           Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, netProbB, 
                           rEMProbB) %>% cbind(wt) %>% select(-No)
    
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
                           Result, Return, PL, PL.R, Rebates, RebatesS, rRates, netEMEdge, netProbB, 
                           rEMProbB) %>% cbind(wt) %>% select(-No)
    
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
  
  KellyPL <- function(K) {
    options(warn = -1)
    
    K %<>% mutate(
      KReturnHKPriceEdge = ifelse(Result == 'Win', KStakesHKPriceEdge * EUPrice, 
                           ifelse(Result == 'Half Win', KStakesHKPriceEdge * (HKPrice * 0.5 + 1), 
                           ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPriceEdge, 
                           ifelse(Result == 'Half Loss', KStakesHKPriceEdge * 0.5, 
                           ifelse(Result == 'Loss', 0, NA))))),  
      
      KReturnnetProbBEdge = ifelse(Result == 'Win', KStakesnetProbBEdge * EUPrice, 
                            ifelse(Result == 'Half Win', KStakesnetProbBEdge * (HKPrice * 0.5 + 1), 
                            ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbBEdge, 
                            ifelse(Result == 'Half Loss', KStakesnetProbBEdge * 0.5, 
                            ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnHKPrice = ifelse(Result == 'Win', KStakesHKPrice * EUPrice, 
                       ifelse(Result == 'Half Win', KStakesHKPrice * (HKPrice * 0.5 + 1), 
                       ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPrice, 
                       ifelse(Result == 'Half Loss', KStakesHKPrice * 0.5, 
                       ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnnetProbB = ifelse(Result == 'Win', KStakesnetProbB * EUPrice, 
                        ifelse(Result == 'Half Win', KStakesnetProbB * (HKPrice * 0.5 + 1), 
                        ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbB, 
                        ifelse(Result == 'Half Loss', KStakesnetProbB * 0.5, 
                        ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnFixed = ifelse(Result == 'Win', KStakesFixed * EUPrice, 
                     ifelse(Result == 'Half Win', KStakesFixed * (HKPrice * 0.5 + 1), 
                     ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixed, 
                     ifelse(Result == 'Half Loss', KStakesFixed * 0.5, 
                     ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnFixednetProbB = ifelse(Result == 'Win', KStakesFixednetProbB * EUPrice, 
                             ifelse(Result == 'Half Win', KStakesFixednetProbB * (HKPrice * 0.5 + 1), 
                             ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixednetProbB, 
                             ifelse(Result == 'Half Loss', KStakesFixednetProbB * 0.5, 
                             ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProb = ifelse(Result == 'Win', KStakesEMProb * EUPrice, 
                      ifelse(Result == 'Half Win', KStakesEMProb * (HKPrice * 0.5 + 1), 
                      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProb, 
                      ifelse(Result == 'Half Loss', KStakesEMProb * 0.5, 
                      ifelse(Result == 'Loss', 0, NA))))), 
      
      KReturnEMProbnetProbB = ifelse(Result == 'Win', KStakesEMProbnetProbB * EUPrice, 
                              ifelse(Result == 'Half Win', KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), 
                              ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProbnetProbB, 
                              ifelse(Result == 'Half Loss', KStakesEMProbnetProbB * 0.5, 
                              ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalf = ifelse(Result == 'Win', KStakesHalf * EUPrice, 
                    ifelse(Result == 'Half Win', KStakesHalf * (HKPrice * 0.5 + 1), 
                    ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalf, 
                    ifelse(Result == 'Half Loss', KStakesHalf * 0.5, 
                    ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfnetProbB = ifelse(Result == 'Win', KStakesHalfnetProbB * EUPrice, 
                            ifelse(Result == 'Half Win', KStakesHalfnetProbB * (HKPrice * 0.5 + 1), 
                            ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfnetProbB, 
                            ifelse(Result == 'Half Loss', KStakesHalfnetProbB * 0.5, 
                            ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarter = ifelse(Result == 'Win', KStakesQuarter * EUPrice, 
                       ifelse(Result == 'Half Win', KStakesQuarter * (HKPrice * 0.5 + 1), 
                       ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarter, 
                       ifelse(Result == 'Half Loss', KStakesQuarter * 0.5, 
                       ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnQuarternetProbB = ifelse(Result == 'Win', KStakesQuarternetProbB * EUPrice, 
                               ifelse(Result == 'Half Win', KStakesQuarternetProbB * (HKPrice * 0.5 + 1), 
                               ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarternetProbB, 
                               ifelse(Result == 'Half Loss', KStakesQuarternetProbB * 0.5, 
                               ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdj = ifelse(Result == 'Win', KStakesAdj * EUPrice, 
                   ifelse(Result == 'Half Win', KStakesAdj * (HKPrice * 0.5 + 1), 
                   ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdj, 
                   ifelse(Result == 'Half Loss', KStakesAdj * 0.5, 
                   ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnAdjnetProbB = ifelse(Result == 'Win', KStakesAdjnetProbB * EUPrice, 
                           ifelse(Result == 'Half Win', KStakesAdjnetProbB * (HKPrice * 0.5 + 1), 
                           ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdjnetProbB, 
                           ifelse(Result == 'Half Loss', KStakesAdjnetProbB * 0.5, 
                           ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdj = ifelse(Result == 'Win', KStakesHalfAdj * EUPrice, 
                       ifelse(Result == 'Half Win', KStakesHalfAdj * (HKPrice * 0.5 + 1), 
                       ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdj, 
                       ifelse(Result == 'Half Loss', KStakesHalfAdj * 0.5, 
                       ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnHalfAdjnetProbB = ifelse(Result == 'Win', KStakesHalfAdjnetProbB * EUPrice, 
                               ifelse(Result == 'Half Win', KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), 
                               ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdjnetProbB, 
                               ifelse(Result == 'Half Loss', KStakesHalfAdjnetProbB * 0.5, 
                               ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdj = ifelse(Result == 'Win', KStakesEMQuarterAdj * EUPrice, 
                            ifelse(Result == 'Half Win', KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), 
                            ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdj, 
                            ifelse(Result == 'Half Loss', KStakesEMQuarterAdj * 0.5, 
                            ifelse(Result == 'Loss', 0, NA))))),
      
      KReturnEMQuarterAdjnetProbB = ifelse(Result == 'Win', KStakesEMQuarterAdjnetProbB * EUPrice, 
                                    ifelse(Result == 'Half Win', KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), 
                                    ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdjnetProbB, 
                                    ifelse(Result == 'Half Loss', KStakesEMQuarterAdjnetProbB * 0.5, 
                                    ifelse(Result == 'Loss', 0, NA)))))
    ) %>% tbl_df
    
    K %<>% mutate(KPLHKPriceEdge = KReturnHKPriceEdge - KStakesHKPriceEdge, 
                  KPLnetProbBEdge = KReturnnetProbBEdge - KStakesnetProbBEdge, 
                  KPLHKPrice = KReturnHKPrice - KStakesHKPrice, 
                  KPLnetProbB = KReturnnetProbB - KStakesnetProbB, 
                  KPLFixed = KReturnFixed - KStakesFixed, 
                  KPLFixednetProbB = KReturnFixednetProbB - KStakesFixednetProbB, 
                  KPLEMProb = KReturnEMProb - KStakesEMProb, 
                  KPLEMProbnetProbB = KReturnEMProbnetProbB - KStakesEMProbnetProbB, 
                  KPLHalf = KReturnHalf - KStakesHalf, 
                  KPLHalfnetProbB = KReturnHalfnetProbB - KStakesHalfnetProbB, 
                  KPLQuarter = KReturnQuarter - KStakesQuarter, 
                  KPLQuarternetProbB = KReturnQuarternetProbB - KStakesQuarternetProbB, 
                  KPLAdj = KReturnAdj - KStakesAdj, 
                  KPLAdjnetProbB = KReturnAdjnetProbB - KStakesAdjnetProbB, 
                  KPLHalfAdj = KReturnHalfAdj - KStakesHalfAdj, 
                  KPLHalfAdjnetProbB = KReturnHalfAdjnetProbB - KStakesHalfAdjnetProbB, 
                  KPLEMQuarterAdj = KReturnEMQuarterAdj - KStakesEMQuarterAdj, 
                  KPLEMQuarterAdjnetProbB = KReturnEMQuarterAdjnetProbB - KStakesEMQuarterAdjnetProbB)
    
    K %<>% mutate(KPLHKPriceEdge.R = percent(KPLHKPriceEdge / KStakesHKPriceEdge), 
                  KPLnetProbBEdge.R = percent(KPLnetProbBEdge / KStakesnetProbBEdge), 
                  KPLHKPrice.R = percent(KPLHKPrice / KStakesHKPrice), 
                  KPLnetProbB.R = percent(KPLnetProbB / KStakesnetProbB), 
                  KPLFixed.R = percent(KPLFixed / KStakesFixed), 
                  KPLFixednetProbB.R = percent(KPLFixednetProbB / KStakesFixednetProbB), 
                  KPLEMProb.R = percent(KPLEMProb / KStakesEMProb), 
                  KPLEMProbnetProbB.R = percent(KPLEMProbnetProbB / KStakesEMProbnetProbB), 
                  KPLHalf.R = percent(KPLHalf / KStakesHalf), 
                  KPLHalfnetProbB.R = percent(KPLHalfnetProbB / KStakesHalfnetProbB), 
                  KPLQuarter.R = percent(KPLQuarter / KStakesQuarter), 
                  KPLQuarternetProbB.R = percent(KPLQuarternetProbB / KStakesQuarternetProbB), 
                  KPLAdj.R = percent(KPLAdj / KStakesAdj), 
                  KPLAdjnetProbB.R = percent(KPLAdjnetProbB / KStakesAdjnetProbB), 
                  KPLHalfAdj.R = percent(KPLHalfAdj / KStakesHalfAdj), 
                  KPLHalfAdjnetProbB.R = percent(KPLHalfAdjnetProbB / KStakesHalfAdjnetProbB), 
                  KPLEMQuarterAdj.R = percent(KPLEMQuarterAdj / KStakesEMQuarterAdj), 
                  KPLEMQuarterAdjnetProbB.R = percent(KPLEMQuarterAdjnetProbB / KStakesEMQuarterAdjnetProbB))
    
    sumd <- K %>% mutate(sm = 1) %>% 
      
      ddply(.(sm), summarise, From = min(TimeUS), To = max(TimeUS), HKPrice = mean(HKPrice), 
            EUPrice = mean(EUPrice), Stakes = sum(Stakes), Return = sum(Return), PL = sum(PL), 
            PL.R = mean(PL.R), Rebates = mean(Rebates), RebatesS = sum(RebatesS), rRates = mean(rRates), 
            netEMEdge = mean(netEMEdge), netProbB = mean(netProbB), rEMProbB = mean(rEMProbB), 
            weight.stakes = mean(weight.stakes), weight = mean(weight), 
            
            PropHKPriceEdge = mean(PropHKPriceEdge), PropnetProbBEdge = mean(PropnetProbBEdge), 
            KProbHKPrice = mean(KProbHKPrice), KProbnetProbB = mean(KProbnetProbB), 
            KProbFixed = mean(KProbFixed), KProbFixednetProbB = mean(KProbFixednetProbB), 
            KEMProb = mean(KEMProb), KEMProbnetProbB = mean(KEMProbnetProbB), 
            KProbHalf = mean(KProbHalf), KProbHalfnetProbB = mean(KProbHalfnetProbB), 
            KProbQuarter = mean(KProbQuarter), KProbQuarternetProbB = mean(KProbQuarternetProbB), 
            KProbAdj = mean(KProbAdj), KProbAdjnetProbB = mean(KProbAdjnetProbB), 
            KHalfAdj = mean(KHalfAdj), KHalfAdjnetProbB = mean(KHalfAdjnetProbB), 
            KEMQuarterAdj = mean(KEMQuarterAdj), KEMQuarterAdjnetProbB = mean(KEMQuarterAdjnetProbB), 
            
            KStakesHKPriceEdge = currency(sum(KStakesHKPriceEdge)), 
            KStakesnetProbBEdge = currency(sum(KStakesnetProbBEdge)), 
            KStakesHKPrice = currency(sum(KStakesHKPrice)), 
            KStakesnetProbB = currency(sum(KStakesnetProbB)), 
            KStakesFixed = currency(sum(KStakesFixed)), 
            KStakesFixednetProbB = currency(sum(KStakesFixednetProbB)), 
            KStakesEMProb = currency(sum(KStakesEMProb)), 
            KStakesEMProbnetProbB = currency(sum(KStakesEMProbnetProbB)), 
            KStakesHalf = currency(sum(KStakesHalf)), 
            KStakesHalfnetProbB = currency(sum(KStakesHalfnetProbB)), 
            KStakesQuarter = currency(sum(KStakesQuarter)), 
            KStakesQuarternetProbB = currency(sum(KStakesQuarternetProbB)), 
            KStakesAdj = currency(sum(KStakesAdj)), 
            KStakesAdjnetProbB = currency(sum(KStakesAdjnetProbB)), 
            KStakesHalfAdj = currency(sum(KStakesHalfAdj)), 
            KStakesHalfAdjnetProbB = currency(sum(KStakesHalfAdjnetProbB)), 
            KStakesEMQuarterAdj = currency(sum(KStakesEMQuarterAdj)), 
            KStakesEMQuarterAdjnetProbB = currency(sum(KStakesEMQuarterAdjnetProbB)), 
            
            KReturnHKPriceEdge = currency(sum(KReturnHKPriceEdge)), 
            KReturnnetProbBEdge = currency(sum(KReturnnetProbBEdge)), 
            KReturnHKPrice = currency(sum(KReturnHKPrice)), 
            KReturnnetProbB = currency(sum(KReturnnetProbB)), 
            KReturnFixed = currency(sum(KReturnFixed)), 
            KReturnFixednetProbB = currency(sum(KReturnFixednetProbB)), 
            KReturnEMProb = currency(sum(KReturnEMProb)), 
            KReturnEMProbnetProbB = currency(sum(KReturnEMProbnetProbB)), 
            KReturnHalf = currency(sum(KReturnHalf)), 
            KReturnHalfnetProbB = currency(sum(KReturnHalfnetProbB)), 
            KReturnQuarter = currency(sum(KReturnQuarter)), 
            KReturnQuarternetProbB = currency(sum(KReturnQuarternetProbB)), 
            KReturnAdj = currency(sum(KReturnAdj)), 
            KReturnAdjnetProbB = currency(sum(KReturnAdjnetProbB)), 
            KReturnHalfAdj = currency(sum(KReturnHalfAdj)), 
            KReturnHalfAdjnetProbB = currency(sum(KReturnHalfAdjnetProbB)), 
            KReturnEMQuarterAdj = currency(sum(KReturnEMQuarterAdj)), 
            KReturnEMQuarterAdjnetProbB = currency(sum(KReturnEMQuarterAdjnetProbB)), 
            
            KPLHKPriceEdge = currency(sum(KPLHKPriceEdge)), 
            KPLnetProbBEdge = currency(sum(KPLnetProbBEdge)), 
            KPLHKPrice = currency(sum(KPLHKPrice)), KPLnetProbB = currency(sum(KPLnetProbB)), 
            KPLFixed = currency(mean(KPLFixed)), 
            KPLFixednetProbB = currency(sum(KPLFixednetProbB)), 
            KPLEMProb = currency(sum(KPLEMProb)), 
            KPLEMProbnetProbB = currency(sum(KPLEMProbnetProbB)), 
            KPLHalf = currency(sum(KPLHalf)), 
            KPLHalfnetProbB = currency(sum(KPLHalfnetProbB)), 
            KPLQuarter = currency(sum(KPLQuarter)), 
            KPLQuarternetProbB = currency(sum(KPLQuarternetProbB)), 
            KPLAdj = currency(sum(KPLAdj)), 
            KPLAdjnetProbB = currency(sum(KPLAdjnetProbB)), 
            KPLHalfAdj = currency(sum(KPLHalfAdj)), 
            KPLHalfAdjnetProbB = currency(sum(KPLHalfAdjnetProbB)), 
            KPLEMQuarterAdj = currency(sum(KPLEMQuarterAdj)), 
            KPLEMQuarterAdjnetProbB = currency(sum(KPLEMQuarterAdjnetProbB))) %>% 
      
      mutate(KPLHKPriceEdge.R = percent(KPLHKPriceEdge / KStakesHKPriceEdge), 
             KPLnetProbBEdge.R = percent(KPLnetProbBEdge / KStakesnetProbBEdge), 
             KPLHKPrice.R = percent(KPLHKPrice / KStakesHKPrice), 
             KPLnetProbB.R = percent(KPLnetProbB / KStakesnetProbB), 
             KPLFixed.R = percent(KPLFixed / KStakesFixed), 
             KPLFixednetProbB.R = percent(KPLFixednetProbB / KStakesFixednetProbB), 
             KPLEMProb.R = percent(KPLEMProb / KStakesEMProb), 
             KPLEMProbnetProbB.R = percent(KPLEMProbnetProbB / KStakesEMProbnetProbB), 
             KPLHalf.R = percent(KPLHalf / KStakesHalf), 
             KPLHalfnetProbB.R = percent(KPLHalfnetProbB / KStakesHalfnetProbB), 
             KPLQuarter.R = percent(KPLQuarter / KStakesQuarter), 
             KPLQuarternetProbB.R = percent(KPLQuarternetProbB / KStakesQuarternetProbB), 
             KPLAdj.R = percent(KPLAdj / KStakesAdj), 
             KPLAdjnetProbB.R = percent(KPLAdjnetProbB / KStakesAdjnetProbB), 
             KPLHalfAdj.R = percent(KPLHalfAdj / KStakesHalfAdj), 
             KPLHalfAdjnetProbB.R = percent(KPLHalfAdjnetProbB / KStakesHalfAdjnetProbB), 
             KPLEMQuarterAdj.R = percent(KPLEMQuarterAdj / KStakesEMQuarterAdj), 
             KPLEMQuarterAdjnetProbB.R = percent(KPLEMQuarterAdjnetProbB / KStakesEMQuarterAdjnetProbB)) %>% 
      
      mutate(KPLHKPriceEdge.R = ifelse(is.nan(KPLHKPriceEdge.R), 0, KPLHKPriceEdge.R), 
             KPLnetProbBEdge.R = ifelse(is.nan(KPLnetProbBEdge.R), 0, KPLnetProbBEdge.R), 
             KPLHKPrice.R = ifelse(is.nan(KPLHKPrice.R), 0, KPLHKPrice.R), 
             KPLnetProbB.R = ifelse(is.nan(KPLnetProbB.R), 0, KPLnetProbB.R), 
             KPLFixed.R = ifelse(is.nan(KPLFixed.R), 0, KPLFixed.R), 
             KPLFixednetProbB.R = ifelse(is.nan(KPLFixednetProbB.R), 0, KPLFixednetProbB.R), 
             KPLEMProb.R = ifelse(is.nan(KPLEMProb.R), 0, KPLEMProb.R), 
             KPLEMProbnetProbB.R = ifelse(is.nan(KPLEMProbnetProbB.R), 0, KPLEMProbnetProbB.R), 
             KPLHalf.R = ifelse(is.nan(KPLHalf.R), 0, KPLHalf.R), 
             KPLHalfnetProbB.R = ifelse(is.nan(KPLHalfnetProbB.R), 0, KPLHalfnetProbB.R), 
             KPLQuarter.R = ifelse(is.nan(KPLQuarter.R), 0, KPLQuarter.R), 
             KPLQuarternetProbB.R = ifelse(is.nan(KPLQuarternetProbB.R), 0, KPLQuarternetProbB.R), 
             KPLAdj.R = ifelse(is.nan(KPLAdj.R), 0, KPLAdj.R), 
             KPLAdjnetProbB.R = ifelse(is.nan(KPLAdjnetProbB.R), 0, KPLAdjnetProbB.R), 
             KPLHalfAdj.R = ifelse(is.nan(KPLHalfAdj.R), 0, KPLHalfAdj.R), 
             KPLHalfAdjnetProbB.R = ifelse(is.nan(KPLHalfAdjnetProbB.R), 0, KPLHalfAdjnetProbB.R), 
             KPLEMQuarterAdj.R = ifelse(is.nan(KPLEMQuarterAdj.R), 0, KPLEMQuarterAdj.R), 
             KPLEMQuarterAdjnetProbB.R = ifelse(is.nan(KPLEMQuarterAdjnetProbB.R), 0, KPLEMQuarterAdjnetProbB.R)) %>% 
      
      mutate(Stakes = currency(Stakes), Return = currency(Return), PL = currency(PL), PL.R = percent(PL.R), 
             Rebates = percent(Rebates), RebatesS = currency(RebatesS)
             
      ) %>% select(-sm) %>% tbl_df
    
    sumdf <- data.frame(
      # From = sumd$From, To = sumd$To, HKPrice = sumd$HKPrice, EUPrice = sumd$EUPrice, 
      # netProbB = sumd$netProbB, rRates = sumd$rRates, weight.stakes = sumd$weight.stakes, 
      # weight = sumd$weight, Rebates = sumd$Rebates, netProbB = sumd$netProbB, 
      # rEMProbB = sumd$rEMProbB, 
      Models = sumd %>% 
        select(netEMEdge, PropHKPriceEdge, PropnetProbBEdge, KProbHKPrice, KProbnetProbB, 
               KProbFixed, KProbFixednetProbB, KEMProb, KEMProbnetProbB, KProbHalf, 
               KProbHalfnetProbB, KProbQuarter, KProbQuarternetProbB, KProbAdj, 
               KProbAdjnetProbB, KHalfAdj, KHalfAdjnetProbB, KEMQuarterAdj, 
               KEMQuarterAdjnetProbB) %>% t, 
      Stakes = sumd %>% 
        select(Stakes, KStakesHKPriceEdge, KStakesnetProbBEdge, KStakesHKPrice, 
               KStakesnetProbB, KStakesFixed, KStakesFixednetProbB, KStakesEMProb, 
               KStakesEMProbnetProbB, KStakesHalf, KStakesHalfnetProbB, KStakesQuarter, 
               KStakesQuarternetProbB, KStakesAdj, KStakesAdjnetProbB, KStakesHalfAdj, 
               KStakesHalfAdjnetProbB, KStakesEMQuarterAdj, KStakesEMQuarterAdjnetProbB) %>% t, 
      Return = sumd %>% 
        select(Return, KReturnHKPriceEdge, KReturnnetProbBEdge, KReturnHKPrice, KReturnnetProbB, 
               KReturnFixed, KReturnFixednetProbB, KReturnEMProb, KReturnEMProbnetProbB, 
               KReturnHalf, KReturnHalfnetProbB, KReturnQuarter, KReturnQuarternetProbB, 
               KReturnAdj, KReturnAdjnetProbB, KReturnHalfAdj, KReturnHalfAdjnetProbB, 
               KReturnEMQuarterAdj, KReturnEMQuarterAdjnetProbB) %>% t, 
      PL = sumd %>% 
        select(PL, KPLHKPriceEdge, KPLnetProbBEdge, KPLHKPrice, KPLnetProbB, KPLFixed, 
               KPLFixednetProbB, KPLEMProb, KPLEMProbnetProbB, KPLHalf, KPLHalfnetProbB, 
               KPLQuarter, KPLQuarternetProbB, KPLAdj, KPLAdjnetProbB, KPLHalfAdj, 
               KPLHalfAdjnetProbB, KPLEMQuarterAdj, KPLEMQuarterAdjnetProbB) %>% t
    ) %>% data.frame(Category = rownames(.), .) %>% tbl_df %>% 
      mutate(Stakes = currency(Stakes), Return = currency(Return), PL = currency(PL), 
             PL.R = percent(ifelse(is.nan(PL / Stakes), 0, PL / Stakes)))
    
    options(warn = 0)
    return(list(data = K, summary = sumdf))
  }
  
  ## ==================== Return function ========================================
  
  if(all(wt$weight.stakes) != 1 & all(wt$weight) != 1) {
    tmp <- list(data = mbase, Kelly1 = KellyPL(K1), Kelly2 = KellyPL(K2), 
                          Kelly3 = KellyPL(K3), Kelly4 = KellyPL(K4), 
                weight.stakes = weight.stakes, weight = weight)
    options(warn = 0)
    return(tmp)
    
  } else {
    tmp <- list(data = mbase, Kelly1 = KellyPL(K1), 
                weight.stakes = weight.stakes, weight = weight)
    options(warn = 0)
    return(tmp)
  }
}

