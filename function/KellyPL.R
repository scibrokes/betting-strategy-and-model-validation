KellyPL <- function(K, adjusted = 1) {
  ## Built-in function inside vKelly() and vKelly2() for sumarise the staking and P&L of 
  ##   various Kelly models.
  options(warn = -1)
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('plyr'))
  suppressMessages(library('formattable'))
  suppressMessages(library('quantmod'))
  suppressMessages(library('doParallel'))
  suppressMessages(library('BBmisc'))
  
  K %<>% mutate(
    KReturnHKPriceEdge = suppressAll(
      ifelse(Result == 'Win', KStakesHKPriceEdge * EUPrice, 
      ifelse(Result == 'Half Win', KStakesHKPriceEdge * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPriceEdge, 
      ifelse(Result == 'Half Loss', KStakesHKPriceEdge * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@        c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@        c(KStakesHKPriceEdge * EUPrice, 
    #'@          KStakesHKPriceEdge * (HKPrice * 0.5 + 1), 
    #'@          KStakesHKPriceEdge, KStakesHKPriceEdge, 
    #'@          KStakesHKPriceEdge * 0.5, 0))), 
    
    KReturnnetProbBEdge = suppressAll(
      ifelse(Result == 'Win', KStakesnetProbBEdge * EUPrice, 
      ifelse(Result == 'Half Win', KStakesnetProbBEdge * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbBEdge, 
      ifelse(Result == 'Half Loss', KStakesnetProbBEdge * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesnetProbBEdge * EUPrice, 
    #'@         KStakesnetProbBEdge * (HKPrice * 0.5 + 1), 
    #'@         KStakesnetProbBEdge, KStakesnetProbBEdge, 
    #'@         KStakesnetProbBEdge * 0.5, 0))), 
    
    KReturnHKPrice = suppressAll(
      ifelse(Result == 'Win', KStakesHKPrice * EUPrice, 
      ifelse(Result == 'Half Win', KStakesHKPrice * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHKPrice, 
      ifelse(Result == 'Half Loss', KStakesHKPrice * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesHKPrice * EUPrice, 
    #'@         KStakesHKPrice * (HKPrice * 0.5 + 1), 
    #'@         KStakesHKPrice, KStakesHKPrice, 
    #'@         KStakesHKPrice * 0.5, 0))), 
    
    KReturnnetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesnetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesnetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesnetProbB, 
      ifelse(Result == 'Half Loss', KStakesnetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesnetProbB * EUPrice, 
    #'@         KStakesnetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesnetProbB, KStakesnetProbB, 
    #'@         KStakesnetProbB * 0.5, 0))), 
    
    KReturnFixed = suppressAll(
      ifelse(Result == 'Win', KStakesFixed * EUPrice, 
      ifelse(Result == 'Half Win', KStakesFixed * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixed, 
      ifelse(Result == 'Half Loss', KStakesFixed * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesFixed * EUPrice, 
    #'@         KStakesFixed * (HKPrice * 0.5 + 1), 
    #'@         KStakesFixed, KStakesFixed, 
    #'@         KStakesFixed * 0.5, 0))), 
    
    KReturnFixednetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesFixednetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesFixednetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesFixednetProbB, 
      ifelse(Result == 'Half Loss', KStakesFixednetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesFixednetProbB * EUPrice, 
    #'@         KStakesFixednetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesFixednetProbB, KStakesFixednetProbB, 
    #'@         KStakesFixednetProbB * 0.5, 0))), 
    
    KReturnEMProb = suppressAll(
      ifelse(Result == 'Win', KStakesEMProb * EUPrice, 
      ifelse(Result == 'Half Win', KStakesEMProb * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProb, 
      ifelse(Result == 'Half Loss', KStakesEMProb * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesEMProb * EUPrice, 
    #'@         KStakesEMProb * (HKPrice * 0.5 + 1), 
    #'@         KStakesEMProb, KStakesEMProb, 
    #'@         KStakesEMProb * 0.5, 0))), 
    
    KReturnEMProbnetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesEMProbnetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMProbnetProbB, 
      ifelse(Result == 'Half Loss', KStakesEMProbnetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesEMProbnetProbB * EUPrice, 
    #'@         KStakesEMProbnetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesEMProbnetProbB, KStakesEMProbnetProbB, 
    #'@         KStakesEMProbnetProbB * 0.5, 0))), 
    
    KReturnHalf = suppressAll(
      ifelse(Result == 'Win', KStakesHalf * EUPrice, 
      ifelse(Result == 'Half Win', KStakesHalf * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalf, 
      ifelse(Result == 'Half Loss', KStakesHalf * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesHalf * EUPrice, 
    #'@         KStakesHalf * (HKPrice * 0.5 + 1), 
    #'@         KStakesHalf, KStakesHalf, 
    #'@         KStakesHalf * 0.5, 0))), 
    
    KReturnHalfnetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesHalfnetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesHalfnetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfnetProbB, 
      ifelse(Result == 'Half Loss', KStakesHalfnetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesHalfnetProbB * EUPrice, 
    #'@         KStakesHalfnetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesHalfnetProbB, KStakesHalfnetProbB, 
    #'@         KStakesHalfnetProbB * 0.5, 0))), 
    
    KReturnQuarter = suppressAll(
      ifelse(Result == 'Win', KStakesQuarter * EUPrice, 
      ifelse(Result == 'Half Win', KStakesQuarter * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarter, 
      ifelse(Result == 'Half Loss', KStakesQuarter * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesQuarter * EUPrice, 
    #'@         KStakesQuarter * (HKPrice * 0.5 + 1), 
    #'@         KStakesQuarter, KStakesQuarter, 
    #'@         KStakesQuarter * 0.5, 0))), 
    
    KReturnQuarternetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesQuarternetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesQuarternetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesQuarternetProbB, 
      ifelse(Result == 'Half Loss', KStakesQuarternetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesQuarternetProbB * EUPrice, 
    #'@         KStakesQuarternetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesQuarternetProbB, KStakesQuarternetProbB, 
    #'@         KStakesQuarternetProbB * 0.5, 0))), 
    
    KReturnAdj = suppressAll(
      ifelse(Result == 'Win', KStakesAdj * EUPrice, 
      ifelse(Result == 'Half Win', KStakesAdj * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdj, 
      ifelse(Result == 'Half Loss', KStakesAdj * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesAdj * EUPrice, 
    #'@         KStakesAdj * (HKPrice * 0.5 + 1), 
    #'@         KStakesAdj, KStakesAdj, 
    #'@         KStakesAdj * 0.5, 0))), 
    
    KReturnAdjnetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesAdjnetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesAdjnetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesAdjnetProbB, 
      ifelse(Result == 'Half Loss', KStakesAdjnetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesAdjnetProbB * EUPrice, 
    #'@         KStakesAdjnetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesAdjnetProbB, KStakesAdjnetProbB, 
    #'@         KStakesAdjnetProbB * 0.5, 0))), 
    
    KReturnHalfAdj = suppressAll(
      ifelse(Result == 'Win', KStakesHalfAdj * EUPrice, 
      ifelse(Result == 'Half Win', KStakesHalfAdj * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdj, 
      ifelse(Result == 'Half Loss', KStakesHalfAdj * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesHalfAdj * EUPrice, 
    #'@         KStakesHalfAdj * (HKPrice * 0.5 + 1), 
    #'@         KStakesHalfAdj, KStakesHalfAdj, 
    #'@         KStakesHalfAdj * 0.5, 0))), 
    
    KReturnHalfAdjnetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesHalfAdjnetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesHalfAdjnetProbB, 
      ifelse(Result == 'Half Loss', KStakesHalfAdjnetProbB * 0.5, 
                                  ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesHalfAdjnetProbB * EUPrice, 
    #'@         KStakesHalfAdjnetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesHalfAdjnetProbB, KStakesHalfAdjnetProbB, 
    #'@         KStakesHalfAdjnetProbB * 0.5, 0))), 
    
    KReturnEMQuarterAdj = suppressAll(
      ifelse(Result == 'Win', KStakesEMQuarterAdj * EUPrice, 
      ifelse(Result == 'Half Win', KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdj, 
      ifelse(Result == 'Half Loss', KStakesEMQuarterAdj * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))), 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesEMQuarterAdj * EUPrice, 
    #'@         KStakesEMQuarterAdj * (HKPrice * 0.5 + 1), 
    #'@         KStakesEMQuarterAdj, KStakesEMQuarterAdj, 
    #'@         KStakesEMQuarterAdj * 0.5, 0))), 
    
    KReturnEMQuarterAdjnetProbB = suppressAll(
      ifelse(Result == 'Win', KStakesEMQuarterAdjnetProbB * EUPrice, 
      ifelse(Result == 'Half Win', KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), 
      ifelse(Result == 'Push'|Result == 'Cancelled', KStakesEMQuarterAdjnetProbB, 
      ifelse(Result == 'Half Loss', KStakesEMQuarterAdjnetProbB * 0.5, 
      ifelse(Result == 'Loss', 0, NA)))))) 
    #'@ plyr::mapvalues(Result, 
    #'@       c('Win', 'Half Win', 'Push', 'Cancelled', 'Half Loss', 'Loss'), 
    #'@       c(KStakesEMQuarterAdjnetProbB * EUPrice, 
    #'@         KStakesEMQuarterAdjnetProbB * (HKPrice * 0.5 + 1), 
    #'@         KStakesEMQuarterAdjnetProbB, KStakesEMQuarterAdjnetProbB, 
    #'@         KStakesEMQuarterAdjnetProbB * 0.5, 0)))
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
  
  ## refer to quantmod packages xts zoo data type but in tbl_df format.
  KPnames <- c('netEMEdge', 'PropHKPriceEdge', 'PropnetProbBEdge', 'KProbHKPrice',
               'KProbnetProbB', 'KProbFixed', 'KProbFixednetProbB', 'KEMProb',
               'KEMProbnetProbB', 'KProbHalf','KProbHalfnetProbB', 'KProbQuarter',
               'KProbQuarternetProbB', 'KProbAdj','KProbAdjnetProbB', 'KHalfAdj',
               'KHalfAdjnetProbB', 'KEMQuarterAdj', 'KEMQuarterAdjnetProbB')
  
  KSnames <- c('Stakes', 'KStakesHKPriceEdge', 'KStakesnetProbBEdge', 'KStakesHKPrice', 
               'KStakesnetProbB', 'KStakesFixed', 'KStakesFixednetProbB', 
               'KStakesEMProb', 'KStakesEMProbnetProbB', 'KStakesHalf', 
               'KStakesHalfnetProbB', 'KStakesQuarter', 'KStakesQuarternetProbB', 
               'KStakesAdj', 'KStakesAdjnetProbB', 'KStakesHalfAdj', 
               'KStakesHalfAdjnetProbB', 'KStakesEMQuarterAdj', 
               'KStakesEMQuarterAdjnetProbB')
  
  KPLnames <- c('PL', 'KPLHKPriceEdge', 'KPLnetProbBEdge', 'KPLHKPrice', 'KPLnetProbB', 
                'KPLFixed', 'KPLFixednetProbB', 'KPLEMProb', 'KPLEMProbnetProbB', 
                'KPLHalf', 'KPLHalfnetProbB', 'KPLQuarter', 'KPLQuarternetProbB', 
                'KPLAdj', 'KPLAdjnetProbB', 'KPLHalfAdj', 'KPLHalfAdjnetProbB', 
                'KPLEMQuarterAdj', 'KPLEMQuarterAdjnetProbB')
  
  KnamesBR <- paste0(KPnames, rep(c('.Open', '.High', '.Low', '.Close', '.Volume', 
                                    '.Adjusted'), each = length(KPnames)))
  
  BR <- matrix(nc = length(KnamesBR), nr = nrow(K), dimnames = list(NULL, KnamesBR)) %>% 
    tbl_df %>% data.frame(.id = seq(nrow(K)), TimeUS = K$TimeUS, DateUS = K$DateUS, 
                          League = K$League, .) %>% tbl_df
  
  KPL <- K %>% select(PL, KPLHKPriceEdge, KPLnetProbBEdge, KPLHKPrice, KPLnetProbB, KPLFixed, 
                      KPLFixednetProbB, KPLEMProb, KPLEMProbnetProbB, KPLHalf, KPLHalfnetProbB, 
                      KPLQuarter, KPLQuarternetProbB, KPLAdj, KPLAdjnetProbB, KPLHalfAdj, 
                      KPLHalfAdjnetProbB, KPLEMQuarterAdj, KPLEMQuarterAdjnetProbB) %>% 
    mutate_all(cumsum)
  
  KST <- K %>% select(Stakes, KStakesHKPriceEdge, KStakesnetProbBEdge, KStakesHKPrice, 
                      KStakesnetProbB, KStakesFixed, KStakesFixednetProbB, KStakesEMProb, 
                      KStakesEMProbnetProbB, KStakesHalf, KStakesHalfnetProbB, KStakesQuarter, 
                      KStakesQuarternetProbB, KStakesAdj, KStakesAdjnetProbB, KStakesHalfAdj, 
                      KStakesHalfAdjnetProbB, KStakesEMQuarterAdj, KStakesEMQuarterAdjnetProbB)
  
  ## Set initial fund size from the staking and profit & loss, below are 2 criteria...
  ##  1) fund size must be enough to place a bet, therefore max stakes + $1.
  ##  2) fund size cannot <= 0 to simulate whole process. Therefore need to set 
  ##  max loss + $1.
  initial <- max(max(KST),abs(min(KPL))) + 1
  
  KPL <- tbl_df(KPL + initial)
  KPL2 <- KPL %>% mutate_all(lag)
  KPL2[1, ] <- initial
  
  BR[grep('.Close', names(BR), value = TRUE)] <- KPL
  BR[grep('.Open', names(BR), value = TRUE)] <- KPL2
  BR[grep('.High', names(BR), value = TRUE)] <- 
    apply(BR[grep('.Close|.Open', names(BR), value = TRUE)], 1, max, na.rm = TRUE)
  BR[grep('.Low', names(BR), value = TRUE)] <- 
    apply(BR[grep('.Close|.Open', names(BR), value = TRUE)], 1, min, na.rm = TRUE)
  BR[grep('.Volume', names(BR), value = TRUE)] <- K %>% 
    select(Stakes, KStakesHKPriceEdge, KStakesnetProbBEdge, KStakesHKPrice, 
           KStakesnetProbB, KStakesFixed, KStakesFixednetProbB, KStakesEMProb, 
           KStakesEMProbnetProbB, KStakesHalf, KStakesHalfnetProbB, KStakesQuarter, 
           KStakesQuarternetProbB, KStakesAdj, KStakesAdjnetProbB, KStakesHalfAdj, 
           KStakesHalfAdjnetProbB, KStakesEMQuarterAdj, KStakesEMQuarterAdjnetProbB)
  BR[grep('.Adjusted', names(BR), value = TRUE)] <- adjusted
  
  rm(KPL, KPL2, KST)
  
  options(warn = 0)
  return(list(data = K, summary = sumdf, BR = BR, initial = initial))
}