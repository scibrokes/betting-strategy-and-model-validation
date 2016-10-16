arrfirmData <- function(dfmList, lProfile = c(AH = 0.10, OU = 0.12), parallel = FALSE){
  ## Here I take the majority leagues setting profile which are "league-10-12"
  ## fMYPriceB = Back with vigorish price; fMYPriceL = Lay with vigorish price
  ## Here we term as Fair Odds
  
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc', quietly = TRUE))){
    suppressWarnings(install.packages('BBmisc'))}
  
  suppressMessages(require('BBmisc', quietly = TRUE))
  pkgs <- c('magrittr', 'plyr', 'dplyr', 'purrr', 'stringr', 'lubridate', 'doParallel')
  suppressAll(lib(pkgs)); rm(pkgs)
  
  if(parallel == TRUE){
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(3)
    doParallel::registerDoParallel(cores = 3)
    #' @BiocParallel::register(MulticoreParam(workers = 2))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical = TRUE)))
  }
  
  dfm <- dfmList$datasets
  if((is.error(dfm))|(is.null(dfm))){
    stop('Please apply readfmirmData() to read the datasets of Sports Consultancy Firm prior to reprocess!')
  }else{
    dfm %<>% mutate(Picked = sapply(strsplit(as.character(Selection), ' - '), function(x) x[length(x)]), 
                    AHOU = factor(ifelse(Picked == 'over'|Picked == 'under', 'OU', 'AH')))
    
    ## Convert backed HKPrice to MYPrice in order to calculate the Layed Price
    dfm %<>% mutate(fMYPriceB = ifelse(HKPrice > 1, round(-1/(HKPrice), 3), round(HKPrice, 3)),
                    fMYPriceL = ifelse(fMYPriceB < 0 & AHOU == 'AH', abs(fMYPriceB + lProfile[1]),
                                ifelse(fMYPriceB <= 1 & fMYPriceB > lProfile[1] & AHOU == 'AH', 
                                       2-fMYPriceB - lProfile[1],
                                ifelse(fMYPriceB <= 1 & fMYPriceB > lProfile[2] & AHOU == 'OU', 
                                       2-fMYPriceB - lProfile[2],
                                ifelse(fMYPriceB < 0 & AHOU == 'OU', abs(fMYPriceB + lProfile[2]), 0))))) %>% 
      mutate(fMYPriceL = ifelse(fMYPriceL > 1, round(-1/(fMYPriceL), 3), round(fMYPriceL, 3)))
    
    ## A price listing which summarise the min to max of Price backed by firm A on agency A
    mPrice <- dfm %>% select(EUPrice, HKPrice, fMYPriceB) %>% unique %>% arrange(EUPrice)
    dfm %<>% mutate(pHKRange = cut(HKPrice, seq(floor(min(mPrice$HKPrice)), ceiling(max(mPrice$HKPrice)), 0.1)), 
                    fHKPriceL = ifelse(fMYPriceL < 0, round(-1/fMYPriceL, 3), round(fMYPriceL, 3)), 
                    pMYRange = cut(fMYPriceB, seq(floor(min(mPrice$fMYPriceB)), 
                                                  ceiling(max(mPrice$fMYPriceB)), 0.1)))
    
    mPrice <- dfm %>% select(pHKRange, pMYRange) %>% llply(levels)
    p1 <- c('(0,0.1]', '(0.1,0.2]', '(0.2,0.3]', '(0.3,0.4]', '(0.4,0.5]', '(0.5,0.6]', '(0.6,0.7]')
    p2hk <- c('(0.7,0.8]', '(0.8,0.9]', '(0.9,1]', '(1,1.1]', '(1.1,1.2]', '(1.2,1.3]')
    p2my <- c('(0.7,0.8]', '(0.8,0.9]', '(0.9,1]', '(-1,-0.9]', '(-0.9,-0.8]', '(-0.8,-0.7]')
    p3hk <- mPrice[[1]][!as.character(mPrice[[1]]) %in% c(p1, p2hk)]
    p3my <- mPrice[[2]][!as.character(mPrice[[2]]) %in% c(p1, p2my)]
    pMY <- list(p1, p2my, p3my); pHK <- list(p1, p2hk, p3hk); rm(mPrice, p1, p2hk, p2my, p3hk, p3my)
    dfm %<>% mutate(pHKRange2 = ifelse(as.character(pHKRange) %in% pHK[[1]], '(0,0.7]', 
                              ifelse(as.character(pHKRange) %in% pHK[[2]], as.character(pHKRange),
                                     paste0('(1.3,', tail(pHK[[3]],1) %>% 
                                              strsplit(',') %>% .[[1]] %>% .[2] %>% str_replace(']',''),']'))), 
                    pMYRange2 = ifelse(as.character(pMYRange) %in% pMY[[1]], '(0,0.7]', 
                              ifelse(as.character(pMYRange) %in% pMY[[2]], as.character(pMYRange),
                                     paste0('(1.3,', tail(pMY[[3]],1) %>% 
                                              strsplit(',') %>% .[[1]] %>% .[2] %>% str_replace(']',''),']'))))
    
    ## Categorize the selection by either 'favorite' or 'underdog', 'under' or 'over'
    dfm %<>% mutate(Picked = factor(ifelse(as.character(AHOU) == 'AH' & as.numeric(HCap) > 0,'underdog',
                                  ifelse(as.character(AHOU) == 'AH' & as.numeric(HCap) < 0,'favorite',
                                  ifelse(as.character(AHOU) == 'AH' & as.numeric(HCap) == 0 &
                                         as.numeric(HKPrice) < as.numeric(fHKPriceL), 'favorite',
                                  ifelse(as.character(AHOU) == 'AH' & as.numeric(HCap) == 0 &
                                         as.numeric(HKPrice) > as.numeric(fHKPriceL), 'underdog',
                                  ifelse(as.character(AHOU) == 'AH' & as.numeric(HCap) == 0 &
                                         as.numeric(HKPrice) == as.numeric(fHKPriceL), 'favorite', Picked)))))))
    
    ## Add an InPlay data range
    ## 1. Cut the time range to be 5 mins interval time
    ##
    dfm %<>% mutate(InPlay = factor(ifelse(str_detect(Mins, 'ET'), 'ET', 
                                           ifelse(str_detect(Mins, 'No'), 'No', 'FT'))), 
                    Mins2 = gsub('ET', '', as.character(Mins))) %>%
      mutate(Mins2 = factor(ifelse(str_detect(Mins2, 'HT'), 'HT',
                          ifelse(str_detect(Mins2, 'FT'), 'FT',
                          ifelse(str_detect(Mins2, 'No'), 'No', gsub('[^0-9]', '', as.character(Mins2)))))),
             InPlay2=factor(ifelse(Mins2 == 'HT'|Mins2 == 'FT'|Mins2 == 0, 'Break', as.character(InPlay))))
    
    dfm <- llply(split(dfm, dfm$InPlay2), function(x) {
      data.frame(x, ipRange = str_replace_na(suppressWarnings(cut(as.numeric(
      as.character(x$Mins2)), breaks = seq(0, 90, 5)))), 'No')}, 
      .parallel = parallel)
    
    ## merge_all() will slower than bind_rows but bind_rows will auto change the 
    ##   'Time' column variable in format hms() into 0 value, use merge_all instead.
    dfm %<>% merge_all %>% 
      mutate(ipRange = ifelse(ipRange == 'NA', as.character(Mins2), ipRange) %>% 
               ifelse(. == 0, as.character(InPlay), .))
    dfm$X.No. <- NULL
    
    ## 2. Set the current handicap right before scoring a goal
    ##
    SC <- suppressAll(ldply(strsplit(ifelse(dfm$CurScore == 'No', '0-0', as.character(dfm$CurScore)), '-')))
    SC <- data.frame(lapply(SC, as.numeric))
    names(SC) <- c('HG', 'AG')
    dfm %<>% mutate(HG = SC$HG, AG = SC$AG)
    suppressAll(rm(SC, mPrice))
    
    ## First half, Full-time or Extra time
    dfm %<>% mutate(FHFTET = factor(ifelse(str_detect(Home, '1st Half'), 'FH',
                                  ifelse(str_detect(Mins, 'ET'), 'ET', 'FT'))),
                   Picked2 = factor(ifelse(as.character(Selection) == as.character(Home), 'home',
                                  ifelse(as.character(Selection) == as.character(Away), 'away', 
                                         as.character(Picked)))),
                   ipHCap = ifelse(Picked2 == 'over', (HG + AG) - HCap,
                            ifelse(Picked2 == 'under', HCap - (HG + AG),
                            ifelse(Picked == 'favorite' & Picked2=='home', (HG + HCap) - AG,
                            ifelse(Picked == 'favorite' & Picked2=='away', (AG + HCap) - HG,
                            ifelse(Picked == 'underdog' & Picked2=='home', AG - (HG + HCap),
                            ifelse(Picked == 'underdog' & Picked2=='away', HG - (AG + HCap), NA)))))))
    dfm %<>% mutate(CurScore2 = paste0(ifelse(HG >= 3,'>2',HG),'-',ifelse(AG >= 3, '>2', AG)))
    
    bP <- ifelse(dfm$fMYPriceB < 0, round(-1 / dfm$fMYPriceB, 3), round(dfm$fMYPriceB, 3))
    lP <- ifelse(dfm$fMYPriceL < 0, round(-1 / dfm$fMYPriceL, 3), round(dfm$fMYPriceL, 3))
    
    ## Real Price / Net Price Back convert to net probabilities as applied in Dixon&Coles1996.
    dfm %<>% mutate(netProbB = round(bP / (bP + lP), 4), netProbL = round(lP / (bP + lP), 4))
    rm(bP, lP)
    
    ## To know the true value price of favorite/over or underdog/under 
    dfm %<>% mutate(favNetProb = ifelse(Picked == 'favorite'|Picked == 'over' , netProbB, netProbL),
                    undNetProb = ifelse(Picked == 'underdog'|Picked == 'under', netProbB, netProbL))
    
    dfmList$datasets <- dfm %>% tbl_df
    return(dfmList)
  }
}


