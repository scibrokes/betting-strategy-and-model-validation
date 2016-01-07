arrfirmDatasets <- function(dfmList, lProfile=c(AH=0.10, OU=0.12), parallel=FALSE){
  ## Here I take the majority leagues setting profile which are "league-10-12"
  ## fMYPriceB = Back with vigorish price; fMYPriceL = Lay with vigorish price
  ## Here we term as Fair Odds
  
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc', quietly=TRUE))){
    suppressWarnings(install.packages('BBmisc'))}
  
  suppressMessages(require('BBmisc', quietly=TRUE))
  pkgs <- c('magrittr', 'plyr', 'dplyr', 'purrr', 'stringr', 'lubridate', 'doParallel')
  suppressAll(lib(pkgs)); rm(pkgs)
  
  if(parallel==TRUE){
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(3)
    doParallel::registerDoParallel(cores = 3)
    #' @BiocParallel::register(MulticoreParam(workers=2))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }
  
  dfm <- dfmList$datasets
  if((is.error(dfm))|(is.null(dfm))){
    stop('Please apply readfmirmDatasets() to read the datasets of Sports Consultancy Firm prior to reprocess!')
  }else{
    dfm$Picked <- sapply(strsplit(as.character(dfm$Selection), ' - '), function(x) x[length(x)])
    dfm$AHOU <- factor(ifelse(dfm$Picked == 'over'|dfm$Picked == 'under', 'OU', 'AH'))
    lProfile <- lProfile ## League setting profile
    
    ## Convert backed HKPrice to MYPrice in order to calculate the Layed Price
    dfm$fMYPriceB <- ifelse(dfm$HKPrice > 1, round(-1/(dfm$HKPrice), 3), round(dfm$HKPrice, 3))
    dfm$fMYPriceL <- ifelse(dfm$fMYPriceB < 0 & dfm$AHOU == 'AH', abs(dfm$fMYPriceB + lProfile[1]),
                     ifelse(dfm$fMYPriceB <= 1 & dfm$fMYPriceB > lProfile[1] & dfm$AHOU == 'AH', 
                            2-dfm$fMYPriceB - lProfile[1],
                     ifelse(dfm$fMYPriceB <= 1 & dfm$fMYPriceB > lProfile[2] & dfm$AHOU == 'OU', 
                            2-dfm$fMYPriceB - lProfile[2],
                     ifelse(dfm$fMYPriceB < 0 & dfm$AHOU == 'OU', abs(dfm$fMYPriceB + lProfile[2]), 0))))
    dfm$fMYPriceL <- ifelse(dfm$fMYPriceL > 1, round(-1/(dfm$fMYPriceL), 3), round(dfm$fMYPriceL, 3))
    
    ## A price listing which summarise the min to mx of Price backed by firm A on agency A
    mPrice <- unique(dfm[c('EUPrice', 'HKPrice', 'fMYPriceB')])
    mPrice <- mPrice[order(mPrice$EUPrice, decreasing=FALSE),]
    dfm$pHKRange <- cut(dfm$HKPrice, seq(floor(min(mPrice$HKPrice)), ceiling(max(mPrice$HKPrice)), 0.1))
    dfm$fHKPriceL <- ifelse(dfm$fMYPriceL < 0, round(-1/dfm$fMYPriceL, 3), round(dfm$fMYPriceL, 3))
    dfm$pMYRange <- cut(dfm$fMYPriceB, seq(floor(min(mPrice$fMYPriceB)), ceiling(max(mPrice$fMYPriceB)), 0.1))
    
    ## Categorize the selection by either 'favorite' or 'underdog', 'under' or 'over'
    dfm$Picked <- factor(ifelse(as.character(dfm$AHOU) == 'AH' & as.numeric(dfm$HCap) > 0,'underdog',
                         ifelse(as.character(dfm$AHOU) == 'AH' & as.numeric(dfm$HCap) < 0,'favorite',
                         ifelse(as.character(dfm$AHOU) == 'AH' & as.numeric(dfm$HCap) == 0 &
                                as.numeric(dfm$HKPrice) < as.numeric(dfm$fHKPriceL), 'favorite',
                         ifelse(as.character(dfm$AHOU) == 'AH' & as.numeric(dfm$HCap) == 0 &
                                as.numeric(dfm$HKPrice) > as.numeric(dfm$fHKPriceL), 'underdog',
                         ifelse(as.character(dfm$AHOU) == 'AH' & as.numeric(dfm$HCap) == 0 &
                                as.numeric(dfm$HKPrice) == as.numeric(dfm$fHKPriceL), 'favorite', dfm$Picked))))))
    
    ## Add an InPlay data range
    ## 1. Cut the time range to be 5 mins interval time
    ##
    dfm$InPlay <- factor(ifelse(str_detect(dfm$Mins, 'ET'), 'ET', ifelse(str_detect(dfm$Mins, 'No'), 'No', 'FT')))
    dfm$Mins2 <- gsub('ET', '', as.character(dfm$Mins))
    dfm$Mins2 <- factor(ifelse(str_detect(dfm$Mins2, 'HT'), 'HT', ifelse(str_detect(dfm$Mins2, 'FT'), 'FT', 
                        ifelse(str_detect(dfm$Mins2, 'No'), 'No', gsub('[^0-9]', '', as.character(dfm$Mins2))))))
    dfm$InPlay2 <- factor(ifelse(dfm$Mins2 == 'HT'|dfm$Mins2 == 'FT'|dfm$Mins2 == 0, 'Break', as.character(dfm$InPlay)))
    
    dfm <- llply(split(dfm, dfm$InPlay2), function(x) {
      data.frame(x, ipRange=str_replace_na(suppressWarnings(cut(as.numeric(
      as.character(x$Mins2)), breaks=seq(0, 90, 5)))), 'No')}, .parallel=parallel) %>% rbind_all %>% mutate(
        ipRange=ifelse(ipRange == 'NA', as.character(Mins2), ipRange) %>% ifelse(. == 0, as.character(InPlay), .))
    
    ## 2. Set the current handicap right before scoring a goal
    ##
    SC <- ldply(strsplit(ifelse(dfm$CurScore == 'No', '0-0', as.character(dfm$CurScore)), '-'))
    SC <- data.frame(lapply(SC, as.numeric))
    names(SC) <- c('HG', 'AG')
    dfm$HG <- SC$HG; dfm$AG <- SC$AG; rm(SC, mPrice)
    
    ## First half, Full-time or Extra time
    dfm$FHFTET <- factor(ifelse(str_detect(dfm$Home,'1st Half'), 'FH', ifelse(str_detect(dfm$Mins, 'ET'), 'ET', 'FT')))
    dfm$Picked2 <- factor(ifelse(as.character(dfm$Selection) == as.character(dfm$Home), 'home',
                          ifelse(as.character(dfm$Selection) == as.character(dfm$Away), 'away', as.character(dfm$Picked))))
    dfm$ipHCap <- ifelse(dfm$Picked2=='over', (dfm$HG + dfm$AG) - dfm$HCap,
                  ifelse(dfm$Picked2=='under', dfm$HCap - (dfm$HG + dfm$AG),
                  ifelse(dfm$Picked=='favorite' & dfm$Picked2=='home', (dfm$HG + dfm$HCap) - dfm$AG,
                  ifelse(dfm$Picked=='favorite' & dfm$Picked2=='away', (dfm$AG + dfm$HCap) - dfm$HG,
                  ifelse(dfm$Picked=='underdog' & dfm$Picked2=='home', dfm$AG - (dfm$HG + dfm$HCap),
                  ifelse(dfm$Picked=='underdog' & dfm$Picked2=='away', dfm$HG - (dfm$AG + dfm$HCap), NA))))))
    #'@ dfm[c('Picked', 'Picked2', 'ipRange', 'FHFTET', 'AHOU', 'HCap', 'HKPrice', 'CurScore', 'ipHCap')]+
    
    bP <- ifelse(dfm$fMYPriceB<0, round(-1/dfm$fMYPriceB,3), round(dfm$fMYPriceB,3))
    lP <- ifelse(dfm$fMYPriceL<0, round(-1/dfm$fMYPriceL,3), round(dfm$fMYPriceL,3))
    
    ## Real Price / Net Price Back convert to net probabilities as applied in Dixon&Coles1996.
    dfm %<>% mutate(netProbB=round(bP/(bP+lP),4), netProbL=round(lP/(bP+lP),4))
    rm(bP, lP)
    
    ## To know the true value price of favorite/over or underdog/under 
    dfm$favNetProb <- ifelse(dfm$Picked=='favorite'|dfm$Picked=='over' , dfm$netProbB, dfm$netProbL)
    dfm$undNetProb <- ifelse(dfm$Picked=='underdog'|dfm$Picked=='under', dfm$netProbB, dfm$netProbL)
    
    dfmList$datasets <- dfm
    return(dfmList)
  }
}

