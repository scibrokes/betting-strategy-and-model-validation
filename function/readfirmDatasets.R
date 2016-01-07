readfirmDatasets <- function(years=years, parallel=FALSE){
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc',quietly=TRUE))){
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
  
  if(is.numeric(years)){
    years <- as.list(years)
  }else if(is.list(years)){
    if(is.numeric(unlist(as.list(years)))){
      years <- years
    }else{
      stop('Please insert a list or a vector of years in numeric format!')
    }
  }else{
    stop('Please insert a list or a vector of years in numeric format!')
  }
  ## Read the datasets
  ## Refer to **Testing efficiency of coding.Rmd** at chunk `get-data-summary-table-2.1`
  dfm <- rbind_all(llply(years, function(x) {
    data.frame(Sess=x, read.csv(paste0(getwd(), '/datasets/', x, '.csv'), header=TRUE, sep=','))}, 
    .parallel=parallel)) %>% tbl_df
  
  ## Data processing and clearing
  matchID <- dfm$Match %>% as.character %>% str_extract_all('[[:alnum:]\\(.\\) ]{1,}[[:alnum:].: ]{1,}') %>% 
    ldply(.parallel=parallel) %>% mutate(V2=str_replace_all(V2, '\\(', '')) %>% tbl_df
  
  ## Checking if the strings length match
  #' @laply(matchID,length)
  #' @which(laply(matchID,length)!=4)
  #' @matchID[laply(matchID,length)!=4]
  
  matchID$V1 %<>% str_split(' vs ') %>% ldply(.parallel=parallel) %>% tbl_df
  matchID <- data.frame(matchID$V1, matchID[-1])
  matchID[str_detect(matchID$V2, '\\('),]$V2 <- paste0(matchID[str_detect(matchID$V2, '\\('),]$V2, ')')
  
  ## Omit the (Corners), (1st Half Corners) and (1st Half)
  others <- sort(unique(c(matchID[str_detect(matchID$V1, '(1st Half)|(Corners)|(1st Half Corners)'),]$V1,
                          matchID[str_detect(matchID$V2, '(1st Half)|(Corners)|(1st Half Corners)'),]$V2)))
  corners <- sort(unique(c(as.character(matchID[str_detect(matchID$V1, '(Corners)'),]$V1),
                           as.character(matchID[str_detect(matchID$V2, '(Corners)'),]$V2))))
  
  #'@ options(dplyr.print_max = 1e9)
  #'@ InPlay <- str_extract_all(as.character(dfm$In.R.), '[^\\?(]{1,}[0-9a-zA-Z]{1,}')
  InPlay <- str_extract_all(as.character(dfm$In.R.), '[^\\?]{1,}[0-9a-zA-Z]{1,}')
  mx <- max(laply(InPlay, length))
  InPlay <- ldply(InPlay, function(x) rep(x, mx)[1:mx]) %>% mutate(V2=gsub('[^0-9a-zA-Z]', '', V2)) %>% tbl_df
  dfm$Match <- NULL
  dfm <- data.frame(cbind(dfm[c(2:1)], matchID, dfm[3:6], InPlay, dfm[8:ncol(dfm)])) %>% tbl_df
  names(dfm) <- c('No', 'Sess', 'Home', 'Away', 'Day', 'Date', 'Time', 'Selection', 'HCap', 'EUPrice', 
                  'Stakes', 'CurScore', 'Mins', 'Result', 'PL', 'Rebates')
  rm(mx, matchID, InPlay)
  
  ## tbl_df() doesn't support POSIXct format, lubridate::day()
  ## https://github.com/hadley/dplyr/issues/1382
  #'@ levels(as.Date(dfm$Date)==as.character(dfm$Date)) #Checking if as.Date(x) same with GMT time zone
  #'@ dfm %>% mutate(DateUK=dmy_hm(paste0(Date,Time), tz='Europe/London'), 
  #'@                Date=dmy_hm(strptime(format(DateUK, tz='Asia/Hong_Kong', usetz=TRUE, 
  #'@                            format='%d %b %Y %H:%M:%S'), format='%d %b %Y %H:%M')), 
  #'@                            tz='Asia/Hong_Kong') %>% select(Date, DateUK)
  
  dfm %<>% mutate(Month=month(dmy(Date)), DateUK=dmy_hm(paste0(Date,Time), tz='Europe/London'), 
                  Date=format(DateUK, tz='Asia/Taipei', usetz=TRUE, format='%Y-%m-%d %H:%M:%S'),
                  Selection=factor(Selection), Stakes=as.numeric(gsub('[^0-9]', '', Stakes)), 
                  PL=ifelse(Result=='Loss', -as.numeric(gsub('[^0-9]', '', PL)), 
                            as.numeric(gsub('[^0-9]', '', PL))), Return=Stakes+PL, HKPrice=EUPrice-1)
  ## Date=as.Date(strptime(Date, format='%d %b %Y', tz='Asia/Taipei')) since spbo follow GMT+8 timezone
  
  ## Clear all spaces at the first and last character inside avery single element, reorder the columns
  ##  and re-class the columns
  dfm %<>% llply(., function(x){gsub('^\\s{1,}|\\s{1,}$', '', x)}, .parallel=parallel) %>% data.frame %>% 
    mutate_each(funs(as.character)) %>% select(No, Sess, Month, Day, DateUK, Date, Time, Home, Away, Selection, 
                                               HCap, EUPrice, HKPrice, Stakes, CurScore, Mins, Result, 
                                               Return, PL, Rebates) %>% tbl_df
  
  dfm %<>% mutate(No=as.numeric(No), Sess=as.numeric(Sess), Month=month(Date), 
                  Day=factor(Day), DateUK=ymd_hms(DateUK), Date=ymd_hms(Date), Time=hm(Time), 
                  Home=factor(Home), Away=factor(Home), Selection=factor(Selection), HCap=as.numeric(HCap), 
                  EUPrice=as.numeric(EUPrice), HKPrice=as.numeric(HKPrice), Stakes=as.numeric(Stakes), 
                  CurScore=factor(CurScore), Mins=factor(Mins), Result=factor(Result), PL=as.numeric(PL), 
                  Rebates=as.numeric(Rebates), Return=as.numeric(Return)) %>% tbl_df
  #'@ %>% map_if(is.numeric, round, 2) %>% map_if(is.character, factor) ## purr::map_if suddenly unable work
  
  res <- list(datasets=dfm, others=others, corners=corners)
  
  return(res)
}


