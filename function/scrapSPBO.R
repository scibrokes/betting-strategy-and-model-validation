scrapSPBO <- function(lnk=lnk, dateID=dateID, path=path, parallel=FALSE){
  ## Due to the scrapSPBO function scrapped unmatched data, example lnk[827],
  ##  therefore I rewrite the function as scrapSPBO2
  ## parallel=TRUE extreme speedy will overhault
  require('doParallel',quietly=TRUE)
  require('plyr',quietly=TRUE)
  require('dplyr',quietly=TRUE)
  require('stringr',quietly=TRUE)
  
  ## Setting to omitt all warnings since there are alot of data validation for manipulation
  options(warn=-1)
  
  lnk <- lnk; dateID <- dateID; path <- path; parallel <- parallel
  if(length(lnk)!=length(dateID)){ stop('The length of url links and date id not tally !!!') }
  n <- seq(1:length(lnk))
  
  ## Web scrapping on leagues and teams
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc',quietly=TRUE))){
    suppressWarnings(install.packages('BBmisc'))
    suppressAll(require('BBmisc',quietly=TRUE))}
  
  ## Might load RSeleniumUtilities package if needed
  ##  https://github.com/greenore/RSeleniumUtilities
  suppressPackageStartupMessages(library('BBmisc'))
  pkgs <- c('reshape','plyr','dplyr','rvest','XML','RCurl','stringr','stringi','doParallel','grDevices')
  suppressPackageStartupMessages(lib(pkgs)); rm(pkgs)
  
  if(parallel==TRUE){
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(3)
    doParallel::registerDoParallel(cores = 3)
    #' @BiocParallel::register(MulticoreParam(workers=2))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }

  llply(n, function(i){
    dataElem <- html_session(lnk[i]) %>% html_nodes('script') %>% .[[1]] %>% html_text %>% str_split(',') %>%
      .[[1]] %>% str_extract_all(.,'[#0-9a-zA-Z].*') %>% .[sapply(.,length)==1] %>%
      .[!str_detect(.,'[\u4e00-\u9fa5]')] %>% unlist %>% gsub('(var bf=\")|([0-9]{1}!)','',.) %>% .[!str_detect(.,'=')]
    #regex(dataElem,'[0-9]{10}'); grep(dataElem,'[0-9]{10}') to match the location of the element within a vector
    ncolID.df <- dataElem %>% str_detect(.,'[0-9]{10}') %>% matrix(.,dimnames=list(NULL,'V1')) %>% data.frame %>%
                   subset(.,V1==TRUE) %>% rownames %>% as.numeric
    irng <- c(ncolID.df,length(dataElem))
    ncol.df <- diff(irng)
    dfm <- as.matrix(rbind_all(llply(seq(1,length(ncol.df)), function(j) data.frame(matrix(dataElem[irng[j]:(irng[j+1]-1)],
           byrow=TRUE,ncol=ncol.df[[j]])),.parallel=parallel)))
    
    ## dfm[(dfm[,04]==0)&(dfm[,05]==0),c(07,10,12:ncol(dfm))] <- dfm[(dfm[,04]==0)&(dfm[,05]==0),c(06,07,08:11)]
    if((dfm[,04]==0)&(dfm[,05]==0)){
      dfm[(dfm[,04]==0)&(dfm[,05]==0),c((ncol(dfm)-8),(ncol(dfm)-5),(ncol(dfm)-3):ncol(dfm))] <- dfm[(dfm[,04]==0)&
      (dfm[,05]==0),c(06:(ncol(dfm)-4))]
      dfm[(dfm[,04]==0)&(dfm[,05]==0),c(06,08,09)] <- 0 # no card since match have'nt started
    }
    if((dfm[,04]!=0)&(nchar(dfm[,04])==1)){
      dfm[(dfm[,04]!=0)&(nchar(dfm[,04])==1),5:ncol(dfm)] <- dfm[(dfm[,04]!=0)&(nchar(dfm[,04])==1),4:14]
    }
    if((nchar(dfm[,04])>1)&(nchar(dfm[,07])==1)){
      dfm[(nchar(dfm[,04])>1)&(nchar(dfm[,07])==1),07:ncol(dfm)] <- c(0,dfm[(nchar(dfm[,04])>1)&(nchar(dfm[,07])==1),7:14])
    }
    if((nchar(dfm[,04])>1)&(nchar(dfm[,10])==1)){
      dfm[(nchar(dfm[,04])>1)&(nchar(dfm[,10])==1),10:ncol(dfm)] <- c(0,dfm[(nchar(dfm[,04])>1)&(nchar(dfm[,10])==1),10:14])
    }
    if((nchar(dfm[,04])>1)&is.na(dfm[,15])){
      dfm[(nchar(dfm[,04])>1)&is.na(dfm[,15]),ncol(dfm)] <- 0
    }
    dfm[,04] <- ifelse(nchar(dfm[,04])>1,paste0(substr(dateID,1,4),'/',dfm[,04]),dfm[,04])
    
    HTGoal <- str_split_fixed(dfm[,12],' - ',2); HTGoal[HTGoal[,02]=='',02] <- 0
    dfm <- cbind(dfm[,01:05],dfm[,07],dfm[,10],dfm[,08:09],HTGoal,dfm[,06],dfm[,11],dfm[,13:ncol(dfm)])
    dfm <- data.frame(dfm); rm(HTGoal)
    dfnames <- c('matchID','LeagueColor','League','Date','Finished','Home',
                  'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')
    names(dfm) <- dfnames
    dfm <- dfm[!duplicated(dfm),dfnames]
    dfm <- dfm[!((is.na(as.numeric(as.character(dfm$FTHG))))|(is.na(as.numeric(as.character(dfm$FTAG))))|
                (is.na(as.numeric(as.character(dfm$HTHG))))|(is.na(as.numeric(as.character(dfm$HTAG))))|
                (is.na(as.numeric(as.character(dfm$H.Card))))|(is.na(as.numeric(as.character(dfm$A.Card))))|
                (is.na(as.numeric(as.character(dfm$HT.matchID))))|(is.na(as.numeric(as.character(dfm$HT.graph1))))|
                (is.na(as.numeric(as.character(dfm$HT.graph2))))),]
    dfm$Home <- str_replace_all(as.character(dfm$Home),'^0','NA')
    dfm$Away <- str_replace_all(as.character(dfm$Away),'^0','NA')
    
    ## There has a team name has contents encoding error, here I gsub the team name. 
    ## Check if '1&ordm; Maio Funchal' inside the team names, example length 1:10
    #'@ sort(unique(c(unique(as.vector(dfm$Home)),unique(as.vector(dfm$Away)))))[1:10]
    #'@ if('1&ordm; Maio Funchal' %in% as.character(dfm$Home)){
      dfm <- dfm %>% mutate(Home=factor(as.character(gsub('^.*;','',as.character(Home)))),
                            Away=factor(as.character(gsub('^.*;','',as.character(Away)))))
    #'@ }

    ## Replace all space which at the first and also last character inside elements.
    dfm <- llply(dfm, function(x){gsub('^\\s{1,}|\\s{1,}$','',x)},.parallel=parallel) %>% data.frame %>% 
      tbl_df %>% mutate_each(funs(as.character))
    
    ## Save livescores data into folder
    dir.create(file.path(paste0(getwd(),'/datasets/',path)))
    
    ## Introduce to ff and ffbase for large datasets
    ## http://www.r-bloggers.com/if-you-are-into-large-data-and-work-a-lot-with-package-ff/
    ## In order to easier manipulate (read/write) the data, here I just keep the csv file seperately by dateID.
    write.csv(dfm, file=paste0(getwd(),'/datasets/',path,'/',dateID[i],'.csv'))
    cat(i,paste0(' Wrote ',dateID[i],'.csv'),'\n')}, .parallel=parallel)
  }


