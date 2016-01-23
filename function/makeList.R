makeList <- function(mbase, spboData, levDist=0.1, parallel=FALSE){
  ## If agrep==TRUE will partial match the strings and generate output, always adjust with levDist for 
  ##   max.distance in agrep()
  
  if((!is.list(mbase)) & (names(mbase)!= c('datasets', 'others', 'corners')) & (!is.data_frame(spboData))){
    stop('Please apply readf1irmDatasets() to get the firm A data and readSPBO2() to get the spboData 
         livescore data !')}
  
  if(parallel==TRUE){
    suppressPackageStartupMessages(require('doParallel', quietly=TRUE))
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(8)
    doParallel::registerDoParallel(cores = 16)
    #' @BiocParallel::register(MulticoreParam(workers=8))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }
  
  if(parallel!=FALSE & parallel!=TRUE){
    stop('Please select "parallel=TRUE" or "parallel=FALSE"!')
  }
  
  if(!is.numeric(levDist)){
    stop('Please enter a numeric number from "levDist=0" to "levDist=Inf"!')
  }
  
  
  ## Loading the packages
  if(!'devtools' %in% installed.packages()){
    install.packages('devtools')}
  if(!'BBmisc' %in% installed.packages()){
    install.packages('BBmisc')}
  
  suppressPackageStartupMessages(library('BBmisc'))
  pkgs <- c('devtools', 'stringr', 'magrittr', 'plyr', 'dplyr', 'foreach', 'doParallel', 'tidyr')
  suppressAll(lib(pkgs)); rm(pkgs)
  suppressAll(source('function/signature.R'))
  
  teamID <- sort(unique(c(as.character(mbase$datasets$Home), as.character(mbase$datasets$Away))))
  spboTeam <- sort(c(as.vector(spboData$Home), as.vector(spboData$Away))) %>% 
    ifelse(nchar(.)==0, NA, .) %>% na.omit
  spboTeamID <- sort(unique(spboTeam)); rm(spboTeam)
  
  ## Apply signature() which will re-arrange the strings for increased the accuracy of Levenstein or 
  ##   or string distance calculation.
  names(teamID) <- sapply(teamID, signature)
  names(spboTeamID) <- sapply(spboTeamID, signature)
  
  ## Filter and drop the first-half, corners and other games
  tmID <- teamID[!teamID %in% mbase$others]
  
  
  
  
  ## STEP 1) Filter the exactly match (or different Capital letter teams' name) teams' name with spboTeamID,
  ##    seperate it to ease the processing time to find the approximate teams' name in section (2).
  ## (a1) Duplicated teams' name
  tmID1A <- tmID[tmID %in% spboTeamID]
  spboTM1A <- spboTeamID[spboTeamID %in% tmID]
  
  ## (a2) Not duplicated teams' name
  tmID1B <- tmID[!tmID %in% spboTeamID]
  spboTM1B <- spboTeamID[!spboTeamID %in% tmID]
  
  ## (b1) Partial match teams' name, only capital letter difference
  tmID2A <- tmID1B[tolower(tmID1B) %in% tolower(spboTM1B)]
  spboTM2A <- spboTM1B[tolower(spboTM1B) %in% tolower(tmID1B)]
  
  ## (b2) Partial match teams' name, not only capital letter difference
  tmID2B <- tmID1B[!tmID1B %in% tmID2A]
  spboTM2B <- spboTM1B[!spboTM1B %in% spboTM2A]
  
  
  ## STEP 2) Apply regular expression to filter/seperate the women team, B team or U21 etc prior to partial 
  ##     matching the most approximate teams' name (teams' name others than in STEP 1).
  ##   For example: 
  ## > tmID[grep('1860',tmID)]
  ## [1] "1860 Munchen"
  ## > spboTeamID[grep('Munchen',spboTeamID)]
  ## [1] "1860 Munchen II"          "1860 Munchen U17"        
  ## [3] "1860 Munchen U19"         "Bayern Munchen"          
  ## [5] "Bayern Munchen BA"        "Bayern Munchen II"       
  ## [7] "Bayern Munchen U17"       "Bayern Munchen U19"      
  ## [9] "Bayern Munchen Women"     "FFC Wacker Munchen Women"
  ## [11] "TSV 1860 Munchen"
  ##
  ## "1860 Munchen II" will be matched after I tried to apply all 10 methods inside stringdist() and also
  ## tried the while loops Maximum Likelihood which refer from below website :
  ## http://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets
  ## since second team has only " II" 3 different characters but "TSV " has 4 different characters.
  ## However the correct team name is "TSV 1860 Munchen" since II is the second team. Therefore I need to
  ## separate the second/third team, U18/U19/U23 team, reserves team etc.
  ##
  ## (a1) Separate women teams' name
  wm <- as.list(c(' women', ' woman', ' female', ' (w)', ' (wm)'))
  tmID3A <- unlist(llply(wm, function(x) tmID2B[grep(tolower(x), tolower(tmID2B), fixed=TRUE)]))
  spboTM3A <- unlist(llply(wm, function(x) spboTM2B[grep(tolower(x), tolower(spboTM2B), fixed=TRUE)]))
  
  ## (a2) Non-women teams' name
  tmID3B <- tmID2B[!tmID2B %in% tmID3A]
  spboTM3B <- spboTM2B[!spboTM2B %in% spboTM3A]
  rm(wm)
  
  ## There has a concern which is second teams' name must be exactly same with first team but only add II, 
  ##  reserved etc to the first team name, for example : Mainz 05 is first team but not fifth reserved team.
  ##  More soccer matches data scrapped will be more accurate, for example if we only scrapped one day data, 
  ##  how can we matching the first team if let say only Chelsea reserved team play on that particular date.
  ##  However there has another concern which is first team 'TSV 1860 Munchen' but second/U19 team termed as 
  ##  '1860 Munchen II', '1860 Munchen U19' etc. Then 'Lincoln' = 'Lincoln City' != 'Lincoln United'
  ##  
  ## Due to I have seperated the youth, reseved, women team, therefore later agrep will also approximately 
  ##  matching the 'TSV 1860 Munchen'.
  ## 
  ## (b1) Separate second team, reserved team or U18/U21 etc teams' name based on women/men
  ext.lst <- as.list(c('2', '02', 'B', 'II', '3', '03', 'C', 'III', '4', '04', 'D', 'IV',
                       paste0('U', seq(15, 23)), 'Res', 'Rev', 'Reserve', 'Reserved', 'Reserves',
                       'Youth', 'All\\s{1}Stars', 'Beach\\s{1}Soccer', 'Futsal'))
  # we can add somemore new element/key words if needed, for example : Futsal, Basketball, Volleybal etc.
  
  wm <- c('\\s{1}women', '\\s{1}woman', '\\s{1}female', '\\s{1}(w)', '\\s{1}(wm)')
  
  ext.m <- llply(ext.lst, function(x) paste0('\\s{1}', x, '$'))
  
  ## ext.wm <- llply(ext.lst, function(x) {paste(paste0('\\s{1}', x, wm, '\\s{1}|\\s{1}', x, wm, '$|',
  ##           wm, '\\s{1}', x, '\\s{1}|', wm, '\\s{1}', x, '$'), collapse='|')})
  ## 
  ## Above ext.wm function Will match the 2nd/3rd team of U17,U18 in both lists.
  ##   For example: Ukraine II Women U17 will matching in both list `U17` and `II`. 
  ##   Here I just omit it since youth team reserve data will not be obtain correctly and even though have 
  ##   no official website. So far, punters almost no place bet on reseve/2nd/3rd team of youth team.
  ext.wm <- llply(ext.lst, function(x) {paste(paste0('\\s{1}', x, wm, '$|', wm, '\\s{1}', x, '$'), collapse='|')})
  
  ## ---------------------------------------------------------------------------------
  ## Due to not familar with function 'eval(expression(x))'. Here I using substitute function.
  llst <- function(lst=substitute(lst)){
    list('II'=c(lst[[1]], lst[[2]], lst[[3]], lst[[4]]),
         'III'=c(lst[[5]], lst[[6]], lst[[7]], lst[[8]]),
         'IV'=c(lst[[9]], lst[[10]], lst[[11]], lst[[12]]), 'U15'=lst[[13]],
         'U16'=lst[[14]], 'U17'=lst[[15]], 'U18'=lst[[16]], 'U19'=lst[[17]],
         'U20'=lst[[18]], 'U21'=lst[[19]], 'U22'=lst[[20]], 'U23'=lst[[21]],
         'Res'=c(lst[[22]], lst[[23]], lst[[24]], lst[[25]], lst[[26]]),
         'Youth'=lst[[27]], 'All Stars'=lst[[28]], 'Beach Soccer'=lst[[29]], 'Futsal'=lst[[30]])
    # we can add somemore new element/key words if needed, for example : Futsal, Basketball, Volleybal etc.
  }
  
  ## ---------------------------------------------------------------------------------
  ## women second/reserved team
  tmID4A <- llst(llply(ext.wm, function(x){
    tmID3A[grep(tolower(x), tolower(tmID3A))]}, .parallel=parallel))
  spboTM4A <- llst(llply(ext.wm, function(x){
    spboTM3A[grep(tolower(x), tolower(spboTM3A))]}, .parallel=parallel))
  
  ## men second/reserved team
  tmID4B <- llst(llply(ext.m, function(x){
    tmID3B[grep(tolower(x), tolower(tmID3B))]}, .parallel=parallel))
  spboTM4B <- llst(llply(ext.m, function(x){
    spboTM3B[grep(tolower(x), tolower(spboTM3B))]}, .parallel=parallel))
  rm(ext.lst, wm, ext.m, ext.wm, llst)
  
  ## (b2) First team seperated by men/women
  ## women first team
  tmID5A <- tmID3A[!tmID3A %in% unlist(tmID4A)]
  spboTM5A <- spboTM3A[!spboTM3A %in% unlist(spboTM4A)]
  
  ## men first team
  tmID5B <- tmID3B[!tmID3B %in% unlist(tmID4B)]
  spboTM5B <- spboTM3B[!spboTM3B %in% unlist(spboTM4B)]
  
  ## combine to list
  tmID6 <- list(wm=c(tmID4A, fst=list(tmID5A)), mn=c(tmID4B, fst=list(tmID5B)))
  spboTM6 <- list(wm=c(spboTM4A, fst=list(spboTM5A)), mn=c(spboTM4B, fst=list(spboTM5B)))
  
  #'@ ext <- c('\\s{1}United', '\\s{1}City', '\\s{1}FC', '\\s{1}BK', '\\s{1}Club', '[^AC]\\s{1}', '\\s{1}SC')
  ext <- c(' United', ' City', ' FC', ' BK', ' Club', 'AC ', ' SC', 'Inter ')
  # we can add somemore new element/key words if needed, for example : AFC, SC, FC.
  
  ## apply agrep to partial match the teams' name
  tm <- llply(seq(tmID6),function(i) {
          llply(seq(tmID6[[i]]), function(j){
            tid <- tmID6[[i]][[j]]; pid <- spboTM6[[i]][[j]]
            lg <- ifelse(levDist!=Inf, length(tid)>0, !is.na(as.character(tid)))
            if(lg==TRUE){
              llply(seq(tid), function(k){
                td <- tolower(str_replace_all(tid[k],paste0(ext, '|', collapse=''), ''))
                tr <- pid[td==tolower(pid)]
                if(length(tr)>0){
                  fc <- tr
                }else{
                  th <- sort(unique(c(pid[grep(td,tolower(pid))])))
                  tc <- tolower(str_replace_all(th,paste0(ext, '|', collapse=''), ''))
                  fc <- th[td==tc]
                }
                if(length(fc)>0) {
                  fc <- fc
                }else{
                  fc <- sort(unique(c(pid[agrep(names(tid[k]),
                        names(pid),max.distance=levDist)],
                        pid[agrep(names(pid[k]),names(tid),
                        max.distance=levDist)])))
                  if(length(fc)>0){
                    fc <- fc
                  }else{
                    fc <- NA
                  }
                }
              }, .parallel=parallel)
            }else{
              tid <- NA
            }
          },.parallel=parallel)
        },.parallel=parallel)
  
  ## rename the list since llply preserved name doesn't work
  for(i in seq(tm)){
    names(tm) <- names(tmID6)
    for(j in seq(tm[[i]])){
      names(tm[[i]]) <- names(tmID6[[i]])
      if(length(tmID6[[i]][[j]])==0){
        names(tm[[i]][[j]]) <- NA
      }else{
        names(tm[[i]][[j]]) <- tmID6[[i]][[j]]
      }
    }
  };rm(i,j, ext)
    
  tmID6[[1]] <- llply(tmID6[[1]], function(x) if(length(x)==0){NA}else{x})
  tmID6[[2]] <- llply(tmID6[[2]], function(x) if(length(x)==0){NA}else{x})
    
  ## count the length of elements inside nested list
  len <- llply(seq(tm),function(i) {
           llply(seq(tm[[i]]), function(j) {
             sapply(seq(tm[[i]][[j]]), function(k) {
               length(tm[[i]][[j]][[k]])})})})
  
  ## max length element inside whole list data
  mx <- max(unlist(len))
  
  ## count the length of NA value needed inside nested list prior to fit
  len <- llply(seq(tm),function(i) {
           llply(seq(tm[[i]]), function(j) {
             sapply(seq(tm[[i]][[j]]), function(k) {
               mx-len[[i]][[j]][[k]]
         })})})
  
  df.agrep <- llply(seq(tm),function(i) {
                llply(seq(tm[[i]]), function(j) {
                  mt = t(mapply(function(x,y) c(x, rep(NA, y)), tm[[i]][[j]],
                       len[[i]][[j]])) %>% as.matrix
                  rownames(mt) = NULL
                  mt %>% data_frame
              })})
  
  dfm <- llply(df.agrep, rbind_all) %>% rbind_all %>% cbind(Sex=substr(names(unlist(tmID6)), 1, 2),
         List=str_replace_all(substring(names(unlist(tmID6)), 4), '\\.\\S+', '') %>% unlist,
         tmID=unlist(tmID6), .) %>% data_frame %>% filter(!is.na(tmID))
  rm(len, mx)
  
  matchbase <- data_frame(tmID=c(tmID1A,tmID2A), spbo=c(spboTM1A, spboTM2A), match=as.numeric(c(rep(0,
               length(tmID1A)), rep(1, length(tmID2A)))))
  
  return(list(tmID=tmID6, spbo=spboTM6, matchData=matchbase, partialData=dfm, partialDataList=tm))
}


