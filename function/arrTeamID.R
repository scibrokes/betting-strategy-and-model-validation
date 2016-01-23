arrTeamID <- function(mbase, spboData, method=c('osa', 'lv', 'dl', 'hamming',
                      'lcs', 'qgram', 'cosine', 'jaccard', 'jw', 'soundex'), levDist=0.1, parallel=FALSE){
  ## I just simply apply the `split` function to categorize the teams' name inside `makeList`.
  ## 
  ## STEP 1) Filter the exactly match (or different Capital letter teams' name) teams' name with spboTeamID,
  ##    seperate it to ease the processing time to find the approximate teams' name in section (2).
  ## 
  ## STEP 2) Apply regular expression to filter/seperate the women team, B team or U21 etc prior to partial 
  ##    matching the most approximate teams' name (teams' name others than in STEP 1).
  
  ## Setting to omitt all warnings
  options(warn=-1)
  
  methd <- c('osa', 'lv', 'dl', 'hamming', 'lcs', 'qgram', 'cosine', 'jaccard', 'jw', 'soundex')
  if(!all(method %in% methd)){
    stop('Please select one or more stringdist.method from above:', cat(methd))
  }
  
  suppressAll(source('function/makeList.R'))
  ## Since the elements are not much enough but list quit a number, 
  ##   just set parallel=FALSE will be faster few minutes.
  dfm <- makeList(mbase, spboData, levDist=levDist, parallel=parallel)
  
  ## STEP 3) Option stringdistList
  ## Load the stringdistList() function
  suppressAll(source('function/stringdistList.R'))
  #'@ parallel=TRUE ## parallel=FALSE will spend above 30 minutes, but parallel=TRUE will hanging
  df2 <- llply(seq(nrow(dfm$partialData)), function(i){
           stringdistList(method=method, tmID_A=dfm$partialData$tmID[i], 
                     tmID_B=na.omit(unlist(dfm$partialData[i, 4:ncol(dfm$partialData)])), parallel=parallel)
           }, .parallel=parallel) %>% rbind_all
                     ## levDist=0.1 depreciated but auto turned to Inf, here I omit to set levDist
         
  df1 <- dfm$matchData %>% .[2:ncol(.)] %>% as.list %>% rep(., 10) %>% 
         data_frame(dfm$matchData[, 1], .)
  names(df1) <- names(df2)
  dfm <- rbind(df1, df2) %>% mutate_each(funs(as.character))
  rm(df1, df2)
  
  return(list(result=dfm, method=method, levDist=levDist))
}


