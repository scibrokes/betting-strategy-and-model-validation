arrDataElem <- function(dataElem=dataElem, dateID=dateID, i=i, n.col=n.col){
  ## we can adjust how many columns we needed, but the ncol of scrapped data (postponed matches) difference.
  ##   Therefore here we use 15 and split the postponed matches into 2nd list, Other sports to 3rd list
  #'@ n.data <- unlist(sapply(1:length(dataElem), function(i) i[length(dataElem)%%i==0]))
  
  dataElem = dataElem; dateID = dateID; i = i; n.col = n.col
  
  if(n.col==15){
    ## Defult suspended matches have 15 columns but there will be only 13 or 14 columns if no cards given
    ## & Date==1 (Suspended within 1st-Half), Date==2 (Suspended within Half-Time), Date==3 (Suspended within 2nd-Half).
    df <- dataElem %>% matrix(.,ncol=15,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date',
          'Finished','H.Card','Home','FTHG','FTAG','Away','A.Card','HTGoal','HT.matchID','HT.graph1','HT.graph2'))) %>%
          data_frame %>% subset(.,(nchar(as.numeric(as.character(.$matchID)))==10)&
          (str_detect(as.character(Date),'[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}')|as.numeric(as.character(Date))>0))
    ## Filter in subset to collect the completed and also suspended matches after kick-off
    if(nrow(df)>0){
      df <- df
    }else{
      df <- na.omit(data_frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
            'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')))))
      return(list(data=df,n=0))
    }
  }else if(n.col==14){
    ## Defult suspended matches have 15 columns but there will be only 13 or 14 columns if no cards given
    ## & Date==1 (Suspended within 1st-Half), Date==2 (Suspended within Half-Time), Date==3 (Suspended within 2nd-Half).
    df <- dataElem %>% matrix(.,ncol=14,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date',
          'Finished','X6','X7','X8','X9','X10','X11','X12','X13','X14'))) %>% data_frame %>%
          subset(.,(nchar(as.numeric(as.character(.$matchID)))==10)&as.numeric(as.character(Date))>0)
    if(nrow(df)>0){
      if((anyNA(as.numeric(as.character(as.vector(df[,06])))))&(!anyNA(as.numeric(as.character(as.vector(df[,10])))))){
        names(df) <- c('matchID','LeagueColor','League','Date','Finished','Home','FTHG','FTAG','Away','A.Card','HTGoal',
                     'HT.matchID','HT.graph1','HT.graph2')
        df$H.Card <- ifelse(is.na(as.numeric(as.character(as.vector(df[,06])))),0,as.numeric(as.character(as.vector(df[,06]))))
      }else if((!anyNA(as.numeric(as.character(as.vector(df[,06])))))&(anyNA(as.numeric(as.character(as.vector(df[,10])))))){
        names(df) <- c('matchID','LeagueColor','League','Date','Finished','H.Card','Home','FTHG','FTAG','Away','HTGoal',
                     'HT.matchID','HT.graph1','HT.graph2')
        df$A.Card <- ifelse(is.na(as.numeric(as.character(as.vector(df[,10])))),0,as.numeric(as.character(as.vector(df[,10]))))
      }else{ stop(paste0('please check again the function of n.col = ',n.col))}
    }else{
      df <- na.omit(data_frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
            'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')))))
      return(list(data=df,n=0))
    }
  }else if(n.col==13){
    ## Defult suspended matches have 15 columns but there will be only 13 or 14 columns if no cards given
    ## & Date==1 (Suspended within 1st-Half), Date==2 (Suspended within Half-Time), Date==3 (Suspended within 2nd-Half).
    df <- dataElem %>% matrix(.,ncol=13,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home','FTHG','FTAG',
          'Away','HTGoal','HT.matchID','HT.graph1','HT.graph2'))) %>% data_frame %>% subset(.,(nchar(as.numeric(as.character(.$matchID)))==10)&
          (str_detect(as.character(Date),'[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}')|as.numeric(as.character(Date))>0))
    ## Filter in subset to collect the completed and also suspended matches after kick-off
    if(nrow(df)>0){
      df <- data_frame(df,H.Card=0,A.Card=0)
    }else{
      df <- na.omit(data_frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
            'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')))))
      return(list(data=df,n=0))
    }
  }else if(n.col==11){
    ## Postponed matches prior to kick-off have 11 columns
    df <- dataElem %>% matrix(.,ncol=11,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home','Away',
          'HTGoal','HT.matchID','HT.graph1','HT.graph2'))) %>% data_frame %>% subset(.,(nchar(as.numeric(as.character(.$matchID)))==10)&
          as.numeric(as.character(Date))==0)
    if(nrow(df)>0){
      df <- data_frame(df, FTHG=NA, FTAG=NA, H.Card=NA, A.Card=NA)
      df$HTGoal <- 'NA-NA'
    }else{
      df <- na.omit(data_frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
            'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')))))
      return(list(data=df,n=0))
    }
  }else{ stop(paste0('Please select n.col= 11,13,14,15 as the data frames we get from spbo livescore.'))}
  
  ## change the class type to filter out the non-completed or other sports matches
  #'@ df %>% mutate_each(funs(as.character))
  df$matchID <- as.numeric(as.character(df$matchID))
  df$LeagueColor <- factor(as.character(df$LeagueColor))
  df$League <- factor(as.character(df$League))
  df$Date <- strptime(paste0(substr(dateID[i],1,4),'/',as.character(df$Date)), format='%Y/%m/%d %H:%M', tz='HKT')
  df$Finished <- as.numeric(as.character(df$Finished))
  df$H.Card <- as.numeric(as.character(df$H.Card))
  df$Home <- factor(as.character(df$Home))
  df$FTHG <- as.numeric(as.character(df$FTHG))
  df$FTAG <- as.numeric(as.character(df$FTAG))
  df$Away <- factor(as.character(df$Away))
  df$A.Card <- as.numeric(as.character(df$A.Card))
  mHTGoal <- data_frame(str_split_fixed(as.character(df$HTGoal),'-',2))
  names(mHTGoal) <- c('HTHG','HTAG')
  df <- data_frame(df[c('matchID','LeagueColor','League','Date','Finished','Home','Away','FTHG','FTAG')],mHTGoal,
                   df[c('H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')])
  df$HTGoal <- NULL
  df$HTHG <- as.numeric(as.character(df$HTHG))
  df$HTAG <- as.numeric(as.character(df$HTAG))
  df$HT.matchID <- as.numeric(as.character(df$HT.matchID))
  df$HT.graph1 <- as.numeric(as.character(df$HT.graph1))
  df$HT.graph2 <- as.numeric(as.character(df$HT.graph2))
  rm(mHTGoal)
  if(n.col==11){ df <- df }else{ df <- na.omit(df) }
  
  if(n.col==15){
    ## Length of completed soccer matches element (before str_split on HTGoals)
    n.df <- length(unlist(sapply(df[!names(df) %in% 'HTAG'], as.character)))
    ifelse(n.df == prod(dim(df[!names(df) %in% 'HTAG'])),
           'The filtered dataframe dim x*y is TALLIED with the length scrapped data element',
           stop('The filtered dataframe dim x*y is NOT TALLY with the length scrapped data element'))
  }else if(n.col==14){
    n.df <- length(unlist(sapply(df[!names(df) %in% c('A.Card','HTAG')], as.character)))
    ifelse(n.df == prod(dim(df[!names(df) %in% c('A.Card','HTAG')])),
           'The filtered dataframe dim x*y is TALLIED with the length scrapped data element',
           stop('The filtered dataframe dim x*y is NOT TALLY with the length scrapped data element'))
  }else if(n.col==13){
    n.df <- length(unlist(sapply(df[!names(df) %in% c('H.Card','A.Card','HTAG')], as.character)))
    ifelse(n.df == prod(dim(df[!names(df) %in% c('H.Card','A.Card','HTAG')])),
           'The filtered dataframe dim x*y is TALLIED with the length scrapped data element',
           stop('The filtered dataframe dim x*y is NOT TALLY with the length scrapped data element'))
  }else if(n.col==11){
    n.df <- length(unlist(sapply(df[!names(df) %in% c('FTHG','FTAG','H.Card','A.Card','HTAG')], as.character)))
    ifelse(n.df == prod(dim(df[!names(df) %in% c('FTHG','FTAG','H.Card','A.Card','HTAG')])),
           'The filtered dataframe dim x*y is TALLIED with the length scrapped data element',
           stop('The filtered dataframe dim x*y is NOT TALLY with the length scrapped data element'))
  }else{
    stop('The filtered dataframe dim x*y is NOT TALLY with the length scrapped data element')
  }
  return(list(data=df,n=n.df)) }


