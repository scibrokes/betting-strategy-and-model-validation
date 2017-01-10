listKelly <- function(filename, type = 'load', load.fast = as.logical(TRUE), 
                      overwrite = as.logical(FALSE), rm.files = as.logical(FALSE), 
                      .print = FALSE) {
  ## Internal function for KModelslite()
  ## 
  ## type = 'load' or type = 'save'.
  ## load.fast = TRUE or load.fast = FALSE only applicable to type = 'load'. Load fast will only read the 
  ##   Kelly models inside the list of datasets.
  ## ================== Load Packages ================================
  suppressPackageStartupMessages(library('magrittr'))
  suppressPackageStartupMessages(library('plyr'))
  suppressPackageStartupMessages(library('tidyverse'))
  suppressPackageStartupMessages(library('doParallel'))
  suppressPackageStartupMessages(library('rlist'))
  source('./function/vKelly.R', local = TRUE)
  source('./function/vKelly2.R', local = TRUE)
  
  ## ================== Kelly models ================================
  allKMnames <- c('K1', 'K2', 'K1W1', 'K1W2', 'K2W1', 'K2W2', #6
                  'K1W1WS1', 'K1W1WS2', 'K1W2WS1', 'K1W2WS2', #4 #10
                  'K2W1WS1', 'K2W1WS2', 'K2W2WS1', 'K2W2WS2', #4 #14
                  'K1D1', 'K1D2', 'K2D1', 'K2D2', #4 #18
                  'K1DWS1', 'K1DWS2', 'K2DWS1', 'K2DWS2', #4 #22
                  'K1D1WS1', 'K1D1WS2', 'K1D2WS1', 'K1D2WS2', #4 #26
                  'K2D1WS1', 'K2D1WS2', 'K2D2WS1', 'K2D2WS2', #4 #30
                  'K1W1DWS1', 'K1W1DWS2', 'K1W2DWS1', 'K1W2DWS2', #4 #34
                  'K2W1DWS1', 'K2W1DWS2', 'K2W2DWS1', 'K2W2DWS2', #4 #38
                  'K1D1DWS1', 'K1D1DWS2', 'K1D2DWS1', 'K1D2DWS2', #4 #42
                  'K2D1DWS1', 'K2D1DWS2', 'K2D2DWS1', 'K2D2DWS2', #4 #46
                  'K1D1DWS1TT', 'K1D1DWS2TT', 'K1D2DWS1TT', 'K1D2DWS2TT', #4 #50
                  'K2D1DWS1TT', 'K2D1DWS2TT', 'K2D2DWS1TT', 'K2D2DWS2TT', #4 #54
                  'K1D1DWS1TO', 'K1D1DWS2TO', 'K1D2DWS1TO', 'K1D2DWS2TO', #4 #58
                  'K2D1DWS1TO', 'K2D1DWS2TO', 'K2D2DWS1TO', 'K2D2DWS2TO', #4 #62
                  'K1D1DWS1DD', 'K1D1DWS2DD', 'K1D2DWS1DD', 'K1D2DWS2DD', #4 #66
                  'K2D1DWS1DD', 'K2D1DWS2DD', 'K2D2DWS1DD', 'K2D2DWS2DD', #4 #70
                  'K1D1DWS1DT', 'K1D1DWS2DT', 'K1D2DWS1DT', 'K1D2DWS2DT', #4 #74
                  'K2D1DWS1DT', 'K2D1DWS2DT', 'K2D2DWS1DT', 'K2D2DWS2DT', #4 #78
                  'K1D1DWS1DO', 'K1D1DWS2DO', 'K1D2DWS1DO', 'K1D2DWS2DO', #4 #82
                  'K2D1DWS1DO', 'K2D1DWS2DO', 'K2D2DWS1DO', 'K2D2DWS2DO', #4 #86
                  'K1D1DWS1OD', 'K1D1DWS2OD', 'K1D2DWS1OD', 'K1D2DWS2OD', #4 #90
                  'K2D1DWS1OD', 'K2D1DWS2OD', 'K2D2DWS1OD', 'K2D2DWS2OD', #4 #94
                  'K1D1DWS1OT', 'K1D1DWS2OT', 'K1D2DWS1OT', 'K1D2DWS2OT', #4 #98
                  'K2D1DWS1OT', 'K2D1DWS2OT', 'K2D2DWS1OT', 'K2D2DWS2OT', #4 #102
                  'K1D1DWS1OO', 'K1D1DWS2OO', 'K1D2DWS1OO', 'K1D2DWS2OO', #4 #106
                  'K2D1DWS1OO', 'K2D1DWS2OO', 'K2D2DWS1OO', 'K2D2DWS2OO') #4 #110
  
  filenameK1 <- allKMnames[(substr(allKMnames, 1, 2) == 'K1') & (nchar(allKMnames) == 2)]
  filenameK2 <- allKMnames[(substr(allKMnames, 1, 2) == 'K2') & (nchar(allKMnames) == 2)]
  filenameKW1 <- allKMnames[(substr(allKMnames, 1, 2) == 'K1') & (nchar(allKMnames) > 2) & 
                            grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(allKMnames, 3)) == FALSE]
  filenameKW2 <- allKMnames[(substr(allKMnames, 1, 2) == 'K2') & (nchar(allKMnames) > 2) & 
                            grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(allKMnames, 3)) == FALSE]
  filenameKD1 <- allKMnames[(substr(allKMnames, 1, 2) == 'K1') & (nchar(allKMnames) > 2) & 
             grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(allKMnames, 3))]
  filenameKD2 <- allKMnames[(substr(allKMnames, 1, 2) == 'K2') & (nchar(allKMnames) > 2) & 
             grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(allKMnames, 3))]
  
  if(type == 'save') {
    ## ================== Save Kelly models ================================
    
    if(paste0(filename, '.rds') %in% dir('./data')) {
      if(overwrite == FALSE) {
        stop('The file has exist in the directory.')
      } else {
        if(filename %in% filenameK1) {
          KM <- vKelly(dat, type = 'flat')
          
        } else if(filename %in% filenameK2) {
          KM <- vKelly2(dat, type = 'flat')
          
        } else if(filename %in% filenameKW1) {
          KM <- vKelly(dat, type = substring(filename, 3))
          
        } else if(filename %in% filenameKW2) {
          KM <- vKelly2(dat, type = substring(filename, 3))
          
        } else if(filename %in% filenameKD1) {
          dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'T', 'time', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'D', 'daily', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'O', 'dynamic')))
          dym.weight.stakes <- ifelse(substring(filename, nchar(filename)) == 'T', 'time', 
                               ifelse(substring(filename, nchar(filename)) == 'D', 'daily', 
                               ifelse(substring(filename, nchar(filename)) == 'O', 'dynamic')))
          KM <- vKelly(dat, type = substr(filename, 3, nchar(filename) - 2), dym.weight = dym.weight, 
                       dym.weight.stakes = dym.weight.stakes)
          
        } else if(filename %in% filenameKD2) {
          dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'T', 'time', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'D', 'daily', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'O', 'dynamic')))
          dym.weight.stakes <- ifelse(substring(filename, nchar(filename)) == 'T', 'time', 
                               ifelse(substring(filename, nchar(filename)) == 'D', 'daily', 
                               ifelse(substring(filename, nchar(filename)) == 'O', 'dynamic')))
          KM <- vKelly2(dat, type = substr(filename, 3, nchar(filename) - 2), dym.weight = dym.weight, 
                        dym.weight.stakes = dym.weight.stakes)
          
        } else {
          stop('Kindly choose "K1" or "K2" to differentiate vKelly() or vKelly2().')
        }
        
        saveRDS(KM, file = paste0('./data/', filename, '.rds'))
        if(.print == TRUE) cat(paste0('"./data/', filename, '.rds" had saved.\n'))
        if(rm.files == TRUE) { rm(KM) } else { return(KM) }
      }
    } else {
      
      if(filename %in% filenameK1) {
        KM <- vKelly(dat, type = 'flat')
        
      } else if(filename %in% filenameK2) {
        KM <- vKelly2(dat, type = 'flat')
        
      } else if(filename %in% filenameKW1) {
        KM <- vKelly(dat, type = substring(filename, 3))
        
      } else if(filename %in% filenameKW2) {
        KM <- vKelly2(dat, type = substring(filename, 3))
        
      } else if(filename %in% filenameKD1) {
        dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'T', 'time', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'D', 'daily', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'O', 'dynamic')))
        dym.weight.stakes <- ifelse(substring(filename, nchar(filename)) == 'T', 'time', 
                             ifelse(substring(filename, nchar(filename)) == 'D', 'daily', 
                             ifelse(substring(filename, nchar(filename)) == 'O', 'dynamic')))
        KM <- vKelly(dat, type = substr(filename, 3, nchar(filename) - 2), dym.weight = dym.weight, 
                     dym.weight.stakes = dym.weight.stakes)
        
      } else if(filename %in% filenameKD2) {
        dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'T', 'time', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'D', 'daily', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'O', 'dynamic')))
        dym.weight.stakes <- ifelse(substring(filename, nchar(filename)) == 'T', 'time', 
                             ifelse(substring(filename, nchar(filename)) == 'D', 'daily', 
                             ifelse(substring(filename, nchar(filename)) == 'O', 'dynamic')))
        KM <- vKelly2(dat, type = substr(filename, 3, nchar(filename) - 2), dym.weight = dym.weight, 
                      dym.weight.stakes = dym.weight.stakes)
        
      } else {
        stop('Kindly choose "K1" or "K2" to differentiate vKelly() or vKelly2().')
      }
      
      saveRDS(KM, file = paste0('./data/', filename, '.rds'))
      if(.print == TRUE) cat(paste0('"./data/', filename, '.rds" had saved.\n'))
      if(rm.files == TRUE) { rm(KM) } else { return(KM) }
      
      #'@ stop(filename, ' is not inside directory "./data/".')
    }
    
  } else if(type == 'load') {
    ## ================== Load Kelly models ================================
    
    if(paste0(filename, '.rds') %in% dir('./data')) {
      if(load.fast == TRUE) {
        KM <- readRDS(file = paste0('./data/', filename, '.rds'))[c('Kelly1', 'Kelly2', 'Kelly3', 'Kelly4')]
      } else {
        KM <- readRDS(file = paste0('./data/', filename, '.rds'))
      }
      return(KM)
      
    } else {
      if(filename %in% filenameK1) {
        KM <- vKelly(dat, type = 'flat')
        
      } else if(filename %in% filenameK2) {
        KM <- vKelly2(dat, type = 'flat')
        
      } else if(filename %in% filenameKW1) {
        KM <- vKelly(dat, type = substring(filename, 3))
        
      } else if(filename %in% filenameKW2) {
        KM <- vKelly2(dat, type = substring(filename, 3))
        
      } else if(filename %in% filenameKD1) {
        dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'T', 'time', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'D', 'daily', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'O', 'dynamic')))
        dym.weight.stakes <- ifelse(substring(filename, nchar(filename)) == 'T', 'time', 
                             ifelse(substring(filename, nchar(filename)) == 'D', 'daily', 
                             ifelse(substring(filename, nchar(filename)) == 'O', 'dynamic')))
        KM <- vKelly(dat, type = substr(filename, 3, nchar(filename) - 2), dym.weight = dym.weight, 
                     dym.weight.stakes = dym.weight.stakes)
        
      } else if(filename %in% filenameKD2) {
        dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'T', 'time', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'D', 'daily', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(filename) - 1) == 'O', 'dynamic')))
        dym.weight.stakes <- ifelse(substring(filename, nchar(filename)) == 'T', 'time', 
                             ifelse(substring(filename, nchar(filename)) == 'D', 'daily', 
                             ifelse(substring(filename, nchar(filename)) == 'O', 'dynamic')))
        KM <- vKelly2(dat, type = substr(filename, 3, nchar(filename) - 2), dym.weight = dym.weight, 
                      dym.weight.stakes = dym.weight.stakes)
      } else {
        stop('Kindly choose "K1" or "K2" to differentiate vKelly() or vKelly2().')
      }
      saveRDS(KM, file = paste0('./data/', filename, '.rds'))
      return(KM)
    }
  }
}
