listKelly <- function(filename, type = 'load', overwrite = as.logical(FALSE), rm.files = as.logical(FALSE)) {
  ## Internal function for KModelslite()
  ## 
  ## type = 'load' or type = 'save'.

  if(type == 'save') {
    if(paste0(filename, '.rds') %in% dir('./data')) {
      if(overwrite == FALSE) {
        stop('The file has exist in the directory.')
      } else {
        if(substr(filename, 1, 2) == 'K1' & substring(filename, 3) == '') {
          KM <- vKelly(dat, type = 'flat')
        } else if(substr(filename, 1, 2) == 'K2' & substring(filename, 3) == '') {
          KM <- vKelly2(dat, type = 'flat')
        } else if(substr(filename, 1, 2) == 'K1' & 
                  grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3)) == FALSE) {
          KM <- vKelly(dat, type = substring(filename, 3))
        } else if(substr(filename, 1, 2) == 'K2' & 
                  grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3)) == FALSE) {
          KM <- vKelly2(dat, type = substring(filename, 3))
        } else if(substr(filename, 1, 2) == 'K1' & 
                  grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3))) {
          dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'T', 'time', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'D', 'daily', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'O', 'dynamic')))
          dym.weight.stakes <- ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'T', 'time', 
                               ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'D', 'daily', 
                               ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'O', 'dynamic')))
          KM <- vKelly(dat, type = substring(filename, 3), dym.weight = dym.weight, 
                       dym.weight.stakes = dym.weight.stakes)
        } else if(substr(filename, 1, 2) == 'K2' & 
                  grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3))) {
          dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'T', 'time', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'D', 'daily', 
                        ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'O', 'dynamic')))
          dym.weight.stakes <- ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'T', 'time', 
                               ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'D', 'daily', 
                               ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'O', 'dynamic')))
          KM <- vKelly2(dat, type = substring(filename, 3), dym.weight = dym.weight, 
                        dym.weight.stakes = dym.weight.stakes)
        } else {
          stop('Kindly choose "K1" or "K2" to differentiate vKelly() or vKelly2().')
        }
        saveRDS(KM, file = paste0('./data/', filename, '.rds'))
        if(rm.files == TRUE) {
          rm(KM)
        } else {
          return(KM)
        }
      }
    } else {
      stop(filename, ' is not inside directory "./data/".')
    }
    
  } else if(type == 'load') {
    if(paste0(filename, '.rds') %in% dir('./data')) {
      KM <- readRDS(file = paste0('./data/', filename, '.rds'))
      return(KM)
      
    } else {
      if(substr(filename, 1, 2) == 'K1' & substring(filename, 3) == '') {
        KM <- vKelly(dat, type = 'flat')
      } else if(substr(filename, 1, 2) == 'K2' & substring(filename, 3) == '') {
        KM <- vKelly2(dat, type = 'flat')
      } else if(substr(filename, 1, 2) == 'K1' & 
                grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3)) == FALSE) {
        KM <- vKelly(dat, type = substring(filename, 3))
      } else if(substr(filename, 1, 2) == 'K2' & 
                grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3)) == FALSE) {
        KM <- vKelly2(dat, type = substring(filename, 3))
      } else if(substr(filename, 1, 2) == 'K1' & 
                grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3))) {
        dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'T', 'time', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'D', 'daily', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'O', 'dynamic')))
        dym.weight.stakes <- ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'T', 'time', 
                             ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'D', 'daily', 
                             ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'O', 'dynamic')))
        KM <- vKelly(dat, type = substring(filename, 3), dym.weight = dym.weight, 
                     dym.weight.stakes = dym.weight.stakes)
      } else if(substr(filename, 1, 2) == 'K2' & 
                grepl('TT|TO|DD|DT|DO|OD|OT|OO', substring(filename, 3))) {
        dym.weight <- ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'T', 'time', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'D', 'daily', 
                      ifelse(substr(filename, nchar(filename) - 1, nchar(Knames) - 1) == 'O', 'dynamic')))
        dym.weight.stakes <- ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'T', 'time', 
                             ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'D', 'daily', 
                             ifelse(substr(filename, nchar(filename), nchar(Knames)) == 'O', 'dynamic')))
        KM <- vKelly2(dat, type = substring(filename, 3), dym.weight = dym.weight, 
                      dym.weight.stakes = dym.weight.stakes)
      } else {
        stop('Kindly choose "K1" or "K2" to differentiate vKelly() or vKelly2().')
      }
      saveRDS(KM, file = paste0('./data/', filename, '.rds'))
      return(KM)
    }
  }
}
