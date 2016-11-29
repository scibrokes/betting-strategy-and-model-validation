KModels <- function(action = 'load', rm.files = as.logical(TRUE), 
                    overwrite = as.logical(FALSE)) {
  ## action = 'load' or action = 'save'.
  ## You can choose either load or save all risk profiles of Kelly models. It will 
  ##   auto calculate and save/load if the file(s) is(are) not exist.
  ## rm.files = TRUE or rm.files = FALSE. You can choose to remove all files from 
  ##   environment once saved. (applicable only for action = "save")
  ## overwrite = FALSE or overwrite = TRUE, you can choose if you want to overwrite 
  ##   if there has any existing files in directory. (applicable only for action = "save")
  
  ## ================== Load Function ================================
  ## Simulate and save the league profiling : all, annual, daily, time or dynamic.
  ## You just need to run once to complete whole weighted parameters measurement, 
  ##   and then you can just execute the === Kelly Models === section since it will 
  ##   auto read saved profiles which will be more efficient compare to use vKelly() 
  ##   to calculate the weighted parameters and get the result from start to end 
  ##   will take few minutes time to complete.
  source('./function/vKelly.R', local = TRUE)
  source('./function/vKelly2.R', local = TRUE)
  
  ## ================== Load Data ================================
  ## Load saved dataset to save the loading time.
  ##   directly load the dataset from running chunk `read-data-summary-table` 
  ##   and also chunk `scrap-data`. The spboData for filtering leagues and 
  ##   matches scores purpose.
  if('shinyData.RData' %in% dir('./KellyApps')) {
    load('./KellyApps/shinyData.RData')
  } else {
    stop('Kindly refer to chunk named `load-data` inside file `Betting Strategy and Ⓜodel Validation - Part I.Rmd`.')
  }
  
  ## ================== Data Validation ================================
  overwrite = as.logical(overwrite)
  rm.files = as.logical(rm.files)
  
  if(action == 'save') {
    ## ================== Save Models ================================
    
    ## --------------- 01. K1 ------------------------
    ## Reserved Stakes Kelly Models.
    if('K1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1 <- vKelly(dat)
          saveRDS(K1 , file = './data/K1.rds')
          if(rm.files == TRUE) rm(K1)
        }
      } else {
        stop('Kindly apply K1 <- vKelly(dat) to measure the Kelly model.')
      }
    } else {
      K1 <- vKelly(dat)
      saveRDS(K1 , file = './data/K1.rds')
      if(rm.files == TRUE) rm(K1)
    }
    
    ## --------------- 02. K2 ------------------------
    ## Reserved EM Probabilities Kelly Models.
    if('K2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2 <- vKelly2(dat)
          saveRDS(K2, file = './data/K2.rds')
          if(rm.files == TRUE) rm(K2)
        }
      } else {
        stop('Kindly apply K2 <- vKelly2(dat) to measure the Kelly model.')
      }
    } else {
      K2 <- vKelly2(dat)
      saveRDS(K2, file = './data/K2.rds')
      if(rm.files == TRUE) rm(K2)
    }
    
    ## --------------- 03. K1W1 ------------------------
    ## Reserved Stakes Kelly Models with Weight past year annual theta constant value.
    ##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K1W1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W1 <- vKelly(dat, type = 'W1')
          saveRDS(K1W1, file = './data/K1W1.rds')
          if(rm.files == TRUE) rm(K1W1)
        }
      } else {
        stop('Kindly apply K1W1 <- vKelly(dat, type = "W1") to measure the Kelly model.')
      }
    } else {
      K1W1 <- vKelly(dat, type = 'W1')
      saveRDS(K1W1, file = './data/K1W1.rds')
      if(rm.files == TRUE) rm(K1W1)
    }
    
    ## --------------- 04. K1W2 ------------------------
    ## Reserved Stakes Kelly Models with Weight past year annual weighted hdp parameter.
    ##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K1W2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W2 <- vKelly(dat, type = 'W2')
          saveRDS(K1W2, file = './data/K1W2.rds')
          if(rm.files == TRUE) rm(K1W2)
        }
      } else {
        stop('Kindly apply K1W2 <- vKelly(dat, type = "W2") to measure the Kelly model.')
      }
    } else {  
      K1W2 <- vKelly(dat, type = 'W2')
      saveRDS(K1W2, file = './data/K1W2.rds')
      if(rm.files == TRUE) rm(K1W2)
    }
    
    ## --------------- 05. K2W1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value.
    ##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K2W1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W1 <- vKelly2(dat, type = 'W1')
          saveRDS(K2W1, file = './data/K2W1.rds')
          if(rm.files == TRUE) rm(K2W1)
        }
      } else {
        stop('Kindly apply K2W1 <- vKelly2(dat, type = "W1") to measure the Kelly model.')
      }
    } else {
      K2W1 <- vKelly2(dat, type = 'W1')
      saveRDS(K2W1, file = './data/K2W1.rds')
      if(rm.files == TRUE) rm(K2W1)
    }
    
    ## --------------- 06. K2W2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual weighted hdp parameter.
    ##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K2W2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W2 <- vKelly2(dat, type = 'W2')
          saveRDS(K2W2, file = './data/K2W2.rds')
          if(rm.files == TRUE) rm(K2W2)
        }
      } else {
        stop('Kindly apply K2W2 <- vKelly2(dat, type = "W2") to measure the Kelly model.')
      }
    } else {
      K2W2 <- vKelly2(dat, type = 'W2')
      saveRDS(K2W2, file = './data/K2W2.rds')
      if(rm.files == TRUE) rm(K2W2)
    }
    
    ## --------------- 07. K1W1WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K1W1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W1WS1 <- vKelly(dat, type = 'W1WS1')
          saveRDS(K1W1WS1, file = './data/K1W1WS1.rds')
          if(rm.files == TRUE) rm(K1W1WS1)
        }
      } else {
        stop('Kindly apply K1W1WS1 <- vKelly(dat, type = "W1WS1") to measure the Kelly model.')
      }
    } else {
      K1W1WS1 <- vKelly(dat, type = 'W1WS1')
      saveRDS(K1W1WS1, file = './data/K1W1WS1.rds')
      if(rm.files == TRUE) rm(K1W1WS1)
    }
    
    ## --------------- 08. K1W1WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K1W1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W1WS2 <- vKelly(dat, type = 'W1WS2')
          saveRDS(K1W1WS2, file = './data/K1W1WS2.rds')
          if(rm.files == TRUE) rm(K1W1WS2)
        }
      } else {
        stop('Kindly apply K1W1WS2 <- vKelly(dat, type = "W1WS2") to measure the Kelly model.')
      }
    } else {
      K1W1WS2 <- vKelly(dat, type = 'W1WS2')
      saveRDS(K1W1WS2, file = './data/K1W1WS2.rds')
      if(rm.files == TRUE) rm(K1W1WS2)
    }
    
    ## --------------- 09. K1W2WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K1W2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W2WS1 <- vKelly(dat, type = 'W2WS1')
          saveRDS(K1W2WS1, file = './data/K1W2WS1.rds')
          if(rm.files == TRUE) rm(K1W2WS1)
        }
      } else {
        stop('Kindly apply K1W2WS1 <- vKelly(dat, type = "W2WS1") to measure the Kelly model.')
      }
    } else {
      K1W2WS1 <- vKelly(dat, type = 'W2WS1')
      saveRDS(K1W2WS1, file = './data/K1W2WS1.rds')
      if(rm.files == TRUE) rm(K1W2WS1)
    }
    
    ## --------------- 10. K1W2WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K1W2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W2WS2 <- vKelly2(dat, type = 'W2WS2')
          saveRDS(K1W2WS2, file = './data/K1W2WS2.rds')
          if(rm.files == TRUE) rm(K1W2WS2)
        }
      } else {
        stop('Kindly apply K1W2WS2 <- vKelly2(dat, type = "W2WS2") to measure the Kelly model.')
      }
    } else {
      K1W2WS2 <- vKelly2(dat, type = 'W2WS2')
      saveRDS(K1W2WS2, file = './data/K1W2WS2.rds')
      if(rm.files == TRUE) rm(K1W2WS2)
    }
    
    ## --------------- 11. K2W1WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K2W1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W1WS1 <- vKelly2(dat, type = 'W1WS1')
          saveRDS(K2W1WS1, file = './data/K2W1WS1.rds')
          if(rm.files == TRUE) rm(K2W1WS1)
        }
      } else {
        stop('Kindly apply K2W1WS1 <- vKelly2(dat, type = "W1WS1") to measure the Kelly model.')
      }
    } else {
      K2W1WS1 <- vKelly2(dat, type = 'W1WS1')
      saveRDS(K2W1WS1, file = './data/K2W1WS1.rds')
      if(rm.files == TRUE) rm(K2W1WS1)
    }
    
    ## --------------- 12. K2W1WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K2W1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W1WS2 <- vKelly2(dat, type = 'W1WS2')
          saveRDS(K2W1WS2, file = './data/K2W1WS2.rds')
          if(rm.files == TRUE) rm(K2W1WS2)
        }
      } else {
        stop('Kindly apply K2W1WS2 <- vKelly2(dat, type = "W1WS2") to measure the Kelly model.')
      }
    } else {
      K2W1WS2 <- vKelly2(dat, type = 'W1WS2')
      saveRDS(K2W1WS2, file = './data/K2W1WS2.rds')
      if(rm.files == TRUE) rm(K2W1WS2)
    }
    
    ## --------------- 13. K2W2WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K2W2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W2WS1 <- vKelly2(dat, type = 'W2WS1')
          saveRDS(K2W2WS1, file = './data/K2W2WS1.rds')
          if(rm.files == TRUE) rm(K2W2WS1)
        }
      } else {
        stop('Kindly apply K2W2WS1 <- vKelly2(dat, type = "W2WS1") to measure the Kelly model.')
      }
    } else {
      K2W2WS1 <- vKelly2(dat, type = 'W2WS1')
      saveRDS(K2W2WS1, file = './data/K2W2WS1.rds')
      if(rm.files == TRUE) rm(K2W2WS1)
    }
    
    ## --------------- 14. K2W2WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K2W2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W2WS2 <- vKelly2(dat, type = 'W2WS2')
          saveRDS(K2W2WS2, file = './data/K2W2WS2.rds')
          if(rm.files == TRUE) rm(K2W2WS2)
        }
      } else {
        stop('Kindly apply K2W2WS2 <- vKelly2(dat, type = "W2WS2") to measure the Kelly model.')
      }
    } else {
      K2W2WS2 <- vKelly2(dat, type = 'W2WS2')
      saveRDS(K2W2WS2, file = './data/K2W2WS2.rds')
      if(rm.files == TRUE) rm(K2W2WS2)
    }
    
    ## --------------- Default Dynamic ------------------------
    ## --------------- 15. K1D1 ------------------------
    if('K1D1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1 <- vKelly(dat, type = 'D1')
          saveRDS(K1D1, file = './data/K1D1.rds')
          if(rm.files == TRUE) rm(K1D1)
        }
      } else {
        stop('Kindly apply K1D1 <- vKelly(dat, type = "D1") to measure the Kelly model.')
      }
    } else {
      K1D1 <- vKelly(dat, type = 'D1')
      saveRDS(K1D1, file = './data/K1D1.rds')
      if(rm.files == TRUE) rm(K1D1)
    }
    
    ## --------------- 16. K1D2 ------------------------
    if('K1D2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2 <- vKelly(dat, type = 'D2')
          saveRDS(K1D2, file = './data/K1D2.rds')
          if(rm.files == TRUE) rm(K1D2)
        }
      } else {
        stop('Kindly apply K1D2 <- vKelly(dat, type = "D2") to measure the Kelly model.')
      }
    } else {
      K1D2 <- vKelly(dat, type = 'D2')
      saveRDS(K1D2, file = './data/K1D2.rds')
      if(rm.files == TRUE) rm(K1D2)
    }
    
    ## --------------- 17. K2D1 ------------------------
    if('K2D1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1 <- vKelly2(dat, type = 'D1')
          saveRDS(K2D1, file = './data/K2D1.rds')
          if(rm.files == TRUE) rm(K2D1)
        }
      } else {
        stop('Kindly apply K2D1 <- vKelly2(dat, type = "D1") to measure the Kelly model.')
      }
    } else {
      K2D1 <- vKelly2(dat, type = 'D1')
      saveRDS(K2D1, file = './data/K2D1.rds')
      if(rm.files == TRUE) rm(K2D1)
    }
    
    ## --------------- 18. K2D2 ------------------------
    if('K2D2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2 <- vKelly2(dat, type = 'D2')
          saveRDS(K2D2, file = './data/K2D2.rds')
          if(rm.files == TRUE) rm(K2D2)
        }
      } else {
        stop('Kindly apply K2D2 <- vKelly2(dat, type = "D2") to measure the Kelly model.')
      }
    } else {
      K2D2 <- vKelly2(dat, type = 'D2')
      saveRDS(K2D2, file = './data/K2D2.rds')
      if(rm.files == TRUE) rm(K2D2)
    }
    
    ## --------------- 19. K1DWS1 ------------------------
    if('K1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1DWS1 <- vKelly(dat, type = 'DWS1')
          saveRDS(K1DWS1, file = './data/K1DWS1.rds')
          if(rm.files == TRUE) rm(K1DWS1)
        }
      } else {
        stop('Kindly apply K1DWS1 <- vKelly(dat, type = "DWS1") to measure the Kelly model.')
      }
    } else {
      K1DWS1 <- vKelly(dat, type = 'DWS1')
      saveRDS(K1DWS1, file = './data/K1DWS1.rds')
      if(rm.files == TRUE) rm(K1DWS1)
    }
    
    ## --------------- 20. K1DWS2 ------------------------
    if('K1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1DWS2 <- vKelly(dat, type = 'DWS2')
          saveRDS(K1DWS2, file = './data/K1DWS2.rds')
          if(rm.files == TRUE) rm(K1DWS2)
        }
      } else {
        stop('Kindly apply K1DWS2 <- vKelly(dat, type = "DWS2") to measure the Kelly model.')
      }
    } else {
      K1DWS2 <- vKelly(dat, type = 'DWS2')
      saveRDS(K1DWS2, file = './data/K1DWS2.rds')
      if(rm.files == TRUE) rm(K1DWS2)
    }
    
    ## --------------- 21. K2DWS1 ------------------------
    if('K2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2DWS1 <- vKelly2(dat, type = 'DWS1')
          saveRDS(K2DWS1, file = './data/K2DWS1.rds')
          if(rm.files == TRUE) rm(K2DWS1)
        }
      } else {
        stop('Kindly apply K2DWS1 <- vKelly2(dat, type = "DWS1") to measure the Kelly model.')
      }
    } else {
      K2DWS1 <- vKelly2(dat, type = 'DWS1')
      saveRDS(K2DWS1, file = './data/K2DWS1.rds')
      if(rm.files == TRUE) rm(K2DWS1)
    }
    
    ## --------------- 22. K2DWS2 ------------------------
    if('K2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2DWS2 <- vKelly2(dat, type = 'DWS2')
          saveRDS(K2DWS2, file = './data/K2DWS2.rds')
          if(rm.files == TRUE) rm(K2DWS2)
        }
      } else {
        stop('Kindly apply K2DWS2 <- vKelly2(dat, type = "DWS2") to measure the Kelly model.')
      }
    } else {
      K2DWS2 <- vKelly2(dat, type = 'DWS2')
      saveRDS(K2DWS2, file = './data/K2DWS2.rds')
      if(rm.files == TRUE) rm(K2DWS2)
    }
    
    ## --------------- 23. K1D1WS1 ------------------------
    if('K1D1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1WS1 <- vKelly(dat, type = 'D1WS1')
          saveRDS(K1D1WS1, file = './data/K1D1WS1.rds')
          if(rm.files == TRUE) rm(K1D1WS1)
        }
      } else {
        stop('Kindly apply K1D1WS1 <- vKelly(dat, type = "D1WS1") to measure the Kelly model.')
      }
    } else {
      K1D1WS1 <- vKelly(dat, type = 'D1WS1')
      saveRDS(K1D1WS1, file = './data/K1D1WS1.rds')
      if(rm.files == TRUE) rm(K1D1WS1)
    }
    
    ## --------------- 24. K1D1WS2 ------------------------
    if('K1D1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1WS2 <- vKelly(dat, type = 'D1WS2')
          saveRDS(K1D1WS2, file = './data/K1D1WS2.rds')
          if(rm.files == TRUE) rm(K1D1WS2)
        }
      } else {
        stop('Kindly apply K1D1WS2 <- vKelly(dat, type = "D1WS2") to measure the Kelly model.')
      }
    } else {
      K1D1WS2 <- vKelly(dat, type = 'D1WS2')
      saveRDS(K1D1WS2, file = './data/K1D1WS2.rds')
      if(rm.files == TRUE) rm(K1D1WS2)
    }
    
    ## --------------- 25. K1D2WS1 ------------------------
    if('K1D2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2WS1 <- vKelly(dat, type = 'D2WS1')
          saveRDS(K1D2WS1, file = './data/K1D2WS1.rds')
          if(rm.files == TRUE) rm(K1D2WS1)
        }
      } else {
        stop('Kindly apply K1D2WS1 <- vKelly(dat, type = "D2WS1") to measure the Kelly model.')
      }
    } else {
      K1D2WS1 <- vKelly(dat, type = 'D2WS1')
      saveRDS(K1D2WS1, file = './data/K1D2WS1.rds')
      if(rm.files == TRUE) rm(K1D2WS1)
    }
    
    ## --------------- 26. K1D2WS2 ------------------------
    if('K1D2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2WS2 <- vKelly(dat, type = 'D2WS2')
          saveRDS(K1D2WS2, file = './data/K1D2WS2.rds')
          if(rm.files == TRUE) rm(K1D2WS2)
        }
      } else {
        stop('Kindly apply K1D2WS2 <- vKelly(dat, type = "D2WS2") to measure the Kelly model.')
      }
    } else {
      K1D2WS2 <- vKelly(dat, type = 'D2WS2')
      saveRDS(K1D2WS2, file = './data/K1D2WS2.rds')
      if(rm.files == TRUE) rm(K1D2WS2)
    }
    
    ## --------------- 27. K2D1WS1 ------------------------
    if('K2D1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1WS1 <- vKelly2(dat, type = 'D1WS1')
          saveRDS(K2D1WS1, file = './data/K2D1WS1.rds')
          if(rm.files == TRUE) rm(K2D1WS1)
        }
      } else {
        stop('Kindly apply K2D1WS1 <- vKelly2(dat, type = "D1WS1") to measure the Kelly model.')
      }
    } else {
      K2D1WS1 <- vKelly2(dat, type = 'D1WS1')
      saveRDS(K2D1WS1, file = './data/K2D1WS1.rds')
      if(rm.files == TRUE) rm(K2D1WS1)
    }
    
    ## --------------- 28. K2D1WS2 ------------------------
    if('K2D1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1WS2 <- vKelly2(dat, type = 'D1WS2')
          saveRDS(K2D1WS2, file = './data/K2D1WS2.rds')
          if(rm.files == TRUE) rm(K2D1WS2)
        }
      } else {
        stop('Kindly apply K2D1WS2 <- vKelly2(dat, type = "D1WS2") to measure the Kelly model.')
      }
    } else {
      K2D1WS2 <- vKelly2(dat, type = 'D1WS2')
      saveRDS(K2D1WS2, file = './data/K2D1WS2.rds')
      if(rm.files == TRUE) rm(K2D1WS2)
    }
    
    ## --------------- 29. K2D2WS1 ------------------------
    if('K2D2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2WS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2WS1 <- vKelly2(dat, type = 'D2WS1')
          saveRDS(K2D2WS1, file = './data/K2D2WS1.rds')
          if(rm.files == TRUE) rm(K2D2WS1)
        }
      } else {
        stop('Kindly apply K2D2WS1 <- vKelly2(dat, type = "D2WS1") to measure the Kelly model.')
      }
    } else {
      K2D2WS1 <- vKelly2(dat, type = 'D2WS1')
      saveRDS(K2D2WS1, file = './data/K2D2WS1.rds')
      if(rm.files == TRUE) rm(K2D2WS1)
    }
    
    ## --------------- 30. K2D2WS2 ------------------------
    if('K2D2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2WS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2WS2 <- vKelly2(dat, type = 'D2WS2')
          saveRDS(K2D2WS2, file = './data/K2D2WS2.rds')
          if(rm.files == TRUE) rm(K2D2WS2)
        }
      } else {
        stop('Kindly apply K2D2WS2 <- vKelly2(dat, type = "D2WS2") to measure the Kelly model.')
      }
    } else {
      K2D2WS2 <- vKelly2(dat, type = 'D2WS2')
      saveRDS(K2D2WS2, file = './data/K2D2WS2.rds')
      if(rm.files == TRUE) rm(K2D2WS2)
    }
    
    ## --------------- 31. K1W1DWS1 ------------------------
    if('K1W1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W1DWS1 <- vKelly(dat, type = 'W1DWS1')
          saveRDS(K1W1DWS1, file = './data/K1W1DWS1.rds')
          if(rm.files == TRUE) rm(K1W1DWS1)
        }
      } else {
        stop('Kindly apply K1W1DWS1 <- vKelly(dat, type = "W1DWS1") to measure the Kelly model.')
      }
    } else {
      K1W1DWS1 <- vKelly(dat, type = 'W1DWS1')
      saveRDS(K1W1DWS1, file = './data/K1W1DWS1.rds')
      if(rm.files == TRUE) rm(K1W1DWS1)
    }
    
    ## --------------- 32. K1W1DWS2 ------------------------
    if('K1W1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W1DWS2 <- vKelly(dat, type = 'W1WS2')
          saveRDS(K1W1DWS2, file = './data/K1W1DWS2.rds')
          if(rm.files == TRUE) rm(K1W1DWS2)
        }
      } else {
        stop('Kindly apply K1W1DWS2 <- vKelly(dat, type = "W1WS2") to measure the Kelly model.')
      }
    } else {
      K1W1DWS2 <- vKelly(dat, type = 'W1WS2')
      saveRDS(K1W1DWS2, file = './data/K1W1DWS2.rds')
      if(rm.files == TRUE) rm(K1W1DWS2)
    }
    
    ## --------------- 33. K1W2DWS1 ------------------------
    if('K1W2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W2DWS1 <- vKelly(dat, type = 'W2DWS1')
          saveRDS(K1W2DWS1, file = './data/K1W2DWS1.rds')
          if(rm.files == TRUE) rm(K1W2DWS1)
        }
      } else {
        stop('Kindly apply K1W2DWS1 <- vKelly(dat, type = "W2DWS1") to measure the Kelly model.')
      }
    } else {
      K1W2DWS1 <- vKelly(dat, type = 'W2DWS1')
      saveRDS(K1W2DWS1, file = './data/K1W2DWS1.rds')
      if(rm.files == TRUE) rm(K1W2DWS1)
    }
    
    ## --------------- 34. K1W2DWS2 ------------------------
    if('K1W2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1W2DWS2 <- vKelly(dat, type = 'W2DWS2')
          saveRDS(K1W2DWS2, file = './data/K1W2DWS2.rds')
          if(rm.files == TRUE) rm(K1W2DWS2)
        }
      } else {
        stop('Kindly apply K1W2DWS2 <- vKelly(dat, type = "W2DWS2") to measure the Kelly model.')
      }
    } else {
      K1W2DWS2 <- vKelly(dat, type = 'W2DWS2')
      saveRDS(K1W2DWS2, file = './data/K1W2DWS2.rds')
      if(rm.files == TRUE) rm(K1W2DWS2)
    }
    
    ## --------------- 35. K2W1DWS1 ------------------------
    if('K2W1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W1DWS1 <- vKelly2(dat, type = 'W1DWS1')
          saveRDS(K2W1DWS1, file = './data/K2W1DWS1.rds')
          if(rm.files == TRUE) rm(K2W1DWS1)
        }
      } else {
        stop('Kindly apply K2W1DWS1 <- vKelly2(dat, type = "W1DWS1") to measure the Kelly model.')
      }
    } else {
      K2W1DWS1 <- vKelly2(dat, type = 'W1DWS1')
      saveRDS(K2W1DWS1, file = './data/K2W1DWS1.rds')
      if(rm.files == TRUE) rm(K2W1DWS1)
    }
    
    ## --------------- 36. K2W1DWS2 ------------------------
    if('K2W1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W1DWS2 <- vKelly2(dat, type = 'W1DWS2')
          saveRDS(K2W1DWS2, file = './data/K2W1DWS2.rds')
          if(rm.files == TRUE) rm(K2W1DWS2)
        }
      } else {
        stop('Kindly apply K2W1DWS2 <- vKelly2(dat, type = "W1DWS2") to measure the Kelly model.')
      }
    } else {
      K2W1DWS2 <- vKelly2(dat, type = 'W1DWS2')
      saveRDS(K2W1DWS2, file = './data/K2W1DWS2.rds')
      if(rm.files == TRUE) rm(K2W1DWS2)
    }
    
    ## --------------- 37. K2W2DWS1 ------------------------
    if('K2W2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W2DWS1 <- vKelly2(dat, type = 'W2DWS1')
          saveRDS(K2W2DWS1, file = './data/K2W2DWS1.rds')
          if(rm.files == TRUE) rm(K2W2DWS1)
        }
      } else {
        stop('Kindly apply K2W2DWS1 <- vKelly2(dat, type = "W2DWS1") to measure the Kelly model.')
      }
    } else {
      K2W2DWS1 <- vKelly2(dat, type = 'W2DWS1')
      saveRDS(K2W2DWS1, file = './data/K2W2DWS1.rds')
      if(rm.files == TRUE) rm(K2W2DWS1)
    }
    
    ## --------------- 38. K2W2DWS2 ------------------------
    if('K2W2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2W2DWS2 <- vKelly2(dat, type = 'W2DWS2')
          saveRDS(K2W2DWS2, file = './data/K2W2DWS2.rds')
          if(rm.files == TRUE) rm(K2W2DWS2)
        }
      } else {
        stop('Kindly apply K2W2DWS2 <- vKelly2(dat, type = "W2DWS2") to measure the Kelly model.')
      }
    } else {
      K2W2DWS2 <- vKelly2(dat, type = 'W2DWS2')
      saveRDS(K2W2DWS2, file = './data/K2W2DWS2.rds')
      if(rm.files == TRUE) rm(K2W2DWS2)
    }
    
    ## --------------- 39. K1D1DWS1 ------------------------
    if('K1D1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1 <- vKelly(dat, type = 'D1DWS1')
          saveRDS(K1D1DWS1, file = './data/K1D1DWS1.rds')
          if(rm.files == TRUE) rm(K1D1DWS1)
        }
      } else {
        stop('Kindly apply K1D1DWS1 <- vKelly(dat, type = "D1DWS1") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1 <- vKelly(dat, type = 'D1DWS1')
      saveRDS(K1D1DWS1, file = './data/K1D1DWS1.rds')
      if(rm.files == TRUE) rm(K1D1DWS1)
    }
    
    ## --------------- 40. K1D1DWS2 ------------------------
    if('K1D1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2 <- vKelly(dat, type = 'D1DWS2')
          saveRDS(K1D1DWS2, file = './data/K1D1DWS2.rds')
          if(rm.files == TRUE) rm(K1D1DWS2)
        }
      } else {
        stop('Kindly apply K1D1DWS2 <- vKelly(dat, type = "D1DWS2") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2 <- vKelly(dat, type = 'D1DWS2')
      saveRDS(K1D1DWS2, file = './data/K1D1DWS2.rds')
      if(rm.files == TRUE) rm(K1D1DWS2)
    }
    
    ## --------------- 41. K1D2DWS1 ------------------------
    if('K1D2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1 <- vKelly(dat, type = 'D2DWS1')
          saveRDS(K1D2DWS1, file = './data/K1D2DWS1.rds')
          if(rm.files == TRUE) rm(K1D2DWS1)
        }
      } else {
        stop('Kindly apply K1D2DWS1 <- vKelly(dat, type = "D2DWS1") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1 <- vKelly(dat, type = 'D2DWS1')
      saveRDS(K1D2DWS1, file = './data/K1D2DWS1.rds')
      if(rm.files == TRUE) rm(K1D2DWS1)
    }
    
    ## --------------- 42. K1D2DWS2 ------------------------
    if('K1D2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2 <- vKelly(dat, type = 'D2DWS2')
          saveRDS(K1D2DWS2, file = './data/K1D2DWS2.rds')
          if(rm.files == TRUE) rm(K1D2DWS2)
        }
      } else {
        stop('Kindly apply K1D2DWS2 <- vKelly(dat, type = "D2DWS2") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2 <- vKelly(dat, type = 'D2DWS2')
      saveRDS(K1D2DWS2, file = './data/K1D2DWS2.rds')
      if(rm.files == TRUE) rm(K1D2DWS2)
    }
    
    ## --------------- 43. K2D1DWS1 ------------------------
    if('K2D1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1 <- vKelly2(dat, type = 'D1DWS1')
          saveRDS(K2D1DWS1, file = './data/K2D1DWS1.rds')
          if(rm.files == TRUE) rm(K2D1DWS1)
        }
      } else {
        stop('Kindly apply K2D1DWS1 <- vKelly2(dat, type = "D1DWS1") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1 <- vKelly2(dat, type = 'D1DWS1')
      saveRDS(K2D1DWS1, file = './data/K2D1DWS1.rds')
      if(rm.files == TRUE) rm(K2D1DWS1)
    }
    
    ## --------------- 44. K2D1DWS2 ------------------------
    if('K2D1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2 <- vKelly2(dat, type = 'D1DWS2')
          saveRDS(K2D1DWS2, file = './data/K2D1DWS2.rds')
          if(rm.files == TRUE) rm(K2D1DWS2)
        }
      } else {
        stop('Kindly apply K2D1DWS2 <- vKelly2(dat, type = "D1DWS2") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2 <- vKelly2(dat, type = 'D1DWS2')
      saveRDS(K2D1DWS2, file = './data/K2D1DWS2.rds')
      if(rm.files == TRUE) rm(K2D1DWS2)
    }
    
    ## --------------- 45. K2D2DWS1 ------------------------
    if('K2D2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1 <- vKelly2(dat, type = 'D2DWS1')
          saveRDS(K2D2DWS1, file = './data/K2D2DWS1.rds')
          if(rm.files == TRUE) rm(K2D2DWS1)
        }
      } else {
        stop('Kindly apply K2D2DWS1 <- vKelly2(dat, type = "D2DWS1") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1 <- vKelly2(dat, type = 'D2DWS1')
      saveRDS(K2D2DWS1, file = './data/K2D2DWS1.rds')
      if(rm.files == TRUE) rm(K2D2DWS1)
    }
    
    ## --------------- 46. K2D2DWS2 ------------------------
    if('K2D2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2 <- vKelly2(dat, type = 'D2DWS2')
          saveRDS(K2D2DWS2, file = './data/K2D2DWS2.rds')
          if(rm.files == TRUE) rm(K2D2DWS2)
        }
      } else {
        stop('Kindly apply K2D2DWS2 <- vKelly2(dat, type = "D2DWS2") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2 <- vKelly2(dat, type = 'D2DWS2')
      saveRDS(K2D2DWS2, file = './data/K2D2DWS2.rds')
      if(rm.files == TRUE) rm(K2D2DWS2)
    }
    
    ## --------------- Dynamic Mixed : Kick-Off Time + Kick-Off Time ------------------------
    ## --------------- 47. K1D1DWS1TT ------------------------
    if('K1D1DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1TT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K1D1DWS1TT, file = './data/K1D1DWS1TT.rds')
          if(rm.files == TRUE) rm(K1D1DWS1TT)
        }
      } else {
        stop('Kindly apply K1D1DWS1TT <- vKelly(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1TT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS1TT, file = './data/K1D1DWS1TT.rds')
      if(rm.files == TRUE) rm(K1D1DWS1TT)
    }
    
    ## --------------- 48. K1D1DWS2TT ------------------------
    if('K1D1DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2TT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K1D1DWS2TT, file = './data/K1D1DWS2TT.rds')
          if(rm.files == TRUE) rm(K1D1DWS2TT)
        }
      } else {
        stop('Kindly apply K1D1DWS2TT <- vKelly(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2TT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS2TT, file = './data/K1D1DWS2TT.rds')
      if(rm.files == TRUE) rm(K1D1DWS2TT)
    }
    
    ## --------------- 49. K1D2DWS1TT ------------------------
    if('K1D2DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1TT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K1D2DWS1TT, file = './data/K1D2DWS1TT.rds')
          if(rm.files == TRUE) rm(K1D2DWS1TT)
        }
      } else {
        stop('Kindly apply K1D2DWS1TT <- vKelly(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1TT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS1TT, file = './data/K1D2DWS1TT.rds')
      if(rm.files == TRUE) rm(K1D2DWS1TT)
    }
    
    ## --------------- 50. K1D2DWS2TT ------------------------
    if('K1D2DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2TT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K1D2DWS2TT, file = './data/K1D2DWS2TT.rds')
          if(rm.files == TRUE) rm(K1D2DWS2TT)
        }
      } else {
        stop('Kindly apply K1D2DWS2TT <- vKelly(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2TT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS2TT, file = './data/K1D2DWS2TT.rds')
      if(rm.files == TRUE) rm(K1D2DWS2TT)
    }
    
    ## --------------- 51. K2D1DWS1TT ------------------------
    if('K2D1DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1TT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K2D1DWS1TT, file = './data/K2D1DWS1TT.rds')
          if(rm.files == TRUE) rm(K2D1DWS1TT)
        }
      } else {
        stop('Kindly apply K2D1DWS1TT <- vKelly2(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1TT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS1TT, file = './data/K2D1DWS1TT.rds')
      if(rm.files == TRUE) rm(K2D1DWS1TT)
    }
    
    ## --------------- 52. K2D1DWS2TT ------------------------
    if('K2D1DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2TT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K2D1DWS2TT, file = './data/K2D1DWS2TT.rds')
          if(rm.files == TRUE) rm(K2D1DWS2TT)
        }
      } else {
        stop('Kindly apply K2D1DWS2TT <- vKelly2(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2TT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS2TT, file = './data/K2D1DWS2TT.rds')
      if(rm.files == TRUE) rm(K2D1DWS2TT)
    }
    
    ## --------------- 53. K2D2DWS1TT ------------------------
    if('K2D2DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1TT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K2D2DWS1TT, file = './data/K2D2DWS1TT.rds')
          if(rm.files == TRUE) rm(K2D2DWS1TT)
        }
      } else {
        stop('Kindly apply K2D2DWS1TT <- vKelly2(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1TT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS1TT, file = './data/K2D2DWS1TT.rds')
      if(rm.files == TRUE) rm(K2D2DWS1TT)
    }
    
    ## --------------- 54. K2D2DWS2TT ------------------------
    if('K2D2DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2TT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2TT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
          saveRDS(K2D2DWS2TT, file = './data/K2D2DWS2TT.rds')
          if(rm.files == TRUE) rm(K2D2DWS2TT)
        }
      } else {
        stop('Kindly apply K2D2DWS2TT <- vKelly2(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2TT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS2TT, file = './data/K2D2DWS2TT.rds')
      if(rm.files == TRUE) rm(K2D2DWS2TT)
    }
    
    ## --------------- Dynamic Mixed : Kick-Off Time + Dynamic ------------------------
    ## --------------- 55. K1D1DWS1TO ------------------------
    if('K1D1DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1TO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K1D1DWS1TO, file = './data/K1D1DWS1TO.rds')
          if(rm.files == TRUE) rm(K1D1DWS1TO)
        }
      } else {
        stop('Kindly apply K1D1DWS1TO <- vKelly(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1TO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS1TO, file = './data/K1D1DWS1TO.rds')
      if(rm.files == TRUE) rm(K1D1DWS1TO)
    }
    
    ## --------------- 56. K1D1DWS2TO ------------------------
    if('K1D1DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2TO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K1D1DWS2TO, file = './data/K1D1DWS2TO.rds')
          if(rm.files == TRUE) rm(K1D1DWS2TO)
        }
      } else {
        stop('Kindly apply K1D1DWS2TO <- vKelly(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2TO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS2TO, file = './data/K1D1DWS2TO.rds')
      if(rm.files == TRUE) rm(K1D1DWS2TO)
    }
    
    ## --------------- 57. K1D2DWS1TO ------------------------
    if('K1D2DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1TO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K1D2DWS1TO, file = './data/K1D2DWS1TO.rds')
          if(rm.files == TRUE) rm(K1D2DWS1TO)
        }
      } else {
        stop('Kindly apply K1D2DWS1TO <- vKelly(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1TO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS1TO, file = './data/K1D2DWS1TO.rds')
      if(rm.files == TRUE) rm(K1D2DWS1TO)
    }
    
    ## --------------- 58. K1D2DWS2TO ------------------------
    if('K1D2DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2TO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K1D2DWS2TO, file = './data/K1D2DWS2TO.rds')
          if(rm.files == TRUE) rm(K1D2DWS2TO)
        }
      } else {
        stop('Kindly apply K1D2DWS2TO <- vKelly(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2TO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS2TO, file = './data/K1D2DWS2TO.rds')
      if(rm.files == TRUE) rm(K1D2DWS2TO)
    }
    
    ## --------------- 59. K2D1DWS1TO ------------------------
    if('K2D1DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1TO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K2D1DWS1TO, file = './data/K2D1DWS1TO.rds')
          if(rm.files == TRUE) rm(K2D1DWS1TO)
        }
      } else {
        stop('Kindly apply K2D1DWS1TO <- vKelly2(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1TO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS1TO, file = './data/K2D1DWS1TO.rds')
      if(rm.files == TRUE) rm(K2D1DWS1TO)
    }
    
    ## --------------- 60. K2D1DWS2TO ------------------------
    if('K2D1DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2TO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K2D1DWS2TO, file = './data/K2D1DWS2TO.rds')
          if(rm.files == TRUE) rm(K2D1DWS2TO)
        }
      } else {
        stop('Kindly apply K2D1DWS2TO <- vKelly2(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2TO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS2TO, file = './data/K2D1DWS2TO.rds')
      if(rm.files == TRUE) rm(K2D1DWS2TO)
    }
    
    ## --------------- 61. K2D2DWS1TO ------------------------
    if('K2D2DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1TO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K2D2DWS1TO, file = './data/K2D2DWS1TO.rds')
          if(rm.files == TRUE) rm(K2D2DWS1TO)
        }
      } else {
        stop('Kindly apply K2D2DWS1TO <- vKelly2(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1TO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS1TO, file = './data/K2D2DWS1TO.rds')
      if(rm.files == TRUE) rm(K2D2DWS1TO)
    }
    
    ## --------------- 62. K2D2DWS2TO ------------------------
    if('K2D2DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2TO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2TO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
          saveRDS(K2D2DWS2TO, file = './data/K2D2DWS2TO.rds')
          if(rm.files == TRUE) rm(K2D2DWS2TO)
        }
      } else {
        stop('Kindly apply K2D2DWS2TO <- vKelly2(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2TO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS2TO, file = './data/K2D2DWS2TO.rds')
      if(rm.files == TRUE) rm(K2D2DWS2TO)
    }
    
    ## --------------- Dynamic Mixed : Daily + Daily ------------------------
    ## --------------- 63. K1D1DWS1DD ------------------------
    if('K1D1DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1DD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K1D1DWS1DD, file = './data/K1D1DWS1DD.rds')
          if(rm.files == TRUE) rm(K1D1DWS1DD)
        }
      } else {
        stop('Kindly apply K1D1DWS1DD <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1DD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS1DD, file = './data/K1D1DWS1DD.rds')
      if(rm.files == TRUE) rm(K1D1DWS1DD)
    }
    
    ## --------------- 64. K1D1DWS2DD ------------------------
    if('K1D1DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2DD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K1D1DWS2DD, file = './data/K1D1DWS2DD.rds')
          if(rm.files == TRUE) rm(K1D1DWS2DD)
        }
      } else {
        stop('Kindly apply K1D1DWS2DD <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2DD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS2DD, file = './data/K1D1DWS2DD.rds')
      if(rm.files == TRUE) rm(K1D1DWS2DD)
    }
    
    ## --------------- 65. K1D2DWS1DD ------------------------
    if('K1D2DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1DD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K1D2DWS1DD, file = './data/K1D2DWS1DD.rds')
          if(rm.files == TRUE) rm(K1D2DWS1DD)
        }
      } else {
        stop('Kindly apply K1D2DWS1DD <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1DD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS1DD, file = './data/K1D2DWS1DD.rds')
      if(rm.files == TRUE) rm(K1D2DWS1DD)
    }
    
    ## --------------- 66. K1D2DWS2DD ------------------------
    if('K1D2DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2DD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K1D2DWS2DD, file = './data/K1D2DWS2DD.rds')
          if(rm.files == TRUE) rm(K1D2DWS2DD)
        }
      } else {
        stop('Kindly apply K1D2DWS2DD <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2DD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS2DD, file = './data/K1D2DWS2DD.rds')
      if(rm.files == TRUE) rm(K1D2DWS2DD)
    }
    
    ## --------------- 67. K2D1DWS1DD ------------------------
    if('K2D1DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1DD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K2D1DWS1DD, file = './data/K2D1DWS1DD.rds')
          if(rm.files == TRUE) rm(K2D1DWS1DD)
        }
      } else {
        stop('Kindly apply K2D1DWS1DD <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1DD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS1DD, file = './data/K2D1DWS1DD.rds')
      if(rm.files == TRUE) rm(K2D1DWS1DD)
    }
    
    ## --------------- 68. K2D1DWS2DD ------------------------
    if('K2D1DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2DD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K2D1DWS2DD, file = './data/K2D1DWS2DD.rds')
          if(rm.files == TRUE) rm(K2D1DWS2DD)
        }
      } else {
        stop('Kindly apply K2D1DWS2DD <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2DD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS2DD, file = './data/K2D1DWS2DD.rds')
      if(rm.files == TRUE) rm(K2D1DWS2DD)
    }
    
    ## --------------- 69. K2D2DWS1DD ------------------------
    if('K2D2DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1DD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K2D2DWS1DD, file = './data/K2D2DWS1DD.rds')
          if(rm.files == TRUE) rm(K2D2DWS1DD)
        }
      } else {
        stop('Kindly apply K2D2DWS1DD <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1DD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS1DD, file = './data/K2D2DWS1DD.rds')
      if(rm.files == TRUE) rm(K2D2DWS1DD)
    }
    
    ## --------------- 70. K2D2DWS2DD ------------------------
    if('K2D2DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2DD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2DD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
          saveRDS(K2D2DWS2DD, file = './data/K2D2DWS2DD.rds')
          if(rm.files == TRUE) rm(K2D2DWS2DD)
        }
      } else {
        stop('Kindly apply K2D2DWS2DD <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2DD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS2DD, file = './data/K2D2DWS2DD.rds')
      if(rm.files == TRUE) rm(K2D2DWS2DD)
    }
    
    ## --------------- Dynamic Mixed : Daily + Kick-Off Time ------------------------
    ## --------------- 71. K1D1DWS1DT ------------------------
    if('K1D1DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1DT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K1D1DWS1DT, file = './data/K1D1DWS1DT.rds')
          if(rm.files == TRUE) rm(K1D1DWS1DT)
        }
      } else {
        stop('Kindly apply K1D1DWS1DT <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1DT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS1DT, file = './data/K1D1DWS1DT.rds')
      if(rm.files == TRUE) rm(K1D1DWS1DT)
    }
    
    ## --------------- 72. K1D1DWS2DT ------------------------
    if('K1D1DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2DT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K1D1DWS2DT, file = './data/K1D1DWS2DT.rds')
          if(rm.files == TRUE) rm(K1D1DWS2DT)
        }
      } else {
        stop('Kindly apply K1D1DWS2DT <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2DT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS2DT, file = './data/K1D1DWS2DT.rds')
      if(rm.files == TRUE) rm(K1D1DWS2DT)
    }
    
    ## --------------- 73. K1D2DWS1DT ------------------------
    if('K1D2DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1DT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K1D2DWS1DT, file = './data/K1D2DWS1DT.rds')
          if(rm.files == TRUE) rm(K1D2DWS1DT)
        }
      } else {
        stop('Kindly apply K1D2DWS1DT <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1DT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS1DT, file = './data/K1D2DWS1DT.rds')
      if(rm.files == TRUE) rm(K1D2DWS1DT)
    }
    
    ## --------------- 74. K1D2DWS2DT ------------------------
    if('K1D2DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2DT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K1D2DWS2DT, file = './data/K1D2DWS2DT.rds')
          if(rm.files == TRUE) rm(K1D2DWS2DT)
        }
      } else {
        stop('Kindly apply K1D2DWS2DT <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2DT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS2DT, file = './data/K1D2DWS2DT.rds')
      if(rm.files == TRUE) rm(K1D2DWS2DT)
    }
    
    ## --------------- 75. K2D1DWS1DT ------------------------
    if('K2D1DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1DT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K2D1DWS1DT, file = './data/K2D1DWS1DT.rds')
          if(rm.files == TRUE) rm(K2D1DWS1DT)
        }
      } else {
        stop('Kindly apply K2D1DWS1DT <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1DT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS1DT, file = './data/K2D1DWS1DT.rds')
      if(rm.files == TRUE) rm(K2D1DWS1DT)
    }
    
    ## --------------- 76. K2D1DWS2DT ------------------------
    if('K2D1DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2DT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K2D1DWS2DT, file = './data/K2D1DWS2DT.rds')
          if(rm.files == TRUE) rm(K2D1DWS2DT)
        }
      } else {
        stop('Kindly apply K2D1DWS2DT <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2DT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS2DT, file = './data/K2D1DWS2DT.rds')
      if(rm.files == TRUE) rm(K2D1DWS2DT)
    }
    
    ## --------------- 77. K2D2DWS1DT ------------------------
    if('K2D2DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1DT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K2D2DWS1DT, file = './data/K2D2DWS1DT.rds')
          if(rm.files == TRUE) rm(K2D2DWS1DT)
        }
      } else {
        stop('Kindly apply K2D2DWS1DT <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1DT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS1DT, file = './data/K2D2DWS1DT.rds')
      if(rm.files == TRUE) rm(K2D2DWS1DT)
    }
    
    ## --------------- 78. K2D2DWS2DT ------------------------
    if('K2D2DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2DT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2DT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
          saveRDS(K2D2DWS2DT, file = './data/K2D2DWS2DT.rds')
          if(rm.files == TRUE) rm(K2D2DWS2DT)
        }
      } else {
        stop('Kindly apply K2D2DWS2DT <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2DT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS2DT, file = './data/K2D2DWS2DT.rds')
      if(rm.files == TRUE) rm(K2D2DWS2DT)
    }
    
    ## --------------- Dynamic Mixed : Daily + Dynamic ------------------------
    ## --------------- 79. K1D1DWS1DO ------------------------
    if('K1D1DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1DO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K1D1DWS1DO, file = './data/K1D1DWS1DO.rds')
          if(rm.files == TRUE) rm(K1D1DWS1DO)
        }
      } else {
        stop('Kindly apply K1D1DWS1DO <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1DO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS1DO, file = './data/K1D1DWS1DO.rds')
      if(rm.files == TRUE) rm(K1D1DWS1DO)
    }
    
    ## --------------- 80. K1D1DWS2DO ------------------------
    if('K1D1DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2DO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K1D1DWS2DO, file = './data/K1D1DWS2DO.rds')
          if(rm.files == TRUE) rm(K1D1DWS2DO)
        }
      } else {
        stop('Kindly apply K1D1DWS2DO <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2DO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS2DO, file = './data/K1D1DWS2DO.rds')
      if(rm.files == TRUE) rm(K1D1DWS2DO)
    }
    
    ## --------------- 81. K1D2DWS1DO ------------------------
    if('K1D2DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1DO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K1D2DWS1DO, file = './data/K1D2DWS1DO.rds')
          if(rm.files == TRUE) rm(K1D2DWS1DO)
        }
      } else {
        stop('Kindly apply K1D2DWS1DO <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1DO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS1DO, file = './data/K1D2DWS1DO.rds')
      if(rm.files == TRUE) rm(K1D2DWS1DO)
    }
    
    ## --------------- 82. K1D2DWS2DO ------------------------
    if('K1D2DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2DO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K1D2DWS2DO, file = './data/K1D2DWS2DO.rds')
          if(rm.files == TRUE) rm(K1D2DWS2DO)
        }
      } else {
        stop('Kindly apply K1D2DWS2DO <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2DO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS2DO, file = './data/K1D2DWS2DO.rds')
      if(rm.files == TRUE) rm(K1D2DWS2DO)
    }
    
    ## --------------- 83. K2D1DWS1DO ------------------------
    if('K2D1DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1DO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K2D1DWS1DO, file = './data/K2D1DWS1DO.rds')
          if(rm.files == TRUE) rm(K2D1DWS1DO)
        }
      } else {
        stop('Kindly apply K2D1DWS1DO <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1DO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS1DO, file = './data/K2D1DWS1DO.rds')
      if(rm.files == TRUE) rm(K2D1DWS1DO)
    }
    
    ## --------------- 84. K2D1DWS2DO ------------------------
    if('K2D1DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2DO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K2D1DWS2DO, file = './data/K2D1DWS2DO.rds')
          if(rm.files == TRUE) rm(K2D1DWS2DO)
        }
      } else {
        stop('Kindly apply K2D1DWS2DO <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2DO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS2DO, file = './data/K2D1DWS2DO.rds')
      if(rm.files == TRUE) rm(K2D1DWS2DO)
    }
    
    ## --------------- 85. K2D2DWS1DO ------------------------
    if('K2D2DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1DO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K2D2DWS1DO, file = './data/K2D2DWS1DO.rds')
          if(rm.files == TRUE) rm(K2D2DWS1DO)
        }
      } else {
        stop('Kindly apply K2D2DWS1DO <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1DO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS1DO, file = './data/K2D2DWS1DO.rds')
      if(rm.files == TRUE) rm(K2D2DWS1DO)
    }
    
    ## --------------- 86. K2D2DWS2DO ------------------------
    if('K2D2DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2DO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2DO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
          saveRDS(K2D2DWS2DO, file = './data/K2D2DWS2DO.rds')
          if(rm.files == TRUE) rm(K2D2DWS2DO)
        }
      } else {
        stop('Kindly apply K2D2DWS2DO <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2DO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS2DO, file = './data/K2D2DWS2DO.rds')
      if(rm.files == TRUE) rm(K2D2DWS2DO)
    }
    
    ## --------------- Dynamic Mixed : Dynamic + Daily ------------------------
    ## --------------- 87. K1D1DWS1OD ------------------------
    if('K1D1DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1OD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K1D1DWS1OD, file = './data/K1D1DWS1OD.rds')
          if(rm.files == TRUE) rm(K1D1DWS1OD)
        }
      } else {
        stop('Kindly apply K1D1DWS1OD <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1OD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS1OD, file = './data/K1D1DWS1OD.rds')
      if(rm.files == TRUE) rm(K1D1DWS1OD)
    }
    
    ## --------------- 88. K1D1DWS2OD ------------------------
    if('K1D1DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2OD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K1D1DWS2OD, file = './data/K1D1DWS2OD.rds')
          if(rm.files == TRUE) rm(K1D1DWS2OD)
        }
      } else {
        stop('Kindly apply K1D1DWS2OD <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2OD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS2OD, file = './data/K1D1DWS2OD.rds')
      if(rm.files == TRUE) rm(K1D1DWS2OD)
    }
    
    ## --------------- 89. K1D2DWS1OD ------------------------
    if('K1D2DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1OD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K1D2DWS1OD, file = './data/K1D2DWS1OD.rds')
          if(rm.files == TRUE) rm(K1D2DWS1OD)
        }
      } else {
        stop('Kindly apply K1D2DWS1OD <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1OD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS1OD, file = './data/K1D2DWS1OD.rds')
      if(rm.files == TRUE) rm(K1D2DWS1OD)
    }
    
    ## --------------- 90. K1D2DWS2OD ------------------------
    if('K1D2DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2OD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K1D2DWS2OD, file = './data/K1D2DWS2OD.rds')
          if(rm.files == TRUE) rm(K1D2DWS2OD)
        }
      } else {
        stop('Kindly apply K1D2DWS2OD <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2OD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS2OD, file = './data/K1D2DWS2OD.rds')
      if(rm.files == TRUE) rm(K1D2DWS2OD)
    }
    
    ## --------------- 91. K2D1DWS1OD ------------------------
    if('K2D1DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1OD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K2D1DWS1OD, file = './data/K2D1DWS1OD.rds')
          if(rm.files == TRUE) rm(K2D1DWS1OD)
        }
      } else {
        stop('Kindly apply K2D1DWS1OD <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1OD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS1OD, file = './data/K2D1DWS1OD.rds')
      if(rm.files == TRUE) rm(K2D1DWS1OD)
    }
    
    ## --------------- 92. K2D1DWS2OD ------------------------
    if('K2D1DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2OD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K2D1DWS2OD, file = './data/K2D1DWS2OD.rds')
          if(rm.files == TRUE) rm(K2D1DWS2OD)
        }
      } else {
        stop('Kindly apply K2D1DWS2OD <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2OD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS2OD, file = './data/K2D1DWS2OD.rds')
      if(rm.files == TRUE) rm(K2D1DWS2OD)
    }
    
    ## --------------- 93. K2D2DWS1OD ------------------------
    if('K2D2DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1OD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K2D2DWS1OD, file = './data/K2D2DWS1OD.rds')
          if(rm.files == TRUE) rm(K2D2DWS1OD)
        }
      } else {
        stop('Kindly apply K2D2DWS1OD <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1OD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS1OD, file = './data/K2D2DWS1OD.rds')
      if(rm.files == TRUE) rm(K2D2DWS1OD)
    }
    
    ## --------------- 94. K2D2DWS2OD ------------------------
    if('K2D2DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2OD.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2OD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
          saveRDS(K2D2DWS2OD, file = './data/K2D2DWS2OD.rds')
          if(rm.files == TRUE) rm(K2D2DWS2OD)
        }
      } else {
        stop('Kindly apply K2D2DWS2OD <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2OD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS2OD, file = './data/K2D2DWS2OD.rds')
      if(rm.files == TRUE) rm(K2D2DWS2OD)
    }
    
    ## --------------- Dynamic Mixed : Dynamic + Kick-Off Time ------------------------
    ## --------------- 95. K1D1DWS1OT ------------------------
    if('K1D1DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1OT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K1D1DWS1OT, file = './data/K1D1DWS1OT.rds')
          if(rm.files == TRUE) rm(K1D1DWS1OT)
        }
      } else {
        stop('Kindly apply K1D1DWS1OT <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1OT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS1OT, file = './data/K1D1DWS1OT.rds')
      if(rm.files == TRUE) rm(K1D1DWS1OT)
    }
    
    ## --------------- 96. K1D1DWS2OT ------------------------
    if('K1D1DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2OT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K1D1DWS2OT, file = './data/K1D1DWS2OT.rds')
          if(rm.files == TRUE) rm(K1D1DWS2OT)
        }
      } else {
        stop('Kindly apply K1D1DWS2OT <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2OT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS2OT, file = './data/K1D1DWS2OT.rds')
      if(rm.files == TRUE) rm(K1D1DWS2OT)
    }
    
    ## --------------- 97. K1D2DWS1OT ------------------------
    if('K1D2DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1OT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K1D2DWS1OT, file = './data/K1D2DWS1OT.rds')
          if(rm.files == TRUE) rm(K1D2DWS1OT)
        }
      } else {
        stop('Kindly apply K1D2DWS1OT <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1OT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS1OT, file = './data/K1D2DWS1OT.rds')
      if(rm.files == TRUE) rm(K1D2DWS1OT)
    }
    
    ## --------------- 98. K1D2DWS2OT ------------------------
    if('K1D2DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2OT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K1D2DWS2OT, file = './data/K1D2DWS2OT.rds')
          if(rm.files == TRUE) rm(K1D2DWS2OT)
        }
      } else {
        stop('Kindly apply K1D2DWS2OT <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2OT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS2OT, file = './data/K1D2DWS2OT.rds')
      if(rm.files == TRUE) rm(K1D2DWS2OT)
    }
    
    ## --------------- 99. K2D1DWS1OT ------------------------
    if('K2D1DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1OT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K2D1DWS1OT, file = './data/K2D1DWS1OT.rds')
          if(rm.files == TRUE) rm(K2D1DWS1OT)
        }
      } else {
        stop('Kindly apply K2D1DWS1OT <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1OT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS1OT, file = './data/K2D1DWS1OT.rds')
      if(rm.files == TRUE) rm(K2D1DWS1OT)
    }
    
    ## --------------- 100. K2D1DWS2OT ------------------------
    if('K2D1DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2OT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K2D1DWS2OT, file = './data/K2D1DWS2OT.rds')
          if(rm.files == TRUE) rm(K2D1DWS2OT)
        }
      } else {
        stop('Kindly apply K2D1DWS2OT <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2OT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS2OT, file = './data/K2D1DWS2OT.rds')
      if(rm.files == TRUE) rm(K2D1DWS2OT)
    }
    
    ## --------------- 101. K2D2DWS1OT ------------------------
    if('K2D2DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1OT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K2D2DWS1OT, file = './data/K2D2DWS1OT.rds')
          if(rm.files == TRUE) rm(K2D2DWS1OT)
        }
      } else {
        stop('Kindly apply K2D2DWS1OT <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1OT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS1OT, file = './data/K2D2DWS1OT.rds')
      if(rm.files == TRUE) rm(K2D2DWS1OT)
    }
    
    ## --------------- 102. K2D2DWS2OT ------------------------
    if('K2D2DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2OT.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2OT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
          saveRDS(K2D2DWS2OT, file = './data/K2D2DWS2OT.rds')
          if(rm.files == TRUE) rm(K2D2DWS2OT)
        }
      } else {
        stop('Kindly apply K2D2DWS2OT <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2OT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS2OT, file = './data/K2D2DWS2OT.rds')
      if(rm.files == TRUE) rm(K2D2DWS2OT)
    }
    
    ## --------------- Dynamic Mixed : Dynamic + Dynamic ------------------------
    ## --------------- 103. K1D1DWS1OO ------------------------
    if('K1D1DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS1OO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K1D1DWS1OO, file = './data/K1D1DWS1OO.rds')
          if(rm.files == TRUE) rm(K1D1DWS1OO)
        }
      } else {
        stop('Kindly apply K1D1DWS1OO <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1OO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS1OO, file = './data/K1D1DWS1OO.rds')
      if(rm.files == TRUE) rm(K1D1DWS1OO)
    }
    
    ## --------------- 104. K1D1DWS2OO ------------------------
    if('K1D1DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D1DWS2OO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K1D1DWS2OO, file = './data/K1D1DWS2OO.rds')
          if(rm.files == TRUE) rm(K1D1DWS2OO)
        }
      } else {
        stop('Kindly apply K1D1DWS2OO <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2OO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS2OO, file = './data/K1D1DWS2OO.rds')
      if(rm.files == TRUE) rm(K1D1DWS2OO)
    }
    
    ## --------------- 105. K1D2DWS1OO ------------------------
    if('K1D2DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS1OO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K1D2DWS1OO, file = './data/K1D2DWS1OO.rds')
          if(rm.files == TRUE) rm(K1D2DWS1OO)
        }
      } else {
        stop('Kindly apply K1D2DWS1OO <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1OO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS1OO, file = './data/K1D2DWS1OO.rds')
      if(rm.files == TRUE) rm(K1D2DWS1OO)
    }
    
    ## --------------- 106. K1D2DWS2OO ------------------------
    if('K1D2DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K1D2DWS2OO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K1D2DWS2OO, file = './data/K1D2DWS2OO.rds')
          if(rm.files == TRUE) rm(K1D2DWS2OO)
        }
      } else {
        stop('Kindly apply K1D2DWS2OO <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2OO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS2OO, file = './data/K1D2DWS2OO.rds')
      if(rm.files == TRUE) rm(K1D2DWS2OO)
    }
    
    ## --------------- 107. K2D1DWS1OO ------------------------
    if('K2D1DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS1OO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K2D1DWS1OO, file = './data/K2D1DWS1OO.rds')
          if(rm.files == TRUE) rm(K2D1DWS1OO)
        }
      } else {
        stop('Kindly apply K2D1DWS1OO <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1OO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS1OO, file = './data/K2D1DWS1OO.rds')
      if(rm.files == TRUE) rm(K2D1DWS1OO)
    }
    
    ## --------------- 108. K2D1DWS2OO ------------------------
    if('K2D1DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D1DWS2OO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K2D1DWS2OO, file = './data/K2D1DWS2OO.rds')
          if(rm.files == TRUE) rm(K2D1DWS2OO)
        }
      } else {
        stop('Kindly apply K2D1DWS2OO <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2OO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS2OO, file = './data/K2D1DWS2OO.rds')
      if(rm.files == TRUE) rm(K2D1DWS2OO)
    }
    
    ## --------------- 109. K2D2DWS1OO ------------------------
    if('K2D2DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS1OO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K2D2DWS1OO, file = './data/K2D2DWS1OO.rds')
          if(rm.files == TRUE) rm(K2D2DWS1OO)
        }
      } else {
        stop('Kindly apply K2D2DWS1OO <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1OO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS1OO, file = './data/K2D2DWS1OO.rds')
      if(rm.files == TRUE) rm(K2D2DWS1OO)
    }
    
    ## --------------- 110. K2D2DWS2OO ------------------------
    if('K2D2DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2OO.rds')))) {
        if(overwrite == FALSE) {
          stop('The file has exist in the directory.')
        } else {
          K2D2DWS2OO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
          saveRDS(K2D2DWS2OO, file = './data/K2D2DWS2OO.rds')
          if(rm.files == TRUE) rm(K2D2DWS2OO)
        }
      } else {
        stop('Kindly apply K2D2DWS2OO <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2OO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS2OO, file = './data/K2D2DWS2OO.rds')
      if(rm.files == TRUE) rm(K2D2DWS2OO)
    }
    
  } else if(action == 'load') {
    ## ================== Load Models ================================
    
    ## --------------- 01. K1 ------------------------
    ## Reserved Stakes Kelly Models.
    if('K1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1.rds')))) {
        K1 <- readRDS('./data/K1.rds')
      } else {
        stop('Kindly apply K1 <- vKelly(dat) to measure the Kelly model.')
      }
    } else {
      K1 <- vKelly(dat)
      saveRDS(K1, file = './data/K1.rds')
    }
    
    ## --------------- 02. K2 ------------------------
    ## Reserved EM Probabilities Kelly Models.
    if('K2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2.rds')))) {
        K2 <- readRDS('./data/K2.rds')
      } else {
        stop('Kindly apply K2 <- vKelly2(dat) to measure the Kelly model.')
      }
    } else {
      K2 <- vKelly2(dat)
      saveRDS(K2, file = './data/K2.rds')
    }
    
    ## --------------- 03. K1W1 ------------------------
    ## Reserved Stakes Kelly Models with Weight past year annual theta constant value.
    ##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K1W1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1.rds')))) {
        K1W1 <- readRDS('./data/K1W1.rds')
      } else {
        stop('Kindly apply K1W1 <- vKelly(dat, type = "W1") to measure the Kelly model.')
      }
    } else {
      K1W1 <- vKelly(dat, type = 'W1')
      saveRDS(K1W1, file = './data/K1W1.rds')
    }
    
    ## --------------- 04. K1W2 ------------------------
    ## Reserved Stakes Kelly Models with Weight past year annual weighted hdp parameter.
    ##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K1W2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2.rds')))) {
        K1W2 <- readRDS('./data/K1W2.rds')
      } else {
        stop('Kindly apply K1W2 <- vKelly(dat, type = "W2") to measure the Kelly model.')
      }
    } else {  
      K1W2 <- vKelly(dat, type = 'W2')
      saveRDS(K1W2, file = './data/K1W2.rds')
    }
    
    ## --------------- 05. K2W1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value.
    ##   Mix weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K2W1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1.rds')))) {
        K2W1 <- readRDS('./data/K2W1.rds')
      } else {
        stop('Kindly apply K2W1 <- vKelly2(dat, type = "W1") to measure the Kelly model.')
      }
    } else {
      K2W1 <- vKelly2(dat, type = 'W1')
      saveRDS(K2W1, file = './data/K2W1.rds')
    }
    
    ## --------------- 06. K2W2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual weighted hdp parameter.
    ##   Separate weighted on -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1
    if('K2W2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2.rds')))) {
        K2W2 <- readRDS('./data/K2W2.rds')
      } else {
        stop('Kindly apply K2W2 <- vKelly2(dat, type = "W2") to measure the Kelly model.')
      }
    } else {
      K2W2 <- vKelly2(dat, type = 'W2')
      saveRDS(K2W2, file = './data/K2W2.rds')
    }
    
    ## --------------- 07. K1W1WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K1W1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1WS1.rds')))) {
        K1W1WS1 <- readRDS('./data/K1W1WS1.rds')
      } else {
        stop('Kindly apply K1W1WS1 <- vKelly(dat, type = "W1WS1") to measure the Kelly model.')
      }
    } else {
      K1W1WS1 <- vKelly(dat, type = 'W1WS1')
      saveRDS(K1W1WS1, file = './data/K1W1WS1.rds')
    }
    
    ## --------------- 08. K1W1WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K1W1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1WS2.rds')))) {
        K1W1WS2 <- readRDS('./data/K1W1WS2.rds')
      } else {
        stop('Kindly apply K1W1WS2 <- vKelly(dat, type = "W1WS2") to measure the Kelly model.')
      }
    } else {
      K1W1WS2 <- vKelly(dat, type = 'W1WS2')
      saveRDS(K1W1WS2, file = './data/K1W1WS2.rds')
    }
    
    ## --------------- 09. K1W2WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K1W2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2WS1.rds')))) {
        K1W2WS1 <- readRDS('./data/K1W2WS1.rds')
      } else {
        stop('Kindly apply K1W2WS1 <- vKelly(dat, type = "W2WS1") to measure the Kelly model.')
      }
    } else {
      K1W2WS1 <- vKelly(dat, type = 'W2WS1')
      saveRDS(K1W2WS1, file = './data/K1W2WS1.rds')
    }
    
    ## --------------- 10. K1W2WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K1W2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2WS2.rds')))) {
        K1W2WS2 <- readRDS('./data/K1W2WS2.rds')
      } else {
        stop('Kindly apply K1W2WS2 <- vKelly2(dat, type = "W2WS2") to measure the Kelly model.')
      }
    } else {
      K1W2WS2 <- vKelly(dat, type = 'W2WS2')
      saveRDS(K1W2WS2, file = './data/K1W2WS2.rds')
    }
    
    ## --------------- 11. K2W1WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K2W1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1WS1.rds')))) {
        K2W1WS1 <- readRDS('./data/K2W1WS1.rds')
      } else {
        stop('Kindly apply K2W1WS1 <- vKelly2(dat, type = "W1WS1") to measure the Kelly model.')
      }
    } else {
      K2W1WS1 <- vKelly2(dat, type = 'W1WS1')
      saveRDS(K2W1WS1, file = './data/K2W1WS1.rds')
    }
    
    ## --------------- 12. K2W1WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K2W1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1WS2.rds')))) {
        K2W1WS2 <- readRDS('./data/K2W1WS2.rds')
      } else {
        stop('Kindly apply K2W1WS2 <- vKelly2(dat, type = "W1WS2") to measure the Kelly model.')
      }
    } else {
      K2W1WS2 <- vKelly2(dat, type = 'W1WS2')
      saveRDS(K2W1WS2, file = './data/K2W1WS2.rds')
    }
    
    ## --------------- 13. K2W2WS1 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
    if('K2W2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2WS1.rds')))) {
        K2W2WS1 <- readRDS('./data/K2W2WS1.rds')
      } else {
        stop('Kindly apply K2W2WS1 <- vKelly2(dat, type = "W2WS1") to measure the Kelly model.')
      }
    } else {
      K2W2WS1 <- vKelly2(dat, type = 'W2WS1')
      saveRDS(K2W2WS1, file = './data/K2W2WS1.rds')
    }
    
    ## --------------- 14. K2W2WS2 ------------------------
    ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
    if('K2W2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2WS2.rds')))) {
        K2W2WS2 <- readRDS('./data/K2W2WS2.rds')
      } else {
        stop('Kindly apply K2W2WS2 <- vKelly2(dat, type = "W2WS2") to measure the Kelly model.')
      }
    } else {
      K2W2WS2 <- vKelly2(dat, type = 'W2WS2')
      saveRDS(K2W2WS2, file = './data/K2W2WS2.rds')
    }
    
    ## --------------- Default Dynamic ------------------------
    ## --------------- 15. K1D1 ------------------------
    if('K1D1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1.rds')))) {
        K1D1 <- readRDS('./data/K1D1.rds')
      } else {
        stop('Kindly apply K1D1 <- vKelly(dat, type = "D1") to measure the Kelly model.')
      }
    } else {
      K1D1 <- vKelly(dat, type = 'D1')
      saveRDS(K1D1, file = './data/K1D1.rds')
    }
    
    ## --------------- 16. K1D2 ------------------------
    if('K1D2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2.rds')))) {
        K1D2 <- readRDS('./data/K1D2.rds')
      } else {
        stop('Kindly apply K1D2 <- vKelly(dat, type = "D2") to measure the Kelly model.')
      }
    } else {
      K1D2 <- vKelly(dat, type = 'D2')
      saveRDS(K1D2, file = './data/K1D2.rds')
    }
    
    ## --------------- 17. K2D1 ------------------------
    if('K2D1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1.rds')))) {
        K2D1 <- readRDS('./data/K2D1.rds')
      } else {
        stop('Kindly apply K2D1 <- vKelly2(dat, type = "D1") to measure the Kelly model.')
      }
    } else {
      K2D1 <- vKelly2(dat, type = 'D1')
      saveRDS(K2D1, file = './data/K2D1.rds')
    }
    
    ## --------------- 18. K2D2 ------------------------
    if('K2D2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2.rds')))) {
        K2D2 <- readRDS('./data/K2D2.rds')
      } else {
        stop('Kindly apply K2D2 <- vKelly2(dat, type = "D2") to measure the Kelly model.')
      }
    } else {
      K2D2 <- vKelly2(dat, type = 'D2')
      saveRDS(K2D2, file = './data/K2D2.rds')
    }
    
    ## --------------- 19. K1DWS1 ------------------------
    if('K1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1DWS1.rds')))) {
        K1DWS1 <- readRDS('./data/K1DWS1.rds')
      } else {
        stop('Kindly apply K1DWS1 <- vKelly(dat, type = "DWS1") to measure the Kelly model.')
      }
    } else {
      K1DWS1 <- vKelly(dat, type = 'DWS1')
      saveRDS(K1DWS1, file = './data/K1DWS1.rds')
    }
    
    ## --------------- 20. K1DWS2 ------------------------
    if('K1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1DWS2.rds')))) {
        K1DWS2 <- readRDS('./data/K1DWS2.rds')
      } else {
        stop('Kindly apply K1DWS2 <- vKelly(dat, type = "DWS2") to measure the Kelly model.')
      }
    } else {
      K1DWS2 <- vKelly(dat, type = 'DWS2')
      saveRDS(K1DWS2, file = './data/K1DWS2.rds')
    }
    
    ## --------------- 21. K2DWS1 ------------------------
    if('K2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2DWS1.rds')))) {
        K2DWS1 <- readRDS('./data/K2DWS1.rds')
      } else {
        stop('Kindly apply K2DWS1 <- vKelly2(dat, type = "DWS1") to measure the Kelly model.')
      }
    } else {
      K2DWS1 <- vKelly2(dat, type = 'DWS1')
      saveRDS(K2DWS1, file = './data/K2DWS1.rds')
    }
    
    ## --------------- 22. K2DWS2 ------------------------
    if('K2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2DWS2.rds')))) {
        K2DWS2 <- readRDS('./data/K2DWS2.rds')
      } else {
        stop('Kindly apply K2DWS2 <- vKelly2(dat, type = "DWS2") to measure the Kelly model.')
      }
    } else {
      K2DWS2 <- vKelly2(dat, type = 'DWS2')
      saveRDS(K2DWS2, file = './data/K2DWS2.rds')
    }
    
    ## --------------- 23. K1D1WS1 ------------------------
    if('K1D1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1WS1.rds')))) {
        K1D1WS1 <- readRDS('./data/K1D1WS1.rds')
      } else {
        stop('Kindly apply K1D1WS1 <- vKelly(dat, type = "D1WS1") to measure the Kelly model.')
      }
    } else {
      K1D1WS1 <- vKelly(dat, type = 'D1WS1')
      saveRDS(K1D1WS1, file = './data/K1D1WS1.rds')
    }
    
    ## --------------- 24. K1D1WS2 ------------------------
    if('K1D1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1WS2.rds')))) {
        K1D1WS2 <- readRDS('./data/K1D1WS2.rds')
      } else {
        stop('Kindly apply K1D1WS2 <- vKelly(dat, type = "D1WS2") to measure the Kelly model.')
      }
    } else {
      K1D1WS2 <- vKelly(dat, type = 'D1WS2')
      saveRDS(K1D1WS2, file = './data/K1D1WS2.rds'); rm(K1D1WS2)
    }
    
    ## --------------- 25. K1D2WS1 ------------------------
    if('K1D2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2WS1.rds')))) {
        K1D2WS1 <- readRDS('./data/K1D2WS1.rds')
      } else {
        stop('Kindly apply K1D2WS1 <- vKelly(dat, type = "D2WS1") to measure the Kelly model.')
      }
    } else {
      K1D2WS1 <- vKelly(dat, type = 'D2WS1')
      saveRDS(K1D2WS1, file = './data/K1D2WS1.rds')
    }
    
    ## --------------- 26. K1D2WS2 ------------------------
    if('K1D2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2WS2.rds')))) {
        K1D2WS2 <- readRDS('./data/K1D2WS2.rds')
      } else {
        stop('Kindly apply K1D2WS2 <- vKelly(dat, type = "D2WS2") to measure the Kelly model.')
      }
    } else {
      K1D2WS2 <- vKelly(dat, type = 'D2WS2')
      saveRDS(K1D2WS2, file = './data/K1D2WS2.rds')
    }
    
    ## --------------- 27. K2D1WS1 ------------------------
    if('K2D1WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1WS1.rds')))) {
        K2D1WS1 <- readRDS('./data/K2D1WS1.rds')
      } else {
        stop('Kindly apply K2D1WS1 <- vKelly2(dat, type = "D1WS1") to measure the Kelly model.')
      }
    } else {
      K2D1WS1 <- vKelly2(dat, type = 'D1WS1')
      saveRDS(K2D1WS1, file = './data/K2D1WS1.rds')
    }
    
    ## --------------- 28. K2D1WS2 ------------------------
    if('K2D1WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1WS2.rds')))) {
        K2D1WS2 <- readRDS('./data/K2D1WS2.rds')
      } else {
        stop('Kindly apply K2D1WS2 <- vKelly2(dat, type = "D1WS2") to measure the Kelly model.')
      }
    } else {
      K2D1WS2 <- vKelly2(dat, type = 'D1WS2')
      saveRDS(K2D1WS2, file = './data/K2D1WS2.rds')
    }
    
    ## --------------- 29. K2D2WS1 ------------------------
    if('K2D2WS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2WS1.rds')))) {
        K2D2WS1 <- readRDS('./data/K2D2WS1.rds')
      } else {
        stop('Kindly apply K2D2WS1 <- vKelly2(dat, type = "D2WS1") to measure the Kelly model.')
      }
    } else {
      K2D2WS1 <- vKelly2(dat, type = 'D2WS1')
      saveRDS(K2D2WS1, file = './data/K2D2WS1.rds')
    }
    
    ## --------------- 30. K2D2WS2 ------------------------
    if('K2D2WS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2WS2.rds')))) {
        K2D2WS2 <- readRDS('./data/K2D2WS2.rds')
      } else {
        stop('Kindly apply K2D2WS2 <- vKelly2(dat, type = "D2WS2") to measure the Kelly model.')
      }
    } else {
      K2D2WS2 <- vKelly2(dat, type = 'D2WS2')
      saveRDS(K2D2WS2, file = './data/K2D2WS2.rds')
    }
    
    ## --------------- 31. K1W1DWS1 ------------------------
    if('K1W1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1DWS1.rds')))) {
        K1W1DWS1 <- readRDS('./data/K1W1DWS1.rds')
      } else {
        stop('Kindly apply K1W1DWS1 <- vKelly(dat, type = "W1DWS1") to measure the Kelly model.')
      }
    } else {
      K1W1DWS1 <- vKelly(dat, type = 'W1DWS1')
      saveRDS(K1W1DWS1, file = './data/K1W1DWS1.rds')
    }
    
    ## --------------- 32. K1W1DWS2 ------------------------
    if('K1W1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W1DWS2.rds')))) {
        K1W1DWS2 <- readRDS('./data/K1W1DWS2.rds')
      } else {
        stop('Kindly apply K1W1DWS2 <- vKelly(dat, type = "W1WS2") to measure the Kelly model.')
      }
    } else {
      K1W1DWS2 <- vKelly(dat, type = 'W1WS2')
      saveRDS(K1W1DWS2, file = './data/K1W1DWS2.rds')
    }
    
    ## --------------- 33. K1W2DWS1 ------------------------
    if('K1W2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2DWS1.rds')))) {
        K1W2DWS1 <- readRDS('./data/K1W2DWS1.rds')
      } else {
        stop('Kindly apply K1W2DWS1 <- vKelly(dat, type = "W2DWS1") to measure the Kelly model.')
      }
    } else {
      K1W2DWS1 <- vKelly(dat, type = 'W2DWS1')
      saveRDS(K1W2DWS1, file = './data/K1W2DWS1.rds')
    }
    
    ## --------------- 34. K1W2DWS2 ------------------------
    if('K1W2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1W2DWS2.rds')))) {
        K1W2DWS2 <- readRDS('./data/K1W2DWS2.rds')
      } else {
        stop('Kindly apply K1W2DWS2 <- vKelly(dat, type = "W2DWS2") to measure the Kelly model.')
      }
    } else {
      K1W2DWS2 <- vKelly(dat, type = 'W2DWS2')
      saveRDS(K1W2DWS2, file = './data/K1W2DWS2.rds')
    }
    
    ## --------------- 35. K2W1DWS1 ------------------------
    if('K2W1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1DWS1.rds')))) {
        K2W1DWS1 <- readRDS('./data/K2W1DWS1.rds')
      } else {
        stop('Kindly apply K2W1DWS1 <- vKelly2(dat, type = "W1DWS1") to measure the Kelly model.')
      }
    } else {
      K2W1DWS1 <- vKelly2(dat, type = 'W1DWS1')
      saveRDS(K2W1DWS1, file = './data/K2W1DWS1.rds')
    }
    
    ## --------------- 36. K2W1DWS2 ------------------------
    if('K2W1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W1DWS2.rds')))) {
        K2W1DWS2 <- readRDS('./data/K2W1DWS2.rds')
      } else {
        stop('Kindly apply K2W1DWS2 <- vKelly2(dat, type = "W1DWS2") to measure the Kelly model.')
      }
    } else {
      K2W1DWS2 <- vKelly2(dat, type = 'W1DWS2')
      saveRDS(K2W1DWS2, file = './data/K2W1DWS2.rds')
    }
    
    ## --------------- 37. K2W2DWS1 ------------------------
    if('K2W2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2DWS1.rds')))) {
        K2W2DWS1 <- readRDS('./data/K2W2DWS1.rds')
      } else {
        stop('Kindly apply K2W2DWS1 <- vKelly2(dat, type = "W2DWS1") to measure the Kelly model.')
      }
    } else {
      K2W2DWS1 <- vKelly2(dat, type = 'W2DWS1')
      saveRDS(K2W2DWS1, file = './data/K2W2DWS1.rds')
    }
    
    ## --------------- 38. K2W2DWS2 ------------------------
    if('K2W2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2W2DWS2.rds')))) {
        K2W2DWS2 <- readRDS('./data/K2W2DWS2.rds')
      } else {
        stop('Kindly apply K2W2DWS2 <- vKelly2(dat, type = "W2DWS2") to measure the Kelly model.')
      }
    } else {
      K2W2DWS2 <- vKelly2(dat, type = 'W2DWS2')
      saveRDS(K2W2DWS2, file = './data/K2W2DWS2.rds')
    }
    
    ## --------------- 39. K1D1DWS1 ------------------------
    if('K1D1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1.rds')))) {
        K1D1DWS1 <- readRDS('./data/K1D1DWS1.rds')
      } else {
        stop('Kindly apply K1D1DWS1 <- vKelly(dat, type = "D1DWS1") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1 <- vKelly(dat, type = 'D1DWS1')
      saveRDS(K1D1DWS1, file = './data/K1D1DWS1.rds')
    }
    
    ## --------------- 40. K1D1DWS2 ------------------------
    if('K1D1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2.rds')))) {
        K1D1DWS2 <- readRDS('./data/K1D1DWS2.rds')
      } else {
        stop('Kindly apply K1D1DWS2 <- vKelly(dat, type = "D1DWS2") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2 <- vKelly(dat, type = 'D1DWS2')
      saveRDS(K1D1DWS2, file = './data/K1D1DWS2.rds')
    }
    
    ## --------------- 41. K1D2DWS1 ------------------------
    if('K1D2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1.rds')))) {
        K1D2DWS1 <- readRDS('./data/K1D2DWS1.rds')
      } else {
        stop('Kindly apply K1D2DWS1 <- vKelly(dat, type = "D2DWS1") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1 <- vKelly(dat, type = 'D2DWS1')
      saveRDS(K1D2DWS1, file = './data/K1D2DWS1.rds')
    }
    
    ## --------------- 42. K1D2DWS2 ------------------------
    if('K1D2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2.rds')))) {
        K1D2DWS2 <- readRDS('./data/K1D2DWS2.rds')
      } else {
        stop('Kindly apply K1D2DWS2 <- vKelly(dat, type = "D2DWS2") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2 <- vKelly(dat, type = 'D2DWS2')
      saveRDS(K1D2DWS2, file = './data/K1D2DWS2.rds')
    }
    
    ## --------------- 43. K2D1DWS1 ------------------------
    if('K2D1DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1.rds')))) {
        K2D1DWS1 <- readRDS('./data/K2D1DWS1.rds')
      } else {
        stop('Kindly apply K2D1DWS1 <- vKelly2(dat, type = "D1DWS1") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1 <- vKelly2(dat, type = 'D1DWS1')
      saveRDS(K2D1DWS1, file = './data/K2D1DWS1.rds')
    }
    
    ## --------------- 44. K2D1DWS2 ------------------------
    if('K2D1DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2.rds')))) {
        K2D1DWS2 <- readRDS('./data/K2D1DWS2.rds')
      } else {
        stop('Kindly apply K2D1DWS2 <- vKelly2(dat, type = "D1DWS2") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2 <- vKelly2(dat, type = 'D1DWS2')
      saveRDS(K2D1DWS2, file = './data/K2D1DWS2.rds')
    }
    
    ## --------------- 45. K2D2DWS1 ------------------------
    if('K2D2DWS1.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1.rds')))) {
        K2D2DWS1 <- readRDS('./data/K2D2DWS1.rds')
      } else {
        stop('Kindly apply K2D2DWS1 <- vKelly2(dat, type = "D2DWS1") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1 <- vKelly2(dat, type = 'D2DWS1')
      saveRDS(K2D2DWS1, file = './data/K2D2DWS1.rds')
    }
    
    ## --------------- 46. K2D2DWS2 ------------------------
    if('K2D2DWS2.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2.rds')))) {
        K2D2DWS2 <- readRDS('./data/K2D2DWS2.rds')
      } else {
        stop('Kindly apply K2D2DWS2 <- vKelly2(dat, type = "D2DWS2") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2 <- vKelly2(dat, type = 'D2DWS2')
      saveRDS(K2D2DWS2, file = './data/K2D2DWS2.rds')
    }
    
    ## --------------- Dynamic Mixed : Kick-Off Time + Kick-Off Time ------------------------
    ## --------------- 47. K1D1DWS1TT ------------------------
    if('K1D1DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1TT.rds')))) {
        K1D1DWS1TT <- readRDS('./data/K1D1DWS1TT.rds')
      } else {
        stop('Kindly apply K1D1DWS1TT <- vKelly(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1TT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS1TT, file = './data/K1D1DWS1TT.rds')
    }
    
    ## --------------- 48. K1D1DWS2TT ------------------------
    if('K1D1DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2TT.rds')))) {
        K1D1DWS2TT <- readRDS('./data/K1D1DWS2TT.rds')
      } else {
        stop('Kindly apply K1D1DWS2TT <- vKelly(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2TT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS2TT, file = './data/K1D1DWS2TT.rds')
    }
    
    ## --------------- 49. K1D2DWS1TT ------------------------
    if('K1D2DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1TT.rds')))) {
        K1D2DWS1TT <- readRDS('./data/K1D2DWS1TT.rds')
      } else {
        stop('Kindly apply K1D2DWS1TT <- vKelly(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1TT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS1TT, file = './data/K1D2DWS1TT.rds')
    }
    
    ## --------------- 50. K1D2DWS2TT ------------------------
    if('K1D2DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2TT.rds')))) {
        K1D2DWS2TT <- readRDS('./data/K1D2DWS2TT.rds')
      } else {
        stop('Kindly apply K1D2DWS2TT <- vKelly(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2TT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS2TT, file = './data/K1D2DWS2TT.rds')
    }
    
    ## --------------- 51. K2D1DWS1TT ------------------------
    if('K2D1DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1TT.rds')))) {
        K2D1DWS1TT <- readRDS('./data/K2D1DWS1TT.rds')
      } else {
        stop('Kindly apply K2D1DWS1TT <- vKelly2(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1TT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS1TT, file = './data/K2D1DWS1TT.rds')
    }
    
    ## --------------- 52. K2D1DWS2TT ------------------------
    if('K2D1DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2TT.rds')))) {
        K2D1DWS2TT <- readRDS('./data/K2D1DWS2TT.rds')
      } else {
        stop('Kindly apply K2D1DWS2TT <- vKelly2(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2TT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS2TT, file = './data/K2D1DWS2TT.rds')
    }
    
    ## --------------- 53. K2D2DWS1TT ------------------------
    if('K2D2DWS1TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1TT.rds')))) {
        K2D2DWS1TT <- readRDS('./data/K2D2DWS1TT.rds')
      } else {
        stop('Kindly apply K2D2DWS1TT <- vKelly2(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1TT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS1TT, file = './data/K2D2DWS1TT.rds')
    }
    
    ## --------------- 54. K2D2DWS2TT ------------------------
    if('K2D2DWS2TT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2TT.rds')))) {
        K2D2DWS2TT <- readRDS('./data/K2D2DWS2TT.rds')
      } else {
        stop('Kindly apply K2D2DWS2TT <- vKelly2(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2TT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS2TT, file = './data/K2D2DWS2TT.rds')
    }
    
    ## --------------- Dynamic Mixed : Kick-Off Time + Dynamic ------------------------
    ## --------------- 55. K1D1DWS1TO ------------------------
    if('K1D1DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1TO.rds')))) {
        K1D1DWS1TO <- readRDS('./data/K1D1DWS1TO.rds')
      } else {
        stop('Kindly apply K1D1DWS1TO <- vKelly(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1TO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS1TO, file = './data/K1D1DWS1TO.rds')
    }
    
    ## --------------- 56. K1D1DWS2TO ------------------------
    if('K1D1DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2TO.rds')))) {
        K1D1DWS2TO <- readRDS('./data/K1D1DWS2TO.rds')
      } else {
        stop('Kindly apply K1D1DWS2TO <- vKelly(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2TO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS2TO, file = './data/K1D1DWS2TO.rds')
    }
    
    ## --------------- 57. K1D2DWS1TO ------------------------
    if('K1D2DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1TO.rds')))) {
        K1D2DWS1TO <- readRDS('./data/K1D2DWS1TO.rds')
      } else {
        stop('Kindly apply K1D2DWS1TO <- vKelly(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1TO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS1TO, file = './data/K1D2DWS1TO.rds')
    }
    
    ## --------------- 58. K1D2DWS2TO ------------------------
    if('K1D2DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2TO.rds')))) {
        K1D2DWS2TO <- readRDS('./data/K1D2DWS2TO.rds')
      } else {
        stop('Kindly apply K1D2DWS2TO <- vKelly(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2TO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS2TO, file = './data/K1D2DWS2TO.rds')
    }
    
    ## --------------- 59. K2D1DWS1TO ------------------------
    if('K2D1DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1TO.rds')))) {
        K2D1DWS1TO <- readRDS('./data/K2D1DWS1TO.rds')
      } else {
        stop('Kindly apply K2D1DWS1TO <- vKelly2(dat, type = "D1DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1TO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS1TO, file = './data/K2D1DWS1TO.rds')
    }
    
    ## --------------- 60. K2D1DWS2TO ------------------------
    if('K2D1DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2TO.rds')))) {
        K2D1DWS2TO <- readRDS('./data/K2D1DWS2TO.rds')
      } else {
        stop('Kindly apply K2D1DWS2TO <- vKelly2(dat, type = "D1DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2TO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS2TO, file = './data/K2D1DWS2TO.rds')
    }
    
    ## --------------- 61. K2D2DWS1TO ------------------------
    if('K2D2DWS1TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1TO.rds')))) {
        K2D2DWS1TO <- readRDS('./data/K2D2DWS1TO.rds')
      } else {
        stop('Kindly apply K2D2DWS1TO <- vKelly2(dat, type = "D2DWS1", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1TO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS1TO, file = './data/K2D2DWS1TO.rds')
    }
    
    ## --------------- 62. K2D2DWS2TO ------------------------
    if('K2D2DWS2TO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2TO.rds')))) {
        K2D2DWS2TO <- readRDS('./data/K2D2DWS2TO.rds')
      } else {
        stop('Kindly apply K2D2DWS2TO <- vKelly2(dat, type = "D2DWS2", dym.weight = "time", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2TO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'time', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS2TO, file = './data/K2D2DWS2TO.rds')
    }
    
    ## --------------- Dynamic Mixed : Daily + Daily ------------------------
    ## --------------- 63. K1D1DWS1DD ------------------------
    if('K1D1DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1DD.rds')))) {
        K1D1DWS1DD <- readRDS('./data/K1D1DWS1DD.rds')
      } else {
        stop('Kindly apply K1D1DWS1DD <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1DD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS1DD, file = './data/K1D1DWS1DD.rds')
    }
    
    ## --------------- 64. K1D1DWS2DD ------------------------
    if('K1D1DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2DD.rds')))) {
        K1D1DWS2DD <- readRDS('./data/K1D1DWS2DD.rds')
      } else {
        stop('Kindly apply K1D1DWS2DD <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2DD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS2DD, file = './data/K1D1DWS2DD.rds')
    }
    
    ## --------------- 65. K1D2DWS1DD ------------------------
    if('K1D2DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1DD.rds')))) {
        K1D2DWS1DD <- readRDS('./data/K1D2DWS1DD.rds')
      } else {
        stop('Kindly apply K1D2DWS1DD <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1DD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS1DD, file = './data/K1D2DWS1DD.rds')
    }
    
    ## --------------- 66. K1D2DWS2DD ------------------------
    if('K1D2DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2DD.rds')))) {
        K1D2DWS2DD <- readRDS('./data/K1D2DWS2DD.rds')
      } else {
        stop('Kindly apply K1D2DWS2DD <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2DD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS2DD, file = './data/K1D2DWS2DD.rds')
    }
    
    ## --------------- 67. K2D1DWS1DD ------------------------
    if('K2D1DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1DD.rds')))) {
        K2D1DWS1DD <- readRDS('./data/K2D1DWS1DD.rds')
      } else {
        stop('Kindly apply K2D1DWS1DD <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1DD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS1DD, file = './data/K2D1DWS1DD.rds')
    }
    
    ## --------------- 68. K2D1DWS2DD ------------------------
    if('K2D1DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2DD.rds')))) {
        K2D1DWS2DD <- readRDS('./data/K2D1DWS2DD.rds')
      } else {
        stop('Kindly apply K2D1DWS2DD <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2DD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS2DD, file = './data/K2D1DWS2DD.rds')
    }
    
    ## --------------- 69. K2D2DWS1DD ------------------------
    if('K2D2DWS1DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1DD.rds')))) {
        K2D2DWS1DD <- readRDS('./data/K2D2DWS1DD.rds')
      } else {
        stop('Kindly apply K2D2DWS1DD <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1DD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS1DD, file = './data/K2D2DWS1DD.rds')
    }
    
    ## --------------- 70. K2D2DWS2DD ------------------------
    if('K2D2DWS2DD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2DD.rds')))) {
        K2D2DWS2DD <- readRDS('./data/K2D2DWS2DD.rds')
      } else {
        stop('Kindly apply K2D2DWS2DD <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2DD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS2DD, file = './data/K2D2DWS2DD.rds')
    }
    
    ## --------------- Dynamic Mixed : Daily + Kick-Off Time ------------------------
    ## --------------- 71. K1D1DWS1DT ------------------------
    if('K1D1DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1DT.rds')))) {
        K1D1DWS1DT <- readRDS('./data/K1D1DWS1DT.rds')
      } else {
        stop('Kindly apply K1D1DWS1DT <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1DT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS1DT, file = './data/K1D1DWS1DT.rds')
    }
    
    ## --------------- 72. K1D1DWS2DT ------------------------
    if('K1D1DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2DT.rds')))) {
        K1D1DWS2DT <- readRDS('./data/K1D1DWS2DT.rds')
      } else {
        stop('Kindly apply K1D1DWS2DT <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2DT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS2DT, file = './data/K1D1DWS2DT.rds')
    }
    
    ## --------------- 73. K1D2DWS1DT ------------------------
    if('K1D2DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1DT.rds')))) {
        K1D2DWS1DT <- readRDS('./data/K1D2DWS1DT.rds')
      } else {
        stop('Kindly apply K1D2DWS1DT <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1DT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS1DT, file = './data/K1D2DWS1DT.rds')
    }
    
    ## --------------- 74. K1D2DWS2DT ------------------------
    if('K1D2DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2DT.rds')))) {
        K1D2DWS2DT <- readRDS('./data/K1D2DWS2DT.rds')
      } else {
        stop('Kindly apply K1D2DWS2DT <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2DT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS2DT, file = './data/K1D2DWS2DT.rds')
    }
    
    ## --------------- 75. K2D1DWS1DT ------------------------
    if('K2D1DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1DT.rds')))) {
        K2D1DWS1DT <- readRDS(K2D1DWS1DT, file = './data/K2D1DWS1DT.rds')
      } else {
        stop('Kindly apply K2D1DWS1DT <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1DT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS1DT, file = './data/K2D1DWS1DT.rds')
    }
    
    ## --------------- 76. K2D1DWS2DT ------------------------
    if('K2D1DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2DT.rds')))) {
        K2D1DWS2DT <- readRDS('./data/K2D1DWS2DT.rds')
      } else {
        stop('Kindly apply K2D1DWS2DT <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2DT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS2DT, file = './data/K2D1DWS2DT.rds')
    }
    
    ## --------------- 77. K2D2DWS1DT ------------------------
    if('K2D2DWS1DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1DT.rds')))) {
        K2D2DWS1DT <- readRDS('./data/K2D2DWS1DT.rds')
      } else {
        stop('Kindly apply K2D2DWS1DT <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1DT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS1DT, file = './data/K2D2DWS1DT.rds')
    }
    
    ## --------------- 78. K2D2DWS2DT ------------------------
    if('K2D2DWS2DT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2DT.rds')))) {
        K2D2DWS2DT <- readRDS('./data/K2D2DWS2DT.rds')
      } else {
        stop('Kindly apply K2D2DWS2DT <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2DT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS2DT, file = './data/K2D2DWS2DT.rds')
    }
    
    ## --------------- Dynamic Mixed : Daily + Dynamic ------------------------
    ## --------------- 79. K1D1DWS1DO ------------------------
    if('K1D1DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1DO.rds')))) {
        K1D1DWS1DO <- readRDS('./data/K1D1DWS1DO.rds')
      } else {
        stop('Kindly apply K1D1DWS1DO <- vKelly(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1DO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS1DO, file = './data/K1D1DWS1DO.rds')
    }
    
    ## --------------- 80. K1D1DWS2DO ------------------------
    if('K1D1DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2DO.rds')))) {
        K1D1DWS2DO <- readRDS('./data/K1D1DWS2DO.rds')
      } else {
        stop('Kindly apply K1D1DWS2DO <- vKelly(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2DO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS2DO, file = './data/K1D1DWS2DO.rds')
    }
    
    ## --------------- 81. K1D2DWS1DO ------------------------
    if('K1D2DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1DO.rds')))) {
        K1D2DWS1DO <- readRDS('./data/K1D2DWS1DO.rds')
      } else {
        stop('Kindly apply K1D2DWS1DO <- vKelly(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1DO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS1DO, file = './data/K1D2DWS1DO.rds')
    }
    
    ## --------------- 82. K1D2DWS2DO ------------------------
    if('K1D2DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2DO.rds')))) {
        K1D2DWS2DO <- readRDS('./data/K1D2DWS2DO.rds')
      } else {
        stop('Kindly apply K1D2DWS2DO <- vKelly(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2DO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS2DO, file = './data/K1D2DWS2DO.rds')
    }
    
    ## --------------- 83. K2D1DWS1DO ------------------------
    if('K2D1DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1DO.rds')))) {
        K2D1DWS1DO <- readRDS('./data/K2D1DWS1DO.rds')
      } else {
        stop('Kindly apply K2D1DWS1DO <- vKelly2(dat, type = "D1DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1DO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS1DO, file = './data/K2D1DWS1DO.rds')
    }
    
    ## --------------- 84. K2D1DWS2DO ------------------------
    if('K2D1DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2DO.rds')))) {
        K2D1DWS2DO <- readRDS('./data/K2D1DWS2DO.rds')
      } else {
        stop('Kindly apply K2D1DWS2DO <- vKelly2(dat, type = "D1DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2DO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS2DO, file = './data/K2D1DWS2DO.rds')
    }
    
    ## --------------- 85. K2D2DWS1DO ------------------------
    if('K2D2DWS1DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1DO.rds')))) {
        K2D2DWS1DO <- readRDS('./data/K2D2DWS1DO.rds')
      } else {
        stop('Kindly apply K2D2DWS1DO <- vKelly2(dat, type = "D2DWS1", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1DO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS1DO, file = './data/K2D2DWS1DO.rds')
    }
    
    ## --------------- 86. K2D2DWS2DO ------------------------
    if('K2D2DWS2DO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2DO.rds')))) {
        K2D2DWS2DO <- readRDS('./data/K2D2DWS2DO.rds')
      } else {
        stop('Kindly apply K2D2DWS2DO <- vKelly2(dat, type = "D2DWS2", dym.weight = "daily", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2DO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'daily', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS2DO, file = './data/K2D2DWS2DO.rds')
    }
    
    ## --------------- Dynamic Mixed : Dynamic + Daily ------------------------
    ## --------------- 87. K1D1DWS1OD ------------------------
    if('K1D1DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1OD.rds')))) {
        K1D1DWS1OD <- readRDS('./data/K1D1DWS1OD.rds')
      } else {
        stop('Kindly apply K1D1DWS1OD <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1OD <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS1OD, file = './data/K1D1DWS1OD.rds')
    }
    
    ## --------------- 88. K1D1DWS2OD ------------------------
    if('K1D1DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2OD.rds')))) {
        K1D1DWS2OD <- readRDS('./data/K1D1DWS2OD.rds')
      } else {
        stop('Kindly apply K1D1DWS2OD <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2OD <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D1DWS2OD, file = './data/K1D1DWS2OD.rds')
    }
    
    ## --------------- 89. K1D2DWS1OD ------------------------
    if('K1D2DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1OD.rds')))) {
        K1D2DWS1OD <- readRDS('./data/K1D2DWS1OD.rds')
      } else {
        stop('Kindly apply K1D2DWS1OD <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1OD <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS1OD, file = './data/K1D2DWS1OD.rds')
    }
    
    ## --------------- 90. K1D2DWS2OD ------------------------
    if('K1D2DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2OD.rds')))) {
        K1D2DWS2OD <- readRDS('./data/K1D2DWS2OD.rds')
      } else {
        stop('Kindly apply K1D2DWS2OD <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2OD <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K1D2DWS2OD, file = './data/K1D2DWS2OD.rds')
    }
    
    ## --------------- 91. K2D1DWS1OD ------------------------
    if('K2D1DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1OD.rds')))) {
        K2D1DWS1OD <- readRDS('./data/K2D1DWS1OD.rds')
      } else {
        stop('Kindly apply K2D1DWS1OD <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1OD <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS1OD, file = './data/K2D1DWS1OD.rds')
    }
    
    ## --------------- 92. K2D1DWS2OD ------------------------
    if('K2D1DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2OD.rds')))) {
        K2D1DWS2OD <- readRDS('./data/K2D1DWS2OD.rds')
      } else {
        stop('Kindly apply K2D1DWS2OD <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2OD <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D1DWS2OD, file = './data/K2D1DWS2OD.rds')
    }
    
    ## --------------- 93. K2D2DWS1OD ------------------------
    if('K2D2DWS1OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1OD.rds')))) {
        K2D2DWS1OD <- readRDS('./data/K2D2DWS1OD.rds')
      } else {
        stop('Kindly apply K2D2DWS1OD <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1OD <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS1OD, file = './data/K2D2DWS1OD.rds')
    }
    
    ## --------------- 94. K2D2DWS2OD ------------------------
    if('K2D2DWS2OD.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2OD.rds')))) {
        K2D2DWS2OD <- readRDS('./data/K2D2DWS2OD.rds')
      } else {
        stop('Kindly apply K2D2DWS2OD <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "daily") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2OD <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'daily')
      saveRDS(K2D2DWS2OD, file = './data/K2D2DWS2OD.rds')
    }
    
    ## --------------- Dynamic Mixed : Dynamic + Kick-Off Time ------------------------
    ## --------------- 95. K1D1DWS1OT ------------------------
    if('K1D1DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1OT.rds')))) {
        K1D1DWS1OT <- readRDS('./data/K1D1DWS1OT.rds')
      } else {
        stop('Kindly apply K1D1DWS1OT <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1OT <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS1OT, file = './data/K1D1DWS1OT.rds')
    }
    
    ## --------------- 96. K1D1DWS2OT ------------------------
    if('K1D1DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2OT.rds')))) {
        K1D1DWS2OT <- readRDS('./data/K1D1DWS2OT.rds')
      } else {
        stop('Kindly apply K1D1DWS2OT <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2OT <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D1DWS2OT, file = './data/K1D1DWS2OT.rds')
    }
    
    ## --------------- 97. K1D2DWS1OT ------------------------
    if('K1D2DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1OT.rds')))) {
        K1D2DWS1OT <- readRDS('./data/K1D2DWS1OT.rds')
      } else {
        stop('Kindly apply K1D2DWS1OT <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1OT <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS1OT, file = './data/K1D2DWS1OT.rds')
    }
    
    ## --------------- 98. K1D2DWS2OT ------------------------
    if('K1D2DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2OT.rds')))) {
        K1D2DWS2OT <- readRDS('./data/K1D2DWS2OT.rds')
      } else {
        stop('Kindly apply K1D2DWS2OT <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2OT <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K1D2DWS2OT, file = './data/K1D2DWS2OT.rds')
    }
    
    ## --------------- 99. K2D1DWS1OT ------------------------
    if('K2D1DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1OT.rds')))) {
        K2D1DWS1OT <- readRDS('./data/K2D1DWS1OT.rds')
      } else {
        stop('Kindly apply K2D1DWS1OT <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1OT <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS1OT, file = './data/K2D1DWS1OT.rds')
    }
    
    ## --------------- 100. K2D1DWS2OT ------------------------
    if('K2D1DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2OT.rds')))) {
        K2D1DWS2OT <- readRDS('./data/K2D1DWS2OT.rds')
      } else {
        stop('Kindly apply K2D1DWS2OT <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2OT <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D1DWS2OT, file = './data/K2D1DWS2OT.rds')
    }
    
    ## --------------- 101. K2D2DWS1OT ------------------------
    if('K2D2DWS1OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1OT.rds')))) {
        K2D2DWS1OT <- readRDS('./data/K2D2DWS1OT.rds')
      } else {
        stop('Kindly apply K2D2DWS1OT <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1OT <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS1OT, file = './data/K2D2DWS1OT.rds')
    }
    
    ## --------------- 102. K2D2DWS2OT ------------------------
    if('K2D2DWS2OT.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2OT.rds')))) {
        K2D2DWS2OT <- readRDS('./data/K2D2DWS2OT.rds')
      } else {
        stop('Kindly apply K2D2DWS2OT <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "time") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2OT <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'time')
      saveRDS(K2D2DWS2OT, file = './data/K2D2DWS2OT.rds')
    }
    
    ## --------------- Dynamic Mixed : Dynamic + Dynamic ------------------------
    ## --------------- 103. K1D1DWS1OO ------------------------
    if('K1D1DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS1OO.rds')))) {
        K1D1DWS1OO <- readRDS('./data/K1D1DWS1OO.rds')
      } else {
        stop('Kindly apply K1D1DWS1OO <- vKelly(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS1OO <- vKelly(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS1OO, file = './data/K1D1DWS1OO.rds')
    }
    
    ## --------------- 104. K1D1DWS2OO ------------------------
    if('K1D1DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D1DWS2OO.rds')))) {
        K1D1DWS2OO <- readRDS('./data/K1D1DWS2OO.rds')
      } else {
        stop('Kindly apply K1D1DWS2OO <- vKelly(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D1DWS2OO <- vKelly(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D1DWS2OO, file = './data/K1D1DWS2OO.rds')
    }
    
    ## --------------- 105. K1D2DWS1OO ------------------------
    if('K1D2DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS1OO.rds')))) {
        K1D2DWS1OO <- readRDS('./data/K1D2DWS1OO.rds')
      } else {
        stop('Kindly apply K1D2DWS1OO <- vKelly(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS1OO <- vKelly(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS1OO, file = './data/K1D2DWS1OO.rds')
    }
    
    ## --------------- 106. K1D2DWS2OO ------------------------
    if('K1D2DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'Kelly3', 'Kelly4', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K1D2DWS2OO.rds')))) {
        K1D2DWS2OO <- readRDS('./data/K1D2DWS2OO.rds')
      } else {
        stop('Kindly apply K1D2DWS2OO <- vKelly(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K1D2DWS2OO <- vKelly(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K1D2DWS2OO, file = './data/K1D2DWS2OO.rds')
    }
    
    ## --------------- 107. K2D1DWS1OO ------------------------
    if('K2D1DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS1OO.rds')))) {
        K2D1DWS1OO <- readRDS('./data/K2D1DWS1OO.rds')
      } else {
        stop('Kindly apply K2D1DWS1OO <- vKelly2(dat, type = "D1DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS1OO <- vKelly2(dat, type = 'D1DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS1OO, file = './data/K2D1DWS1OO.rds')
    }
    
    ## --------------- 108. K2D1DWS2OO ------------------------
    if('K2D1DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D1DWS2OO.rds')))) {
        K2D1DWS2OO <- readRDS('./data/K2D1DWS2OO.rds')
      } else {
        stop('Kindly apply K2D1DWS2OO <- vKelly2(dat, type = "D1DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D1DWS2OO <- vKelly2(dat, type = 'D1DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D1DWS2OO, file = './data/K2D1DWS2OO.rds')
    }
    
    ## --------------- 109. K2D2DWS1OO ------------------------
    if('K2D2DWS1OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS1OO.rds')))) {
        K2D2DWS1OO <- readRDS('./data/K2D2DWS1OO.rds')
      } else {
        stop('Kindly apply K2D2DWS1OO <- vKelly2(dat, type = "D2DWS1", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS1OO <- vKelly2(dat, type = 'D2DWS1', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS1OO, file = './data/K2D2DWS1OO.rds')
    }
    
    ## --------------- 110. K2D2DWS2OO ------------------------
    if('K2D2DWS2OO.rds' %in% dir('./data')) {
      if(all(c('data', 'Kelly1', 'Kelly2', 'weight', 'weight.stakes') %in% 
             names(readRDS(file = './data/K2D2DWS2OO.rds')))) {
        K2D2DWS2OO <- readRDS('./data/K2D2DWS2OO.rds')
      } else {
        stop('Kindly apply K2D2DWS2OO <- vKelly2(dat, type = "D2DWS2", dym.weight = "dynamic", dym.weight.stakes = "dynamic") to measure the Kelly model.')
      }
    } else {
      K2D2DWS2OO <- vKelly2(dat, type = 'D2DWS2', dym.weight = 'dynamic', dym.weight.stakes = 'dynamic')
      saveRDS(K2D2DWS2OO, file = './data/K2D2DWS2OO.rds')
    }
    
    ## list the Kelly models for easier handling.
    Kmodels <- c(K1, K2, K1W1, K1W2, K2W1, K2W2, #6
                 K1W1WS1, K1W1WS2, K1W2WS1, K1W2WS2, #4 #10
                 K2W1WS1, K2W1WS2, K2W2WS1, K2W2WS2, #4 #14
                 K1D1, K1D2, K2D1, K2D2, #4 #18
                 K1DWS1, K1DWS2, K2DWS1, K2DWS2, #4 #22
                 K1D1WS1, K1D1WS2, K1D2WS1, K1D2WS2, #4 #26
                 K2D1WS1, K2D1WS2, K2D2WS1, K2D2WS2, #4 #30
                 K1W1DWS1, K1W1DWS2, K1W2DWS1, K1W2DWS2, #4 #34
                 K2W1DWS1, K2W1DWS2, K2W2DWS1, K2W2DWS2, #4 #38
                 K1D1DWS1, K1D1DWS2, K1D2DWS1, K1D2DWS2, #4 #42
                 K2D1DWS1, K2D1DWS2, K2D2DWS1, K2D2DWS2, #4 #46
                 K1D1DWS1TT, K1D1DWS2TT, K1D2DWS1TT, K1D2DWS2TT, #4 #50
                 K2D1DWS1TT, K2D1DWS2TT, K2D2DWS1TT, K2D2DWS2TT, #4 #54
                 K1D1DWS1TO, K1D1DWS2TO, K1D2DWS1TO, K1D2DWS2TO, #4 #58
                 K2D1DWS1TO, K2D1DWS2TO, K2D2DWS1TO, K2D2DWS2TO, #4 #62
                 K1D1DWS1DD, K1D1DWS2DD, K1D2DWS1DD, K1D2DWS2DD, #4 #66
                 K2D1DWS1DD, K2D1DWS2DD, K2D2DWS1DD, K2D2DWS2DD, #4 #70
                 K1D1DWS1DT, K1D1DWS2DT, K1D2DWS1DT, K1D2DWS2DT, #4 #74
                 K2D1DWS1DT, K2D1DWS2DT, K2D2DWS1DT, K2D2DWS2DT, #4 #78
                 K1D1DWS1DO, K1D1DWS2DO, K1D2DWS1DO, K1D2DWS2DO, #4 #82
                 K2D1DWS1DO, K2D1DWS2DO, K2D2DWS1DO, K2D2DWS2DO, #4 #86
                 K1D1DWS1OD, K1D1DWS2OD, K1D2DWS1OD, K1D2DWS2OD, #4 #90
                 K2D1DWS1OD, K2D1DWS2OD, K2D2DWS1OD, K2D2DWS2OD, #4 #94
                 K1D1DWS1OT, K1D1DWS2OT, K1D2DWS1OT, K1D2DWS2OT, #4 #98
                 K2D1DWS1OT, K2D1DWS2OT, K2D2DWS1OT, K2D2DWS2OT, #4 #102
                 K1D1DWS1OO, K1D1DWS2OO, K1D2DWS1OO, K1D2DWS2OO, #4 #106
                 K2D1DWS1OO, K2D1DWS2OO, K2D2DWS1OO, K2D2DWS2OO) #4 #110
    
    return(Kmodels)
    
  } else {
    stop('Kindly select action = "save" or action = "load".')
  }
}
