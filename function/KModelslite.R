KModelslite <- function(action = 'load', rm.files = as.logical(TRUE), 
                        overwrite = as.logical(FALSE), parallel = FALSE) {
  ## A lite version of KModels() due to the function always face size allocation issues.
  ## 
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
  source('./function/listKelly.R', local = TRUE)
  
  if(parallel == TRUE) {
    suppressPackageStartupMessages(library('doParallel'))
    registerDoParallel(cores = detectCores())
  }
  
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
  
  ## ================== Internal Function ================================
  ## listKelly()
  
  ## ================== Kelly names ================================
  KMnames <- c('K1', 'K2', 'K1W1', 'K1W2', 'K2W1', 'K2W2', #6
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
  
  if(action == 'save') {
    ## ================== Save Models ================================
    KM <- llply(KMnames, listKelly, type = 'save', overwrite = overwrite, 
                rm.files = rm.files, .parallel = parallel)
  
  } else if(action == 'load') {
    ## ================== Load Models ================================
    KM <- llply(KMnames, listKelly, type = 'save', .parallel = parallel)
  }
  
  return(KM)
}
