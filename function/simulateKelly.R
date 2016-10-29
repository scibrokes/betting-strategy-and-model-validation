simulateKelly <- function(mbase, type = 'flat', weight.stakes, weight, maxit = 100, parallel = FALSE) {
  ## Comparison of various fractional Kelly models
  ## mbase = a converted data frame by using readfirmData() and arrfirmData()s.
  ## type = 'flat' ot 'dynamic' for static or dynamic which is simulate the process to get the optimal weight 
  ##   parameter.
  ## weight.stakes = a numeric weight parameter in single or vector format. Manual weight only work on 'flat' type.
  ## weight = a numeric weight parameter in single or vector format. Manual weight only work on 'flat' type.
  ## maxit = maximum iteration for the dynamic process. Only work on 'dynamic' type.
  ##   Once choose 'dynamic'.
  
  ## --------------------- Load packages --------------------------------
  suppressMessages(library('formattable'))
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(source('./function/vKelly.R'))
  
  ## --------------------- Data validation -------------------------------- 
  if(!is.data.frame(mbase)) stop('Kindly apply the readfirmData() and arrfirmData() 
                                 in order to turn the data into a fittable data frame.')
  
  if(!is.logical(parallel)) parallel <- as.logical(as.numeric(parallel))
  
  if(!c('flat', 'dynamic') %in% type) {
    
    stop('Kindly choose "flat" or "dynamic" for parameter named "type". You can choose 
         fit the "weight" parameter controller for both models.')
    
  } else {
    if(type == 'dynamic') {
      
      wt <- data_frame(No = seq(nrow(mbase)))
      
      if(is.null(weight.stakes)) {
        wt$weight.stakes <- 1
      } else {
        if(!is.vector(weight.stakes)) {
          stop('Kindly insert a range of vector or single numeric value as weight.stakes parameter.')
        } else {
          if(is.vector(weight.stakes)) wt$weight.stakes <- weight.stakes
        }
      }
      
      if(is.null(weight)) {
        wt$weight <- 1
      } else {
        if(!is.vector(weight)) {
          stop('Kindly insert a range of vector or single numeric value as weight parameter.')
        } else {
          if(is.vector(weight)) wt$weight <- weight
        }
      }
      
      if(is.null(maxit)) {
        maxit <- 1
      } else {
        if(!is.numeric(maxit)) {
          stop('Kindly insert a numeric value as maximum iteration parameter.')
        } else {
        maxit <- maxit
        }
      }
    } else {
      wt <- data_frame(No = seq(nrow(mbase)))
      
      if(is.null(weight.stakes)) {
        wt$weight.stakes <- 1
      } else {
        if(!is.vector(weight.stakes)) {
          stop('Kindly insert a range of vector or single numeric value as weight.stakes parameter.')
        } else {
          if(is.vector(weight.stakes)) wt$weight.stakes <- weight.stakes
        }
      }
      
      if(is.null(weight)) {
        wt$weight <- 1
      } else {
        if(!is.vector(weight)) {
          stop('Kindly insert a range of vector or single numeric value as weight parameter.')
        } else {
          if(is.vector(weight)) wt$weight <- weight
        }
      }
      
      if(is.null(maxit)) {
        maxit <- 1
      } else {
        if(!is.vector(maxit)) stop('Kindly insert a range of vector or single numeric value as maximum iteration parameter.')
      }
    }
  }
  
  
  
}