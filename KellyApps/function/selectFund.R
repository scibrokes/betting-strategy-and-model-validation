selectFund <- function(selectedFund, pth = './data1/') {
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('plyr'))
  suppressAll(library('stringr'))
  suppressAll(library('tidyverse'))
  suppressAll(library('quantmod'))
  suppressAll(library('formattable'))
  
  fundsopt <- list.files(pth, pattern = '.rds$')
  fundsopt <- c(tail(fundsopt, 1), fundsopt) %>% unique %>% str_replace_all('.rds', '')
  
  sfund <- read_rds(path = paste0(pth, selectedFund, '.rds'))
  DateUS <- data.frame(sfund) %>% row.names %>% as.Date
  
  sfund1 <- data.frame(Op(sfund), Hi(sfund), Lo(sfund), Cl(sfund)) %>% tbl_df %>% 
    mutate_each(funs(currency))
  sfund2 <- data.frame(Vo(sfund), Ad(sfund)) %>% tbl_df %>% 
    #'@ mutate_each(funs(formattable::digits(., 0, format = 'd', big.mark = ','))) #suddenly error
    map_if(is.numeric, formattable::digits, 0, format = 'd', big.mark = ',') %>% 
        data.frame %>% tbl_df
  
  sfundDT <- data.frame(DateUS, sfund1, sfund2) %>% tbl_df
  rm(DateUS, sfund1, sfund2)
  
  tmp <- list(sfund = sfund, sfundDT = sfundDT)
  return(tmp)
}

