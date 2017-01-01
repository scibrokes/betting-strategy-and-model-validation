selectFund <- function(selectedFund) {
  suppressPackageStartupMessages(library("BBmisc"))
  suppressAll(library('plyr'))
  suppressAll(library('stringr'))
  suppressAll(library('tidyverse'))
  suppressAll(library('quantmod'))
  
  fundsopt <- list.files(pattern = '.rds$')
  fundsopt <- c(tail(fundsopt, 1), fundsopt) %>% unique %>% str_replace_all('.rds', '')
  
  sfund <- read_rds(path = paste0(selectedFund, '.rds'))
  DateUS <- data.frame(sfund) %>% row.names %>% as.Date
  
  sfund1 <- data.frame(Op(sfund), Hi(sfund), Lo(sfund), Cl(sfund)) %>% tbl_df %>% 
    mutate_each(funs(currency))
  sfund2 <- data.frame(Vo(sfund), Ad(sfund)) %>% tbl_df %>% 
    mutate_each(funs(digits(., 4, format = 'd', big.mark = ',')))
  
  sfundDT <- data.frame(DateUS, sfund1, sfund2) %>% tbl_df
  rm(DateUS, sfund1, sfund2)
  
  tmp <- list(sfund = sfund, sfundDT = sfundDT)
  return(tmp)
}

