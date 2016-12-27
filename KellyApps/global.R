## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
suppressMessages(library('utils'))
suppressMessages(library('shiny'))
suppressMessages(library('shinyjs'))
suppressMessages(library('plyr'))
suppressMessages(library('dplyr'))
suppressMessages(library('magrittr'))
suppressMessages(library('formattable'))
suppressMessages(library('stringr'))

## ========= Read Data =================================
## Load saved dataset to save the loading time.
## directly load the dataset from running chunk `read-data-summary-table` and also chunk `scrap-data`. 
## The spboData for filtering leagues and matches scores purpose. Kindly refer to file named 
## `compacted data - shinyData.txt` inside folder `data`.

## Run above codes and save.images() and now directly load for shinyApp use.
load('./shinyData.RData', envir = .GlobalEnv)
load('./K1mean1.rda', envir = .GlobalEnv)
load('./K1mean2.rda', envir = .GlobalEnv)
load('./K2mean1.rda', envir = .GlobalEnv)
load('./K2mean2.rda', envir = .GlobalEnv)

lRiskProf <<- ddply(dat[c('League', 'Stakes')], .(League), summarise, min = currency(min(Stakes)), 
                    mean = currency(mean(Stakes)), median = currency(median(Stakes)), 
                    sd = currency(sd(Stakes)), max = currency(max(Stakes))) %>% tbl_df %>% 
  mutate(League = factor(League))

## ========= Kelly Models ================================
## 



