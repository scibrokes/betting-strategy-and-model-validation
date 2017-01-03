## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
suppressPackageStartupMessages(library("BBmisc"))
suppressAll(library('plyr'))
suppressAll(library('stringr'))
suppressAll(library('tidyverse'))
suppressAll(library("shiny"))
suppressAll(library('shinyBS'))
suppressAll(library('highcharter'))
suppressAll(library('DT'))
suppressAll(library('quantmod'))
suppressAll(source("./function/selectFund.R"))
suppressAll(source("./function/compareKelly.R"))
suppressAll(source("./function/plotChart2.R"))

## ========= Read Data =================================
## Load saved dataset to save the loading time.
## directly load the dataset from running chunk `read-data-summary-table` and also chunk `scrap-data`. 
## The spboData for filtering leagues and matches scores purpose. Kindly refer to file named 
## `compacted data - shinyData.txt` inside folder `data`.

## Run above codes and save.images() and now directly load for shinyApp use.
#'@ load('./shinyData.RData', envir = .GlobalEnv)
#'@ load('./K1mean1.rda', envir = .GlobalEnv)
#'@ load('./K1mean2.rda', envir = .GlobalEnv)
#'@ load('./K2mean1.rda', envir = .GlobalEnv)
#'@ load('./K2mean2.rda', envir = .GlobalEnv)

fundsopt <<- list.files(pattern = '.rds$')
fundsopt <<- c(tail(fundsopt, 1), fundsopt) %>% unique %>% str_replace_all('.rds', '')

##http://stackoverflow.com/questions/33020558/embed-iframe-inside-shiny-app
members <<- list(
  'Natural Language Analysis' = 'http://rpubs.com/englianhu/natural-language-analysis', 
  'Part I' = 'https://englianhu.github.io/2016/09/Betting%20Strategy%20and%20Model%20Validation/Betting_Strategy_and_Model_Validation_-_Part_01.html', 
  'Part II' = 'http://rpubs.com/englianhu/208636', 
  'regressionApps' = 'https://beta.rstudioconnect.com/content/1807/')

