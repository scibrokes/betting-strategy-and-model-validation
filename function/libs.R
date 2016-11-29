## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Setting to omit all warnings
options(warn = -1)

## ========= Load Packages =================================
## Loading the package 'BBmisc'
if(suppressMessages(!require('BBmisc'))){
  install.packages('BBmisc', repos = 'https://cran.rstudio.com')}
suppressMessages(library('BBmisc'))

#'@ if(suppressAll(!require('devtools'))){
#'@   suppressAll(install.packages('devtools'))}
#'@ if(suppressAll(!require('BiocParallel'))){
#'@   suppressAll(devtools::install_github('Bioconductor/BiocParallel'))}

## http://www.r-bloggers.com/new-package-dplyrr-utilities-for-comfortable-use-of-dplyr-with-databases
## direct connect to database (if any)
#'@ if(!'dplyrr' %in% installed.packages()){
#'@   devtools::install_github("hoxo-m/dplyrr")}
#'@ install.packages('nycflights13')
#'@ library(c('dplyrr', 'nycflights13'))

## Loading multiple packages at once
pkgs <- c('tufte', 'devtools', 'zoo', 'chron', 'stringr', 'reshape', 'reshape2', 'data.table', 'lme4',
          'stringdist', 'DT', 'plyr', 'tidyverse', 'magrittr', 'foreach', 'manipulate', 'sparkline',
          'ggthemes', 'proto', 'extrafont', 'directlabels', 'PerformanceAnalytics', 'plotly', 'rstan',
          'doParallel', 'rvest', 'highlightHTML', 'knitr', 'rmarkdown', 'scales', 'lubridate', 
          'whisker', 'gtable', 'grid', 'gridExtra', 'arules', 'arulesViz', 'googleVis', 'highcharter', 
          'xtable', 'rCharts', 'stargazer', 'shiny', 'shinyjs', 'formattable', 'htmltools', 
          'DiagrammeR')
#'@ c('memoise', 'RStudioAMI', 'pander', 'parallel', 'sqldf', 'BiocParallel', 'RSelenium', 
#'@   'doMC', 'editR', textreg') #load if needed
#'@ suppressAll(lib(pkgs)
suppressMessages(plyr::l_ply(pkgs, require, character.only = TRUE))
rm(pkgs)

## Load the functions
funs <- c('scrapSPBO.R', 'readfirmData.R', 'arrfirmData.R', 'readSPBO.R', 'vKelly.R', 
          'vKelly2.R', 'simulateKelly.R', 'bvp.R') #'stopImplicitCluster2.R'
plyr::l_ply(funs, function(x) source(paste0('./function/', x))); rm(funs)

## ========= Setup parallel computing =================================
## Creating a parallel computing Cluster and support functions.
## Preparing the parallel cluster using the cores
doParallel::registerDoParallel(cores = detectCores())
#'@ doParallel::registerDoParallel(cores = 16)
#'@ BiocParallel::register(MulticoreParam(workers = 8))

## Set the googleVis options first to change the behaviour of plot.gvis, so that only the chart 
##  component of the HTML file is written into the output file.
op <- options(gvis.plot.tag = 'chart')

