## Setup Options, Loading Required Libraries and Preparing Environment

## Setting to omit all warnings
options(warn = -1)

## Loading the package 'BBmisc'
if(suppressMessages(!require('BBmisc'))){
  install.packages('BBmisc')}
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
pkgs <- c('devtools', 'zoo', 'chron', 'stringr', 'reshape', 'reshape2', 'data.table', 'sparkline', 
          'stringdist', 'DT', 'plyr', 'dplyr', 'purrr', 'magrittr', 'foreach', 'manipulate', 'ggplot2', 
          'ggthemes', 'proto', 'extrafont', 'directlabels', 'PerformanceAnalytics', 'plotly', 
          'doParallel', 'rvest', 'highlightHTML', 'knitr', 'rmarkdown', 'scales', 'lubridate', 'tidyr', 
          'whisker', 'gtable', 'grid', 'gridExtra', 'arules', 'arulesViz', 'googleVis', 'formattable')
#'@ c('memoise', 'RStudioAMI', 'pander', 'parallel', 'sqldf', 'BiocParallel', 'RSelenium', 
#'@   'doMC', 'editR') #load if needed
#'@ suppressAll(lib(pkgs)
suppressMessages(plyr::l_ply(pkgs, require, character.only = TRUE))
rm(pkgs)

## Load the functions
funs <- c('scrapSPBO.R', 'readfirmData.R', 'arrfirmData.R', 'readSPBO.R')
l_ply(funs, function(x) source(paste0('function/', x))); rm(funs)

## Creating a parallel computing Cluster and support functions.
## Preparing the parallel cluster using the cores
#'@ doParallel::registerDoParallel(cores = 16)
#'@ BiocParallel::register(MulticoreParam(workers = 8))

## Set the googleVis options first to change the behaviour of plot.gvis, so that only the chart 
##  component of the HTML file is written into the output file.
op <- options(gvis.plot.tag = 'chart')


