## Setup Options, Loading Required Libraries and Preparing Environment
## Setup `knitr` options and loading the required libraries.

## Setting to omit all warnings
options(warn=-1)

## Loading the packages
if(!suppressMessages(require('BBmisc'))){
  install.packages('BBmisc')}
suppressMessages(library('BBmisc'))

if(!suppressAll(require('devtools'))){
  suppressAll(install.packages('devtools'))}
if(!suppressAll(require('BiocParallel'))){
  suppressAll(devtools::install_github('Bioconductor/BiocParallel'))}

## http://www.r-bloggers.com/new-package-dplyrr-utilities-for-comfortable-use-of-dplyr-with-databases
## direct connect to database (if any)
#'@ if(!'dplyrr' %in% installed.packages()){
#'@   devtools::install_github("hoxo-m/dplyrr")}
#'@ install.packages('nycflights13')
#'@ library(c('dplyrr','nycflights13'))

pkgs <- c('devtools','zoo','chron','stringr','reshape','reshape2','data.table','sparkline',
          'DT','plyr','dplyr','purrr','sqldf','magrittr','foreach','manipulate','ggplot2','ggthemes',
          'proto','extrafont','directlabels','PerformanceAnalytics','plotly','doParallel','rvest',
          'highlightHTML','knitr','rmarkdown','scales','lubridate','tidyr','whisker','gtable','grid',
          'gridExtra','pander','arules','arulesViz','googleVis')
#'@ c('memoise','RStudioAMI','parallel','BiocParallel','RSelenium','doMC','editR') #load if needed
suppressAll(lib(pkgs)); rm(pkgs)

## Load the functions
funs <- c('readfirmDatasets.R', 'arrfirmDatasets.R', 'readSPBO.R')
l_ply(funs, function(x) source(paste0(getwd(), '/function/', x))); rm(funs)

## Creating a parallel computing Cluster and support functions.
## Preparing the parallel cluster using the cores
doParallel::registerDoParallel(cores = 16)
#'@ BiocParallel::register(MulticoreParam(workers=8))

## knitr configuration
opts_knit$set(progress=FALSE)
opts_chunk$set(echo=TRUE, message=FALSE, tidy=TRUE, comment=NA, fig.path="figure/", fig.keep="high", 
               fig.width=10, fig.height=6, fig.align="center")

## Table width setting
panderOptions('table.split.table', Inf)

## Setup plotly API
Sys.setenv('plotly_username'='englianhu')
Sys.setenv('plotly_api_key'='xxxxxxxxx')

## Set the googleVis options first to change the behaviour of plot.gvis, so that only the chart 
##  component of the HTML file is written into the output file.
op <- options(gvis.plot.tag='chart')

# Define image sizes
img.width <- 450
img.height <- 300
options(RCHART_HEIGHT = img.height, RCHART_WIDTH = img.width)
opts_chunk$set(fig.width=6, fig.height=4)


