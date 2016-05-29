## Loading the packages
if(!'BBmisc' %in% installed.packages()){
  install.packages('BBmisc')}
if(!'BiocParallel' %in% installed.packages()){
  source("http://bioconductor.org/biocLite.R")
  biocLite("BiocParallel")}
if(!'seleniumJars' %in% installed.packages()){
  install_github('LluisRamon/seleniumJars')}

suppressPackageStartupMessages(library('BBmisc'))
suppressPackageStartupMessages(lib(c('zoo','stringi','stringr','reshape','reshape2','plyr','dplyr','magrittr',
                                     'ggplot2','ggthemes','plotly','foreach','memoise','doMC','doParallel','BiocParallel',
                                     'markdown','parallel','rmarkdown','manipulate','knitr','turner','scales',
                                     'lubridate','whisker'))) #'RStudioAMI','editR'

## ---------------------------------------------------------------------------------------------
## http://stackoverflow.com/questions/22954623/view-markdown-generated-html-in-rstudio-viewer
render(paste0(getwd(),'/Betting Strategy and Model Validation.Rmd'),'all')
#'@ View(paste0(getwd(),'/Betting_Strategy_and_Model_Validation.html'))
browseURL(paste0(getwd(),'/Betting Strategy and Model Validation.html'))

## https://github.com/swarm-lab/editR
editR(paste0(getwd(),'/Betting Strategy and Model Validation.Rmd'))


## Besides, need to scrap the final-scores / half-time scores / result of soccer matches
teamID <- sort(unique(c(as.character(mbase$Home), as.character(mbase$Away))))
dateID <- sort(unique(mbase$Date)); spboDate <- gsub('-','',dateID)
lnk <- paste0('http://www8.spbo.com/history.plex?day=',spboDate,'&l=en')

## http://stackoverflow.com/questions/2158780/r-catching-an-error-and-then-branching-logic
## http://www.win-vector.com/blog/2012/10/error-handling-in-r/
## Due to the scrapSPBO function scrapped unmatched data, example lnk[827],
##  therefore I rewrite the function as scrapSPBO2
source(paste0(getwd(),'/function/scrapSPBO2.R'))
scrapSPBO2(lnk=lnk, dateID=dateID, path='livescore', parallel=FALSE)

## Read scraped spbo datasets
source(paste0(getwd(),'/function/readSPBO.R'))
spboData <- readSPBO(dateID=dateID, path='livescore', parallel=FALSE)



## https://github.com/pablobarbera/instaR
## https://github.com/pablobarbera/Rfacebook
install_github ## can try during free time
## ---------------------------------------------------------------------------------------------
## Load the scraped spbo livescore datasets.
##... will take some times since dim spboData [156841 x 17]
source(paste0(getwd(),'/function/readSPBO2.R'))
spboData <- readSPBO2(dateID=dateID, parallel=TRUE)

## filter spboTeamID
spboTeamID <- sort(c(unique(as.vector(spboData$Home)),unique(as.vector(spboData$Away))))
tmID <- teamID[!teamID %in% mbase$others]

spboData[(is.na(spboData$Date))&(nchar(as.vector(spboData$Time))==5),]
spboData[subset(spboData, (is.na(data.frame(spboData)$Date))&(nchar(as.vector(spboData$Time))==5))$X,]

> dim(mbase$datasets)
[1] 48744    17
> dim(spboData)
[1] 319744     20

mbase$datasets[mbase$datasets$DateUK %in% spboData$DateUK,]
#Source: local data frame [17,934 x 17]

na.omit(mbase$datasets[mbase$datasets$DateUK %in% spboData$DateUK,][order(mbase$datasets$No,decreasing=FALSE),])
#Source: local data frame [25,489 x 17]

library('tau')
library('textcat')
library('stringdist')











http://wizardofvegas.com/forum/gambling/sports/10555-halt-time-betting/3/
http://quant.stackexchange.com/questions/2500/how-to-apply-the-kelly-criterion-when-expected-return-may-be-negative
https://en.wikipedia.org/wiki/Gambling_and_information_theory
http://www.eecs.harvard.edu/cs286r/courses/fall12/papers/Thorpe_KellyCriterion2007.pdf
http://www.sportsbookreview.com/betting-tools/kelly-calculator/
http://thestakingmachine.com/laykelly.php
### http://www.sportsbettingcalculator.co.uk/kelly-staking-calculator/
http://tipstertables.com/blog/betting-system-using-tipster-statistics-and-kelly-criterion
########################################################################################

## Scrape the League in order to assign the virogish/spread margins/overrounds
library(RSelenium)
teamID <- sort(unique(unlist(mbase$Home), unlist(mbase$Away)))
lnk <- 'http://www8.spbo.com/history.plex?day=20110107&l=en'

#'@ system('java -jar selenium-server-standalone.jar')
checkForServer() ## if you need the stand-alone Java binary
startServer()
webDr <- remoteDriver$new()
webDr$open()
webDr$navigate(lnk)
webDr$navigate("http://www.bbc.co.uk")
webDr$goBack()
webDr$goForward()
webDr$quit()

## https://github.com/greenore/RSeleniumUtilities
library(RSeleniumUtilities)
RSeleniumUtilities::checkSelenium()
webDr <- ieDriver()
webDr <- firefoxDriver(use_profile=TRUE, profile_name="selenium")
webDr <- chromeDriver(use_profile=TRUE, profile_name="selenium", internal_testing=TRUE)


## Linear regression
llply(split(mbase,mbase$Sess),function(x)lm(PL~Selection+HCap+Price,x))


#'@ stopCluster(cl)

x <- seq(as.Date('2011-01-01'), as.Date('2015-07-31'), by='months')
y <- seq(min(mbase$PL),max(mbase$Stake), by=10000)
labels <- date_format('%b')(x)
breaks <- as.Date(sort(c(as.POSIXct(x), as.POSIXct(seq(min(mbase$Date), 
          max(mbase$Date), by='months')), ymd('2015-08-01'))))
labels <- c('', as.vector(rbind(labels, rep('', length(labels)))))

ggplot(data=mbase, aes(x=x, y=y, shape=AHOU)) +
  geom_line(aes(y = mbase$Stake, colour = 'Stake'), size=1.5) +
  geom_line(aes(y = mbase$PL, colour = 'PL'), size=1.5) +
  geom_point(size=2, fill='blue') + expand_limits(y=0) +         ## Set y range to include 0
  scale_colour_hue(name='PL', l=30) +                          ## Set legend title use darker colors (lightness=30)
  scale_shape_manual(name='PL', values=c(22,21)) +             ## Use points with a fill color
  scale_shape_manual(values=c(22,21)) + xlab('Time of Day') + ylab('HK Dollars (HKD)') +
  scale_x_date(labels = labels, breaks = breaks, limits=range(breaks)) + ## scale_x_date(labels = date_format("%b"),breaks = date_breaks("months")) +
  ggtitle('Stakes and Profit & Lose') +                          ## Set title
  theme_bw() + theme(legend.position=c(.7, .4))               ## Position legend inside this must go after theme_bw

qplot(Stake, data=mbase, geom='density', fill=AHOU, alpha=I(.5), 
      main='Turnover and P&L', xlab='Year in Month', 
      ylab='HKD Amount') + scale_x_date(breaks=date_breaks('months'), labels = date_format("%b"))

### http://statisticalrecipes.blogspot.com/2012/02/simulating-genetic-drift.html
dtm <- factor(sapply(strsplit(as.character(mbase$Date),'-'),function(x) x[2]))
dtm <- data.frame(month=mapvalues(dtm, sort(levels(dtm)),c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')),
                  mbase$Stake/10000, mbase$PL/10000); names(dtm) <- c('Month','Stake','PL')
sdata <- data.frame(Date=factor(paste0(dtm$Month,'-',mbase$Sess)),dtm[-1]); rm(dtm)
sdata <- ddply(sdata, .(Date), summarise, Stake=sum(Stake), PL=sum(PL))
sdata[order(sdata$Date, decresing=FALSE),]

## plot on same grid, each series colored differently -- 
## good if the series have same scale
ggplot(sim_data, aes(Month,'HKD 0000')) + geom_line(aes(colour = Series)) + 
  scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
  theme(axis.text.x=element_text(face="bold",colour="red",size=14))

## ==================================================================================================================================
## http://wenku.baidu.com/view/3574f639580216fc700afdfc.html
## https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.models.html
## http://doc.qkzz.net/article/e6f33685-e220-4803-8c89-3228501b9412.htm






