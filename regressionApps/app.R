## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
suppressMessages(library('utils'))
suppressMessages(require('shiny', quietly = TRUE))
suppressMessages(require('shinyjs', quietly = TRUE))
suppressMessages(require('plyr', quietly = TRUE))
suppressMessages(require('dplyr', quietly = TRUE))
suppressMessages(require('magrittr', quietly = TRUE))
suppressMessages(require('broom', quietly = TRUE))
suppressMessages(require('formattable', quietly = TRUE))

appCSS <- "
#loading-content {
  position: absolute;
  background: #3498DB;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #D4AC0D;
  <img src='./loader.gif' alt='alternative text'>
}
"

## ========= Read Data =================================
## Read the data
## Refer to **Testing efficiency of coding.Rmd** at chunk `get-data-summary-table-2.1`
#'@ years <- seq(2011, 2015)

## Here I take the majority leagues setting profile which are "league-10-12"
## fMYPriceB = Back with vigorish price; fMYPriceL = Lay with vigorish price
## Here we term as Fair Odds
#'@ lProfile <- c(AH = 0.10, OU = 0.12)
#'@ pth <- paste0(dirname(getwd()), '/data/')

#'@ mbase <- readfirmData(years = years, pth = pth) %>% arrfirmData(lProfile = lProfile)

## In order to analyse the AHOU, here I need to filter out all soccer matches other than AHOU. (For example : Corners, Total League Goals etc.)
## the stakes amount display as $1 = $10,000
#'@ mbase$datasets[!(mbase$datasets$Home %in% mbase$corners)|!(mbase$datasets$Away %in% mbase$corners),]
#'@ dat <- mbase$datasets %>% filter((!Home %in% mbase$others)|(!Away %in% mbase$others)) %>% mutate(Stakes = Stakes/10000, Return = Return/10000, PL = PL/10000)

## Get the investment return rates per annun
## http://www.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf
## value rRates is based on annual EMProb/netProb ratio, while EMProb get from equation 4.1.2
#'@ m <- ddply(dat, .(Sess), summarise, Stakes = sum(Stakes), Return = sum(Return), n = length(Sess), rRates = Return / Stakes)

## http://www.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf
## value R.rates is based on annual EMProb/netProb ratio, while EMProb get from equation 4.1.2
## Please refer to function arrfirmDatasets()
#'@ dat %<>% mutate(rEMProbB = round(unlist(sapply(split(m, m$Sess), function(x) rep(x$rRates, x$n))) * netProbB, 6), rEMProbL = round(1 - rEMProbB, 6))

#'@ rm(m, lProfile, readfirmData, arrfirmData, mbase)

## Run above codes and save.images() and now directly load for shinyApp use.
load('./shinyData.RData', envir = .GlobalEnv)

## ========= Linear Regression ================================
## Choosing the variables of linear models
## The net probabilities might open diversified handicap, therefore the HCap 
##   parameter need to be insert as one of parameter since the return of 
##   draw-no-bet, win-half, win-full, loss-half, loss-full
##   (example : 0, 0/0.5, 0.5, 0.5/1, 1) affect the return of investment.
lm0 <<- lm(Return ~ Stakes, data = dat)
lm1 <<- lm(Return ~ Stakes + HCap, data = dat)
lm2 <<- lm(Return ~ Stakes + netProbB, data = dat)
lm3 <<- lm(Return ~ Stakes + HCap + netProbB, data = dat)
lm4 <<- lm(Return ~ Stakes + ipRange, data = dat)
lm5 <<- lm(Return ~ Stakes + ipHCap, data = dat)
lm6 <<- lm(Return ~ Stakes + HCap + ipRange, data = dat)
lm7 <<- lm(Return ~ Stakes + CurScore + ipHCap, data = dat)
lm8 <<- lm(Return ~ Stakes + CurScore + ipRange, data = dat)
lm9 <<- lm(Return ~ Stakes + CurScore + ipRange + ipHCap, data = dat)

## Linear Interative Effect Models
lm10 <<- lm(Return ~ HCap + netProbB + HCap:netProbB, data = dat)
lm11 <<- lm(Return ~ Stakes + ipHCap + ipRange + ipHCap:ipRange, data = dat)
lm12 <<- lm(Return ~ Stakes + CurScore + ipRange + ipHCap + CurScore:ipHCap:ipRange, data = dat)

## Linear Mixed Effects Models
## Mixed effect categorised the parameters by group... 
##   similar with current score during living betting modelling, 
##   the scoring rate (intensity of scores) during 0-0 is different with scoring 
##   rates during 1-0 etc.
lm13 <<- lm(Return ~ Stakes + (1|HCap), data = dat)
lm14 <<- lm(Return ~ HCap + (1|Stakes), data = dat) # the stakes amount placed by 
##   firm A must be based on the degree of the edges to punter. Here I try to 
##   test the effect. (Although firm A might bet via several agents, here I can 
##   only took available sample from population to test the efficiency.)

lms <<- list(lm0 = lm0, lm1 = lm1, lm2 = lm2, lm3 = lm3, lm4 = lm4, lm5 = lm5, 
            lm6 = lm6, lm7 = lm7, lm8 = lm8, lm9 = lm9, lm10 = lm10, 
            lm11 = lm11, lm12 = lm12, lm13 = lm13, lm14 = lm14)

compare <<- ldply(lms, function(x) {
  y <- summary(x)$fstatistic
  df <- data.frame(AIC = AIC(x), BIC = BIC(x), t(summary(x)$df), 'p.value' = pf(y[1], y[2], y[3], lower.tail = FALSE))
  names(df) <- c('AIC', 'BIC', 'df', 'residuals', 'df', 'p.value'); df}) %>% tbl_df

## ========= ShinyApp ================================
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  ## this is your web page header information
  tags$head(
    ## here you include your inline styles
    tags$style(HTML("
                    body {
                    Text color: yellow;
                    background-color: darkgoldenrod;
                    }
					"))),
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(id = 'loading-content', h2('Loading...')),
  
  # The main app code goes here
  hidden(
    div(id = 'app-content',
      titlePanel('Summary of linear models to test the efficiency of staking model of firm A.'), 
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          div(id = 'models',
              h2('Select variables below for your linear model'),
              a(id = 'toggleModels', 'Select variables for your model', href = '#'),
              hidden(
                div(id = 'yourmodel',
                    selectInput('response', label = h4('What is the column name of your response variable Y?'), choices = names(dat)),
                    checkboxGroupInput('explan1', label = h4('What is the column(s) name of your explanatory variable X?'), choices = names(dat)),
                    actionButton('analysis', 'Analyze!'))),
              br(),
              a(id = 'toggleAdvanced', 'Show/hide advanced info', href = '#'),
              hidden(
                div(id = "advanced",
                    p('- Author Profile:', HTML("<a href='https://beta.rstudioconnect.com/englianhu/ryo-eng/'>®γσ, Eng Lian Hu</a>")),
                    p('- GitHub:', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation'>Source Code</a>")))),
              br(),
              p('Timestamp: ',
                span(id = 'time', date()),
                a(id = 'update', 'Update', href = '#')),
              actionButton('reset', 'Reset form'))),
        
        mainPanel(
          tabsetPanel(
            tabPanel('1 x 2 Combo to AH (by William Chen 2006)',
                     fluidRow(
                       column(3, numericInput('num1', label = 'Home Win', value = 2.5)),
                       column(4, numericInput('num2', label = 'Draw', value = 3.20)),
                       column(5, numericInput('num3', label = 'Away Win', value = 2.5))),
                     fluidRow(
                       column(3, ''),
                       column(4, numericInput('spread', label = 'Spread / Vigorish / Overround', value = 1.12))),
                     hr(),
                     fluidRow(
                       column(3, ''),
                       column(4, verbatimTextOutput('txt'))),
                     fluidRow(
                       column(3, tableOutput('tab1')),
                       column(4, ''),
                       column(5, tableOutput('tab2'))),
                     fluidRow(
                       column(3, ''),
                       column(4, tableOutput('tab3'))),
                     fluidRow(
                       column(3, tableOutput('tab4')),
                       column(4, ''),
                       column(5, tableOutput('tab5')))),
            tabPanel('Your model', verbatimTextOutput('modelSummary')),
            tabPanel('Model 01', verbatimTextOutput('model0')),
            tabPanel('Model 02', verbatimTextOutput('model1')),
            tabPanel('Model 03', verbatimTextOutput('model2')),
            tabPanel('Model 04', verbatimTextOutput('model3')),
            tabPanel('Model 05', verbatimTextOutput('model4')),
            tabPanel('Model 06', verbatimTextOutput('model5')),
            tabPanel('Model 07', verbatimTextOutput('model6')),
            tabPanel('Model 08', verbatimTextOutput('model7')),
            tabPanel('Model 09', verbatimTextOutput('model8')),
            tabPanel('Model 10', verbatimTextOutput('model9')),
            tabPanel('Model 11', verbatimTextOutput('model10')),
            tabPanel('Model 12', verbatimTextOutput('model11')),
            tabPanel('Model 13', verbatimTextOutput('model12')),
            tabPanel('Model 14', verbatimTextOutput('model13')),
            tabPanel('Model 15', verbatimTextOutput('model14')),
            tabPanel('Comparison', formattableOutput('table')),
            tabPanel('Reference', h4('Reference:'),
                     p('01. ', HTML("<a href='https://www.youtube.com/watch?v=66z_MRwtFJM'>Linear Regression in R (R Tutorial 5.1 to 5.11)</a>")),
                     p('02. ', HTML("<a href='http://www.r-bloggers.com/getting-started-with-mixed-effect-models-in-r/'>Getting Started with Mixed Effect Models in R</a>")),
                     p('03. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/A%20Very%20Basic%20Tutorial%20for%20Performing%20Linear%20Mixed%20Effects%20Analyses.pdf'>A very basic tutorial for performing linear mixed effects analyses</a>")),
                     p('04. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Linear%20Models%20with%20R.pdf'>Linear Models with R</a>")),
                     p('05. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Extending%20the%20Linear%20Model%20with%20R%20-%20Generalized%20Linear%2C%20Mixed%20Effects%20and%20Nonparametric%20Regression%20Models.pdf'>Extending the Linear Model with R : Generalized Linear, Mixed Effects and Nonparametric Regression Models</a>")),
                     p('06. ', HTML("<a href='http://www.ats.ucla.edu/stat/mult_pkg/whatstat/'>What statistical analysis should I use?</a>"),
                       tags$a(href='https://beta.rstudioconnect.com/englianhu/ryo-eng', target='_blank', 
                              tags$img(height = '20px', alt='hot', #align='right', 
                                       src='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/figure/hot.jpg?raw=true'))),
                     p('07. ', HTML("<a href='http://r4ds.had.co.nz/many-models.html'>Linear Models with R</a>")),
                     p('08. ', HTML("<a href='http://biostat.mc.vanderbilt.edu/wiki/Main/RmS'>REGRESSION MODELING STRATEGIES with Applications to Linear Models, Logistic and Ordinal Regression, and Survival Analysis</a>")),
                     p('09. ', HTML("<a href='https://www.zoology.ubc.ca/~schluter/R/fit-model/'>Fit models to data</a>"),
                       tags$a(href='https://beta.rstudioconnect.com/englianhu/ryo-eng', target='_blank', 
                              tags$img(height = '20px', alt='hot', #align='right', 
                                       src='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/figure/hot.jpg?raw=true'))),
                     p('10. ', HTML("<a href='http://stats.stackexchange.com/questions/172782/how-to-use-r-anova-results-to-select-best-model'>How to use R anova() results to select best model?</a>")),
                     p('11. ', HTML("<a href='http://blog.minitab.com/blog/adventures-in-statistics/how-to-choose-the-best-regression-model'>How to Choose the Best Regression Model</a>")),
                     p('12. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/ANOVA%20-%20Model%20Selection.pdf'>ANOVA - Model Selection</a>")),
                     p('13. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Model%20Selection%20in%20R.pdf'>Model Selection in R</a>")))),
          p("Powered by - Copyright® Intellectual Property Rights of ",
            tags$a(href='http://www.scibrokes.com', target='_blank',
                   tags$img(height = '20px', alt='hot', #align='right',
                            src='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/figure/oda-army.jpg?raw=true')), HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"), "個人の経営企業")
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = 'loading-content', anim = TRUE, animType = 'fade')    
  show('app-content')
  
  onclick('toggleModels', shinyjs::toggle(id = 'yourmodel', anim = TRUE))
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE))    
  onclick('update', shinyjs::html('time', date()))
  
  observeEvent(input$analysis, {
    y = ifelse(is.null(input$response)||is.na(input$response)||length(input$response) == 0, 'Return', input$response)
    x = ifelse(is.null(input$explan1)||is.na(input$explan1)||length(input$explan1) == 0, 'Stakes', input$explan1)
    
    model = lm(as.formula(paste0(y, '~', paste(x, collapse = ' + '))), data = dat)
    output$modelSummary = renderPrint({
      list(Summary = summary(model), Anova = anova(model))
    })
  })
  
  output$model0 <- renderPrint(list(Summary = summary(lm0), Anova = anova(lm0, lm1, lm2, lm3, lm4)))
  output$model1 <- renderPrint(list(Summary = summary(lm1), Anova = anova(lm0, lm1, lm2, lm3, lm4)))
  output$model2 <- renderPrint(list(Summary = summary(lm2), Anova = anova(lm0, lm1, lm2, lm3, lm4)))
  output$model3 <- renderPrint(list(Summary = summary(lm3), Anova = anova(lm0, lm1, lm2, lm3, lm4)))
  output$model4 <- renderPrint(list(Summary = summary(lm4), Anova = anova(lm0, lm1, lm2, lm3, lm4)))
  output$model5 <- renderPrint(list(Summary = summary(lm5), Anova = anova(lm5)))
  output$model6 <- renderPrint(list(Summary = summary(lm6), Anova = anova(lm6)))
  output$model7 <- renderPrint(list(Summary = summary(lm7), Anova = anova(lm7)))
  output$model8 <- renderPrint(list(Summary = summary(lm8), Anova = anova(lm8)))
  output$model9 <- renderPrint(list(Summary = summary(lm9), Anova = anova(lm9)))
  output$model10 <- renderPrint(list(Summary = summary(lm10), Anova = anova(lm10)))
  output$model11 <- renderPrint(list(Summary = summary(lm11), Anova = anova(lm11)))
  output$model12 <- renderPrint(list(Summary = summary(lm12), Anova = anova(lm12)))
  output$model13 <- renderPrint(list(Summary = summary(lm13), Anova = anova(lm13)))
  output$model14 <- renderPrint(list(Summary = summary(lm14), Anova = anova(lm14)))
  
  output$table <- renderFormattable({
    compare %>% formattable(list(
      AIC = formatter('span', style = x ~ style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      BIC = formatter('span', style = x ~ style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      df = formatter('span', style = x ~ style(color = ifelse(rank(-x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(-x))),
      
      residuals = formatter('span', style = x ~ style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(x))),
      
      p.value = formatter('span', style = x ~ style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %02d)', x, rank(x)))
    ))
  })
  
  output$txt <- renderText({
    fOdds <- c(input$num1, input$num2, input$num3) / input$spread
    c(paste0('Home Win = ', round(fOdds[1], 3)), ';', paste0('Draw = ', round(fOdds[2], 3)), ';', paste0('Away Win = ', round(fOdds[3], 3)))
  })
  
  output$tab1 <- renderTable({
    fOdds <- c(input$num1, input$num2, input$num3) / input$spread
    data_frame(Team = c('Home', 'Away'), Hdp = c('0.5', ''), Price = c(fOdds[1] - 1, ((fOdds[2] * fOdds[3]) / (fOdds[2] + fOdds[3])) - 1))
  })
  
  output$tab2 <- renderTable({
    fOdds <- c(input$num1, input$num2, input$num3) / input$spread
    data_frame(Team = c('Home', 'Away'), Hdp = c('', '0.5'), Price = c(((fOdds[1] * fOdds[2]) / (fOdds[1] + fOdds[2])) - 1, fOdds[3] - 1))
  })
  
  output$tab3 <- renderTable({
    fOdds <- c(input$num1, input$num2, input$num3) / input$spread
    data_frame(Team = c('Home', 'Away'), Hdp = c('0', '0'), Price = c(((fOdds[1] * fOdds[2]) - fOdds[1] - fOdds[2]) / fOdds[2], ((fOdds[2] * fOdds[3]) - fOdds[2] - fOdds[3]) / fOdds[2]))
  })
  
  output$tab4 <- renderTable({
    fOdds <- c(input$num1, input$num2, input$num3) / input$spread
    data_frame(Team = c('Home', 'Away'), Hdp = c('0/0.5', ''), Price = c(((2 * fOdds[1] * fOdds[2]) - fOdds[1] - (2 * fOdds[2])) / (2 * fOdds[2]), ((2 * fOdds[2] * fOdds[3]) - (2 * fOdds[2]) - (2 * fOdds[3])) / ((2 * fOdds[2]) + fOdds[3])))
  })
  
  output$tab5 <- renderTable({
    fOdds <- c(input$num1, input$num2, input$num3) / input$spread
    data_frame(Team = c('Home', 'Away'), Hdp = c('', '0/0.5'), Price = c(((2 * fOdds[1] * fOdds[2]) - (2 * fOdds[1]) - (2 * fOdds[2])) / ((2 * fOdds[2]) + fOdds[1]), ((2 * fOdds[2] * fOdds[3]) - fOdds[3] - (fOdds[2])) / (2 * fOdds[2])))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

