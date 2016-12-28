## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
suppressMessages(library('utils'))
suppressMessages(library('shiny'))
suppressMessages(library('shinyjs'))
suppressMessages(library('shinyBS'))
suppressMessages(library('shinythemes'))
suppressMessages(library('plyr'))
suppressMessages(library('dplyr'))
suppressMessages(library('magrittr'))
suppressMessages(library('formattable'))
suppressMessages(library('stringr'))
suppressMessages(library('highcharter'))

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
background: url(www/loader.gif) center no-repeat #fff;
}
"

## ========= ShinyUI ================================
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
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
  extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse") }'),
  
  # Loading message
  div(id = 'loading-content', h2('Loading...')),
  
  # The main app code goes here
  hidden(
    div(id = 'app-content',
        titlePanel('Summary of Kelly models to test the efficiency of staking model of firm A.'), 
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(
            bsCollapse(id = 'adjuster',
                h2('Comparison of Kelly Models'),
                sliderInput('n', 'EM Edge Adjuster:', min = 1, max = 10, value = 1.10, step = 0.0001),
                sliderInput('n', 'Stakes Weight Adjuster:', min = 1, max = 5, value = 1, step = 0.0001),
                sliderInput('n', 'Weight Adjuster (EXP):', min = 0, max = 5, value = 1, step = 0.0001),
                sliderInput('n', 'Maximum Iteration Adjuster:', min = 1, max = 1000, value = 1, step = 1),
                actionButton('tabBut', 'View Table'), style = 'primary'),
            br(),
            bsCollapse(id = 'chart',
              selectInput('type', label = 'Type', width = '100%',
                          choices = c('line', 'column', 'bar', 'spline'), 
                          selected = 'bar'), 
              selectInput('stacked', label = 'Stacked',  width = '100%',
                          choices = c(FALSE, 'normal', 'percent'), 
                          selected = 'percent'),
              selectInput('hc_theme', label = 'Theme',  width = '100%',
                          choices = c('538' = 'hc_theme_538()', 'economist' = 'hc_theme_economist()',
                                      'darkunica' = 'hc_theme_darkunica()', 
                                      'gridlight' = 'hc_theme_gridlight()', 
                                      'sandsignika' = 'hc_theme_sandsignika()',
                                      'null' = 'hc_theme_null()', 'handdrwran' = 'hc_theme_handdrawn()', 
                                      'chalk' = 'hc_theme_chalk'), 
                          selected = 'hc_theme_economist()'), style = 'primary'),
            br(),
            bsCollapse(id = 'advanced',
                       p('- Author Profile:', HTML("<a href='https://beta.rstudioconnect.com/englianhu/ryo-eng/'>RYO, ENG Lian Hu</a>")),
                       p('- GitHub:', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation'>Source Code</a>")),
                       br(),
                       p('Timestamp: ',
                         span(id = 'time', base::date()),
                         a(id = 'update', 'Update', href = '#')),
                       actionButton('reset', 'Reset form')), style = 'info'),
          
          mainPanel(
            tabsetPanel(
              tabPanel('Introduction',
                       tabsetPanel(
                         tabPanel('John Kelly (1956)',
                                  h1('John Larry Kelly (1956) - Kelly criterion'), 
                                  p('In probability theory and intertemporal portfolio choice, the ', 
                                    strong('Kelly criterion, Kelly strategy, Kelly formula'), ', or', 
                                    strong('Kelly bet'), ', is a formula used to determine the optimal 
                                     size of a series of bets. In most gambling scenarios, and some 
                                     investing scenarios under some simplifying assumptions, the Kelly 
strategy will do better than any essentially different strategy in the long run (that is, over a span 
of time in which the observed fraction of bets that are 
                       successful equals the probability that any given bet will be successful). It was described by J. L. Kelly, Jr, a 
                       researcher at Bell Labs, in 1956. The practical use of the formula has been demonstrated.'), 
                                   p('You are feel free to read the source of article through', 
HTML("<a href='https://en.wikipedia.org/wiki/Kelly_criterion'>Kelly Criterion</a>"), '. Below is the research paper wrote by John Kelly.'), 
HTML('<iframe src=\"https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/ffaca0fc85f64c47f5bfa164e9acbdf707a78e8a/references/A%20New%20Interpretation%20of%20Information%20Rate.pdf" width=\"900\" height=\"600\"></iframe>'),
              imageOutput('imp_pdf', width='500px', height='500px'),
              HTML("<a href='http://vnijs.github.io/radiant/'>Radiant is a platform-independent browser-based interface for business analytics in R, based on the Shiny package</a>")),
                         tabPanel('Niko Marttinen (2001)'),
                         tabPanel('Fabián Enrique Moya (2012)'))),
              tabPanel('Kelly and Fractional Kelly', 
                       tabsetPanel(
                         tabPanel('0. lProf', h1('0. Risk Profile'), 
                                  plot(dist(dat$Stakes)), 
                                  table(lRiskProf)),
                         tabPanel('1. K1', h1('1. Reversed Stakes Models')),
                         tabPanel('2. K2', h1('2. Reversed EM Models'))
                       )),
              tabPanel('Weighted Kelly', 
                       tabsetPanel(
                         tabPanel('3. K1W1', h1('Reversed Stakes Models (Weight 1)')),
                         tabPanel('4. K1W2', h1('Reversed Stakes Models (Weight 2)')),
                         tabPanel('5. K2W1', h1('Reversed EM Models (Weight 1)')),
                         tabPanel('6. K2W2', h1('Reversed EM Models (Weight 2)')),
                         tabPanel('7. K1W1WS1', h1('Reversed Stakes Models (Weight 1 Weight Stakes 1)')),
                         tabPanel('8. K1W1WS2', h1('Reversed Stakes Models (Weight 1 Weight Stakes 2)')),
                         tabPanel('9. K1W2WS1', h1('Reversed Stakes Models (Weight 2 Weight Stakes 1)')),
                         tabPanel('10. K1W2WS2', h1('Reversed Stakes Models (Weight 2 Weight Stakes 2)')),
                         tabPanel('11. K2W1WS1', h1('Reversed EM Models (Weight 1 Weight Stakes 1)')),
                         tabPanel('12. K2W1WS2', h1('Reversed EM Models (Weight 1 Weight Stakes 2)')),
                         tabPanel('13. K2W2WS1', h1('Reversed EM Models (Weight 2 Weight Stakes 1)')),
                         tabPanel('14. K2W2WS2', h1('Reversed EM Models (Weight 2 Weight Stakes 2)'))
                       )),
              tabPanel('Dynamic Kelly',
                       tabsetPanel(
                         tabPanel('15. K1D1', h1('Reversed Stakes Models (Dynamic Weight 1)')),
                         tabPanel('16. K1D2', h1('Reversed Stakes Models (Dynamic Weight 2)')),
                         tabPanel('17. K2D1', h1('Reversed EM Models (Dynamic Weight 1)')),
                         tabPanel('18. K2D2', h1('Reversed EM Models (Dynamic Weight 2)')),
                         tabPanel('19. K1D1WS1', h1('Reversed Stakes Models (Dynamic Weight 1 Weight Stakes 1)')),
                         tabPanel('20. K1D1WS2', h1('Reversed Stakes Models (Dynamic Weight 1 Weight Stakes 2)')),
                         tabPanel('21. K1D2WS1', h1('Reversed Stakes Models (Dynamic Weight 2 Weight Stakes 1)')),
                         tabPanel('22. K1D2WS2', h1('Reversed Stakes Models (Dynamic Weight 2 Weight Stakes 2)')),
                         tabPanel('23. K2D1WS1', h1('Reversed EM Models (Dynamic Weight 1 Weight Stakes 1)')),
                         tabPanel('24. K2D1WS2', h1('Reversed EM Models (Dynamic Weight 1 Weight Stakes 2)')),
                         tabPanel('25. K2D2WS1', h1('Reversed EM Models (Dynamic Weight 2 Weight Stakes 1)')),
                         tabPanel('26. K2D2WS2', h1('Reversed EM Models (Dynamic Weight 2 Weight Stakes 2)')),
                         tabPanel('27. K1W1DWS1', h1('Reversed Stakes Models (Weight 1 Dynamic Weight Stakes 1)')),
                         tabPanel('28. K1W1DWS2', h1('Reversed Stakes Models (Weight 1 Dynamic Weight Stakes 2)')),
                         tabPanel('29. K1W2DWS1', h1('Reversed Stakes Models (Weight 2 Dynamic Weight Stakes 1)')),
                         tabPanel('30. K1W2DWS2', h1('Reversed Stakes Models (Weight 2 Dynamic Weight Stakes 2)')),
                         tabPanel('31. K2W1DWS1', h1('Reversed EM Models (Weight 1 Dynamic Weight Stakes 1)')),
                         tabPanel('32. K2W1DWS2', h1('Reversed EM Models (Weight 1 Dynamic Weight Stakes 2)')),
                         tabPanel('33. K2W2DWS1', h1('Reversed EM Models (Weight 2 Dynamic Weight Stakes 1)')),
                         tabPanel('34. K2W2DWS2', h1('Reversed EM Models (Weight 2 Dynamic Weight Stakes 2)')),
                         tabPanel('35. K1D1DWS1', h1('Reversed Stakes Models (Dynamic Weight 1 Dynamic Weight Stakes 1)')),
                         tabPanel('36. K1D1DWS2', h1('Reversed Stakes Models (Dynamic Weight 1 Dynamic Weight Stakes 2)')),
                         tabPanel('37. K1D2DWS1', h1('Reversed Stakes Models (Dynamic Weight 2 Dynamic Weight Stakes 1)')),
                         tabPanel('38. K1D2DWS2', h1('Reversed Stakes Models (Dynamic Weight 2 Dynamic Weight Stakes 2)')),
                         tabPanel('39. K2D1DWS1', h1('Reversed EM Models (Dynamic Weight 1 Dynamic Weight Stakes 1)')),
                         tabPanel('40. K2D1DWS2', h1('Reversed EM Models (Dynamic Weight 1 Dynamic Weight Stakes 2)')),
                         tabPanel('41. K2D2DWS1', h1('Reversed EM Models (Dynamic Weight 2 Dynamic Weight Stakes 1)')),
                         tabPanel('42. K2D2DWS2', h1('Reversed EM Models (Dynamic Weight 2 Dynamic Weight Stakes 2)'))
                       )),
              tabPanel('Comparison'),
              tabPanel('Reference', h4('Reference:'),
                       p('01. ', HTML("<a href='https://egon.stats.ucl.ac.uk/projects/bmetaweb/'>bmetaweb</a>")),
                       p('02. ', HTML("<a href='https://egon.stats.ucl.ac.uk/projects/BCEAweb/'>BCEAweb</a>")),
                       p('03. ', HTML("<a href='https://github.com/scibrokes/kelly-criterion'>Application of Kelly Criterion model in Sportsbook Investment</a>")),
                       p('04. ', HTML("<a href='https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers'>Odds Modelling and Testing Inefficiency of Sports Bookmakers</a>")),
                       p('05. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/A%20New%20Interpretation%20of%20Information%20Rate.pdf'>A New Interpretation of Information Rate</a>")),
                       p('06. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Statistical%20Methodology%20for%20Profitable%20Sports%20Gambling.pdf'>Statistical Methodology for Profitable Sports Gambling</a>"),
                         tags$a(href='https://beta.rstudioconnect.com/englianhu/ryo-eng', target='_blank', 
                                tags$img(height = '20px', alt='hot', #align='right', 
                                         src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/hot.jpg'))),
                       p('07. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/An%20Introduction%20to%20Football%20Modelling%20at%20SmartOdds%20(v1).pdf'>An Introduction to Football Modelling at SmartOdds (v1)</a>")),
                       p('08. ', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Creating%20a%20Profitable%20Betting%20Strategy%20for%20Football%20by%20Using%20Statistical%20Modelling.pdf'>Creating a Profitable Betting Strategy for Football by Using Statistical Modelling</a>"),
                         tags$a(href='https://beta.rstudioconnect.com/englianhu/ryo-eng', target='_blank', 
                                tags$img(height = '20px', alt='hot', #align='right', 
                                         src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/hot.jpg'))))),
            p("Powered by - Copyright® Intellectual Property Rights of ",
              tags$a(href='http://www.scibrokes.com', target='_blank',
                     tags$img(height = '20px', alt='hot', #align='right',
                              src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))))))
    )
  )

