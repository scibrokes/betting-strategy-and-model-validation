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
        titlePanel('Investment Fund Management'), 
        navbarPage('My Application', 
                   tabPanel('Betting Strategy and Model Validation', 
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                              sidebarPanel(
                                bsCollapse(id = 'selectSIFund', open = 'Select Fund', 
                                           bsCollapsePanel('Select Fund', 
                                                           selectInput("funds", label = "Fund", width = "100%", 
                                                                       choices = fundsopt, selected = "SOFund"), 
                                                           actionButton('tabBut', 'View Table'), style = 'primary'),
                                           #br(),
                                           bsCollapsePanel('Chart Option', 
                                                           selectInput("type", label = "Type", width = "100%", 
                                                                       choices = c(FALSE, "line", "column", "spline", "bar", "pie"), 
                                                                       selected = "line"), 
                                                           selectInput("stacked", label = "Stacked",  width = "100%", 
                                                                       choices = c(FALSE, "normal", "percent"), 
                                                                       selected = "normal"), 
                                                           selectInput("hc_theme", label = "Theme",  width = "100%", 
                                                                       choices = c("theme" = "hc_theme()", "538" = "hc_theme_538()", 
                                                                                   "chalk" = "hc_theme_chalk()", 
                                                                                   "darkunica" = "hc_theme_darkunica()", 
                                                                                   "db" = "hc_theme_db()", 
                                                                                   "economist" = "hc_theme_economist()", 
                                                                                   "flat" = "hc_theme_flat()", 
                                                                                   "flatdark" = "hc_theme_flatdark()", 
                                                                                   "ft" = "hc_theme_ft()", 
                                                                                   "google" = "hc_theme_google()", 
                                                                                   "gridlight" = "hc_theme_gridlight()", 
                                                                                   "handdrwran" = "hc_theme_handdrawn()", 
                                                                                   "merge" = "hc_theme_merge()", 
                                                                                   "null" = "hc_theme_null()", 
                                                                                   "sandsignika" = "hc_theme_sandsignika()", 
                                                                                   "smpl" = "hc_theme_smpl()", 
                                                                                   "sparkline" = "hc_theme_sparkline()"), 
                                                                       selected = "hc_theme_economist()"), style = 'primary')),
                                br(),
                                a(id = 'toggleAdvanced', 'Show/hide advanced info', href = '#'),
                                hidden(
                                  div(id = 'advanced', 
                                      p('- Author Profile:', HTML("<a href='https://beta.rstudioconnect.com/englianhu/ryo-eng/'>RYO, ENG Lian Hu</a>")),
                                      p('- GitHub:', HTML("<a href='https://github.com/scibrokes/betting-strategy-and-model-validation'>Source Code</a>")),
                                      br(),
                                      p('Timestamp: ', 
                                        span(id = 'time', base::date()), 
                                        a(id = 'update', 'Update', href = '#')), 
                                      actionButton('reset', 'Reset form'), style = 'info'))),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Introduction',
                                           tabsetPanel(
                                             tabPanel('Article', 
                                                      h1('Betting Strategy and Model Validation'), 
                                                      #http://stackoverflow.com/questions/33020558/embed-iframe-inside-shiny-app
                                                      #http://stackoverflow.com/questions/25849968/shiny-iframe-depending-on-input
                                                      selectInput("page", label = h5("Choose an article"), 
                                                                  choices = c('Natural Language Analysis', 'Part I', 'Part II', 'regressionApps'), selected = 'Part II'),
                                                      htmlOutput('displaypage')), 
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
                                             tabPanel('Niko Marttinen (2001)', 
                                                      h1('Niko Marttinen (2001)'), 
                                                      p('This is a Master Degree thesis by the author. He apply few steps to test the odds modelling and application of Kelly model. The thesis is similar with my previous research ', 
                                                        HTML("<a href='https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers'>Odds Modelling and Testing Inefficiency of Sports Bookmakers</a>"), 
                                                        ' and ', em('Dixon and Coles (1996)'), ' but more sophiscated : '), 
                                                      p('- Firstly, using few statistical algorithm calculation for odds modelling.'), 
                                                      p('- Secondly, using Kelly model for portfolio management.'), 
                                                      p('- Lastly, using Monte Carlo Markov Chain (MCMC) to test the optimal staking portfolio/model.'), 
                                                      p(em('4.4.1 Niko Marttinen (2001)'), ' in my research paper has roughly talk about it and my later research will enhanced as well. You are feel free to read the source of article below.'), 
                                                      HTML('<iframe src=\"https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/8adcccbde5140a4321bf064ef2e065551bc195ed/references/Creating%20a%20Profitable%20Betting%20Strategy%20for%20Football%20by%20Using%20Statistical%20Modelling.pdf" width=\"900\" height=\"600\"></iframe>'),
                                                      imageOutput('imp_pdf', width='500px', height='500px'),
                                                      HTML("<a href='http://vnijs.github.io/radiant/'>Radiant is a platform-independent browser-based interface for business analytics in R, based on the Shiny package</a>")),
                                             tabPanel('Fabián Enrique Moya (2012)', 
                                                      h1('Fabián Enrique Moya (2012)'), 
                                                      p(), 
                                                      HTML('<iframe src=\"https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/c2da2e5ca09aaf218616045031c9ee4ce3537b18/references/Statistical%20Methodology%20for%20Profitable%20Sports%20Gambling.pdf" width=\"900\" height=\"600\"></iframe>'),
                                                      imageOutput('imp_pdf', width='500px', height='500px'),
                                                      HTML("<a href='http://vnijs.github.io/radiant/'>Radiant is a platform-independent browser-based interface for business analytics in R, based on the Shiny package</a>")
                                                      ))),
                                  tabPanel('Fund', 
                                           tabsetPanel(
                                             tabPanel('Fund Selection', h1('Fund Selection'), 
                                                      p('SOFund is the fund growth of British firm A and the rest are Kelly based investment fund without allocation of portfolio. You are feel free to refer to ', 
                                                        HTML("<a href='http://rstudio-pubs-static.s3.amazonaws.com/208636_454ebe20f3434859a15bb4196ca1503c.html#kelly-odel'>4.3 Kelly Ⓜodel</a>"), ' for more details.'), 
                                                      br(), 
                                                      highchartOutput("hcontainer", height = "500px"),
                                                      ## https://ebailey78.github.io/shinyBS/docs/Modals.html#components
                                                      bsModal("modalExample", "Data Table", "tabBut", size = "large",
                                                              dataTableOutput('distTable'))), 
                                             #table(lRiskProf)),
                                             tabPanel('Fund Comparison', h1('Fund Comparison'), 
                                                      p('Kindly refer to ', em('table 4.3.5.2') ,' in the research paper if it is slow to display.'),
                                                      dataTableOutput('BRSM')),
                                             tabPanel('League Risk Profile', h1('2. Reversed EM Models')))),
                                  tabPanel('Portfolio', 
                                           tabsetPanel(
                                             tabPanel('Fund Selection', h1('Fund Selection'), 
                                                      p('SOFund is the fund growth of British firm A and the rest are Kelly based investment fund with allocation of portfolio. You are feel feel to refer to ', 
                                                        HTML("<a href='http://rstudio-pubs-static.s3.amazonaws.com/208636_454ebe20f3434859a15bb4196ca1503c.html#staking-odel-and-oney-anagement'>4.5.1 Risk Management</a>"), ' for more details.'), 
                                                      br()#, 
                                                      #highchartOutput("hcontainer", height = "500px"),
                                                      ## https://ebailey78.github.io/shinyBS/docs/Modals.html#components
                                                      #bsModal("modalExample", "Data Table", "tabBut", size = "large",
                                                      #        dataTableOutput("distTable"))), 
                                             ), 
                                             #table(lRiskProf)),
                                             tabPanel('Fund Comparison', h1('1. Reversed Stakes Models')), 
                                             tabPanel('League Risk Profile', h1('2. Reversed EM Models')))), 
                                  tabPanel('Reference', 
                                           tabsetPanel(
                                             tabPanel('Reference', h4('Reference'), 
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
                                                                        src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/hot.jpg'))), 
                                                      p('09. ', HTML("<a href='https://github.com/R-Finance'>R-Finance</a>"))#,
                                                      ## p('10. ', HTML("<a href='https://mp.weixin.qq.com/s?__biz=MzA3NDUxMjYzMA%3D%3D&mid=216065319&idx=1&sn=31af52816c7e8b937f15480c4d5f6e41&key=0acd51d81cb052bcbc420864d8003491eba2f4bbc722bf3a7bc7da0d59fefc64ea6fc32bdb33673eebd62f201cbc2190&ascene=7&uin=MjAwMTM4MjU0OA%3D%3D&devicetype=android-19&version=26020236&nettype=WIFI&pass_ticket=GdViEIR%2F5PLzVFnzLxc71K39ze4fb6VAwvFp1bhH3inbu5xBjyQ7BLEpDOrQhWZ1'>R高性能包介绍与并行运算</a>")), 
                                                      ## p('11. ', HTML("<a href='http://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651054987&idx=1&sn=11c6bb68dbb0d77598a1d2459cff6dcf&chksm=84d9c21cb3ae4b0ade8f398760e6414be06c4e9cb69d1389df46e326fd481e3320c9ffb92319&scene=0#wechat_redirect'>R语言量化投资常用包总结</a>"))
                                             ), 
                                             tabPanel('Source Codes', h4('Source Codes'), 
                                                      tags$iframe(src='https://github.com/scibrokes', height = 500, width = 500, frameborder = 0, seamless = 'seamless'))
                                           )))))),
                   tabPanel('Portfolio Management', h4('Investment Fund Portfolio Management'), 
                            tags$iframe(src='https://github.com/scibrokes/kelly-criterion', height = 500, width = 500, frameborder = 0, seamless = 'seamless'))))),
        br(), 
        p('Powered by - Copyright® Intellectual Property Rights of ', 
          tags$a(href='http://www.scibrokes.com', target='_blank', 
                 tags$img(height = '20px', alt='hot', #align='right', 
                          src='https://raw.githubusercontent.com/scibrokes/betting-strategy-and-model-validation/master/regressionApps/oda-army.jpg')), 
          HTML("<a href='http://www.scibrokes.com'>Scibrokes®</a>"))))


