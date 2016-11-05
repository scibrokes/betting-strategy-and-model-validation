## ========= Setup Options =================================
## Setup Options, Loading Required Libraries and Preparing Environment
## Loading the packages and setting adjustment
suppressMessages(library('utils'))
suppressMessages(library('shiny'))
suppressMessages(library('shinyjs'))
suppressMessages(library('shinyBS'))
suppressMessages(library('shinythemes'))
suppressMessages(library('shinydashboard'))
suppressMessages(library('plyr'))
suppressMessages(library('dplyr'))
suppressMessages(library('magrittr'))
suppressMessages(library('formattable'))
suppressMessages(library('stringr'))

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

## ========= ShinyUI ================================
ui <- dashboardPage(
  dashboardHeader(title = 'Testing Kelly Models on British Sportsbook Consultancy Firm A',
                  dropdownMenuOutput('messageMenu'),
                  dropdownMenu(type = 'notifications',
                               notificationItem(text = '5 new users today', icon('users')),
                               notificationItem(text = '12 items delivered', 
                                                icon('truck'), status = 'success'),
                               notificationItem(text = 'Server load at 86%', 
                                                icon = icon('exclamation-triangle'), 
                                                status = 'warning')),
                  dropdownMenu(type = 'tasks',
                               badgeStatus = 'success',
                               taskItem(value = 90, color = 'green', 'Documentation'),
                               taskItem(value = 17, color = 'aqua', 'Project X'),
                               taskItem(value = 75, color = 'yellow', 'Server deployment'),
                               taskItem(value = 80, color = 'red', 'Overall project')),
                  tags$li(class = 'dropdown',
                          tags$a(href='http://www.scibrokes.com', target='_blank', 
                                 tags$img(height = '20px', alt='Scibrokes Logo', align='right', 
                                          src='https://avatars0.githubusercontent.com/u/13562894?v=3&s=200')
                          )
                    )
                  ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      menuItem('Widgets', tabName = 'widgets', icon = icon('th')),
      menuItem('The Economics of Soccer', tabName = 'soccer', icon = icon('bar-chart'),
               menuSubItem('Summary Charts', tabName='report1', icon = icon('bar-chart')),
               menuSubItem('Econometrics in Soccer Betting', tabName='econometrics', icon = icon('bar-chart'))),
      menuItem('Company Profile', tabName = 'profile', icon = icon('bar-chart'),
               menuSubItem('Annual Summary Charts', tabName='report2', icon = icon('bar-chart')),
               menuSubItem('Stock Market', tabName='stocks', icon = icon('line-chart')),
               menuSubItem('Financial Report', tabName='finance', icon = icon('bar-chart'))),
      menuItem('References', tabName = 'reference', icon = icon('file-pdf-o')),
      menuItem('Source code', icon = icon('file-code-o'), 
               href = 'https://github.com/scibrokes/analyse-the-finance-and-stocks-price-of-bookmakers'),
      sidebarSearchForm(textId = 'searchText', buttonId = 'searchButton', label = 'Search...')
    )),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = 'dashboard',
              h3(HTML("<a href='http://success.wp.shu.edu.tw/1050323%EF%BC%9A%E8%B2%A1%E7%B6%93%E9%96%8B
                      %E6%94%BE%E6%95%B8%E6%93%9A%E5%B9%B3%E5%8F%B0stock-ai%E5%92%8Cr%E5%88%86%E6%9E%90%
                      E4%BB%8B%E9%9D%A2/'>財經開放數據平台Stock AI和R分析介面</a>")),
              fluidRow(
                box(plotOutput("plot1")),
                box(
                  "Box content here", br(), "More box content",
                  sliderInput("slider", "Slider input:", 1, 100, 50),
                  textInput("text", "Text input:")
                ))),
      # 2nd tab content
      tabItem(tabName = 'widgets',
              h2('Widget content')),
      # 3rd tab content
      tabItem(tabName = 'soccer',
              h2('The Economics of Soccer')),
      # 4th tab content
      tabItem(tabName = 'report1',
              h2('Summary Charts')),
      # 5th tab content
      tabItem(tabName = 'econometrics',
              h2('Econometrics in Soccer Betting')),
      # 6th tab content
      tabItem(tabName = 'profile',
              h2('Company Profile')),
      # 7th tab content
      tabItem(tabName = 'report2',
              h2('Annual Summary Chart')),
      # 8th tab content
      tabItem(tabName = 'stocks',
              h2('Stock Market'),
              fluidPage(
                titlePanel('Stock Counter of Gaming Industry Public Listed Companies'),
                sidebarLayout(
                  sidebarPanel(
                    helpText('Select the stocks to examine. Information will be collected from Google/Yahoo finance.'),
                    checkboxGroupInput('counter', 'Select counter(s)', choices = tickers), 
                    actionLink('selectall', 'Select/Unselect All'),
                    br(),
                    br(),
                    dateRangeInput('dates', 'Date range', start = '2013-01-01', end = as.character(Sys.Date())),
                    checkboxInput('log', 'Plot y axis on log scale', value = FALSE),
                    checkboxInput('adjust', 'Adjust prices for inflation', value = FALSE),
                    br(),
                    actionButton('submit', 'Submit')),
                  mainPanel(
                    tabsetPanel( #tabBox(
                      tabPanel('Data', h4('Stock Price Table'), 
                               textOutput('textOut'), DT::dataTableOutput('table1'), 
                               downloadButton('downloadData', 'Download')),
                      tabPanel('Summary', htmlOutput('data')),
                      tabPanel('Candle-Stick', h4('Stock Price Graph'), htmlOutput('gvis')),
                      tabPanel('Line-chart', htmlOutput('gvis')),
                      tabPanel('Comp', helpText('test')))
                    )))),
      # 9th tab content
      tabItem(tabName = 'finance',
              h2('Financial Report'),
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              ),
              
              # infoBoxes with fill=TRUE
              fluidRow(
                infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                infoBoxOutput("progressBox2"),
                infoBoxOutput("approvalBox2")
              ),
              
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4, actionButton("count", "Increment progress"))
              )
      ),
      # 10th tab content
      tabItem(tabName = 'reference',
              h2('Reference:'),
              tabPanel("Help",
                       HTML('<iframe src=\"https://englianhu.files.wordpress.com/2016/03/financial-statements-a-step-by-step-guide-to-understanding-and-creating-financial-reports.pdf" width=\"900\" height=\"600\"></iframe>')),
              imageOutput('imp_pdf', width='500px', height='500px'),
              HTML("<a href='http://vnijs.github.io/radiant/'>Radiant is a platform-independent browser-based interface for business analytics in R, based on the Shiny package</a>")
              ))))


