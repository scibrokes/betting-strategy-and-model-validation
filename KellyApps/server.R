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
suppressMessages(library('PerformanceAnalytics'))
suppressMessages(library('quantmod'))
suppressMessages(library('formattable'))
suppressMessages(library('stringr'))

## ========= ShinyServer ================================
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = 'loading-content', anim = TRUE, animType = 'fade')   
  shinyjs::show(id = 'app-content', anim = TRUE, animType = 'fade')
  #'@ loadData()
  
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE, animType = 'fade'))    
  onclick('update', shinyjs::html('time', date()))
  
  #'@ loadpage <- reactive({ 
  #'@   validate(
  #'@     need(input$page, 'Web page input is null!!')
  #'@   )
  #'@   disppage <- pages[which(pages == input$page)]
  #'@   paste0(disppage)
  #'@ })
  
  #'@ output$displaypage <- renderUI({
  #'@   tags$iframe(src = loadpage(), height = 800, width = 500)
  #'@ })
  
  observe({ 
    query <- pages[which(pages == input$page)]
    disppage <<- pages[which(pages == input$page)]
  })
  
  output$displaypage <- renderUI({
    input$page
    dpage <- tags$iframe(src = disppage, height = 800, width = 500)
    dpage
  })
  
  ## Define a reactive expression for the document term matrix
  terms <- reactive({
    ## Change when the "update" button is pressed...
    input$funds
    ## ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing graph...")
        selectFund(input$funds)
      })
    })
  })
  
  output$hcontainer <- renderHighchart({
    fund <- terms()$sfund
    plotChart2(fund, type = 'single', chart.type2 = input$type, 
               chart.theme = input$hc_theme, stacked = input$stacked)
    
  })
  
  output$distTable <- renderDataTable({
    fundDT <- terms()$sfundDT
    fundDT %>% datatable(
      caption = "Table 2.1.1 : Firm A Staking Data (in $0,000)", 
      escape = FALSE, filter = "top", rownames = FALSE, 
      extensions = list("ColReorder" = NULL, "RowReorder" = NULL, 
                        "Buttons" = NULL, "Responsive" = NULL), 
      options = list(dom = 'BRrltpi', autoWidth = TRUE, scrollX = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
  })#, options = list(pageLength = 10))
  
  
  output$BRSM <- renderDataTable({
    BRSum %>% datatable(
      caption = "Table 4.3.5.2 : Summary of Kelly Main Funds ('0,000)", 
      escape = FALSE, filter = 'top', rownames = FALSE, 
      extensions = list('ColReorder' = NULL, 'RowReorder' = NULL, 
                        'Buttons' = NULL, 'Responsive' = NULL), 
      options = list(dom = 'BRrltpi', autoWidth = TRUE,  scrollX = TRUE, 
                     lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                     ColReorder = TRUE, rowReorder = TRUE, 
                     buttons = list('copy', 'print', 
                                    list(extend = 'collection', 
                                         buttons = c('csv', 'excel', 'pdf'), 
                                         text = 'Download'), I('colvis'))))
    })
  
  #http://stackoverflow.com/questions/25920548/shiny-select-go-to-different-tabpanel-using-action-button-or-something/25921150
  
  ## -------------------- render plot -----------------------------------------------
  #'@ output$table1A <- renderFormattable({
  #'@   suppressMessages(library('magrittr'))
  #'@   compare[[1]] %>% formattable(list(
  #'@     AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     df = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(-x))),
  #'@     
  #'@     residuals = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(x))),
  #'@     
  #'@     p.value = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %02d)', x, rank(x))),
  #'@     
  #'@     delta.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     Loglik.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     weight.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     delta.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     Loglik.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
  #'@     
  #'@     weight.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x)))
  #'@     
  #'@   ))
  #'@ })
  
  
})