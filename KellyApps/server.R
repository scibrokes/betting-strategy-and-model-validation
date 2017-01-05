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
  hide(id = 'loading-content', anim = TRUE, animType = 'fade')    
  show('app-content')
  
  #'@ onclick('toggleModels', shinyjs::toggle(id = 'selectSIFund', anim = TRUE))
  #'@ onclick('toggleAdvanced', shinyjs::toggle(id = 'adjuster', anim = TRUE))    
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE))    
  onclick('update', shinyjs::html('time', date()))
  
  #'@ observeEvent(input$analysis, {
  #'@   y = ifelse(is.null(input$response)||is.na(input$response)||length(input$response) == 0, 'Return', input$response)
  #'@   x = ifelse(is.null(input$explan1)||is.na(input$explan1)||length(input$explan1) == 0, 'Stakes', input$explan1)
  #'@   
  #'@   model = vKelly()
  #'@   output$modelSummary = renderPrint({
  #'@     list(Summary = summary(model), Anova = anova(model))
  #'@   })
  #'@ })
  
  loadpage <- reactive({ 
    validate(
      need(input$page, "Web page input is null!!")
    )
    disppage <- pages[which(pages == input$page)]
    paste0(disppage)
  })
  
  output$displaypage <- renderUI({
    tags$iframe(src = loadpage(), height = 800, width = 600)
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
    ldply(BRSummary$KM, ldply, as.matrix) %>% datatable(
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
  
  #'@ output$txt <- renderText({
  #'@   fOdds <- c(input$num1, input$num2, input$num3) / input$spread
  #'@   c(paste0('Home Win = ', round(fOdds[1], 3)), ';', paste0('Draw = ', round(fOdds[2], 3)), ';', paste0('Away Win = ', round(fOdds[3], 3)))
  #'@ })
  
})