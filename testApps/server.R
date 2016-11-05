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

## ========= ShinyServer ================================
# Define server logic required to draw a histogramserver <- shinyServer(
  function(input, output, session){
    observe({
      if(input$selectall == 0){
        return(NULL)
      } else if(input$selectall%%2 == 0){
        updateCheckboxGroupInput(session, 'counter', 'Select counter(s)', choices = tickers)
      } else{
        updateCheckboxGroupInput(session, 'counter', 'Select counter(s)', choices = tickers, selected = tickers)
      }
      })
    
    ## http://stackoverflow.com/questions/29282524/obtain-the-values-from-a-checkboxgroup-only-once-in-shiny
    # store as a dataframe to be called wherever desired
    # evaluated whenever inputs change
    datasetInput <- reactive({
      ticked <- input$counter
    })
    
    # call as a text output
    output$textOut <- renderText({
      input$submit # makes sure nothing moves till the button is hit
      # isolate prevents datasetInput from reactively evaluating
      isolate(datasetInput())
    })
    
    # call in a different place as a data table output
    output$table1 <- renderDataTable({
      input$submit
      isolate(dt <- data.frame(inputs = datasetInput()))
      datatable(dt, caption="Table 1.1.1 : Public Listed Companies Stock Price", 
                extensions = list("ColReorder"=NULL, "ColVis"=NULL, "TableTools"=NULL
                                  #, "FixedColumns"=list(leftColumns=2)
                                  ), 
                options = list(autoWidth=TRUE, oColReorder=list(realtime=TRUE), 
                               #oColVis=list(exclude=c(0, 1), activate='mouseover'), 
                               oTableTools=list(
                                 sSwfPath="//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf", 
                                 aButtons=list("copy", "print", 
                                               list(sExtends="collection", sButtonText="Save", 
                                                    aButtons=c("csv","xls")))), 
                               dom='CRTrilftp', scrollX=TRUE, scrollCollapse=TRUE, 
                               colVis=list(exclude=c(0), activate='mouseover')))
      })
    
    output$gvis <- renderGvis({
      input$submit
      isolate(dt <- data.frame(inputs = datasetInput()))
    if(input$adjust) dt <- adjust(dt)   #'@ smp <- adjust(subset(data, Com %in% input$counter &
                                            #'@ Date>=input$dates[1] & Date<=input$dates[2]))
    if(input$log) dt <- mutate(Low=log(Low), Open=log(Open), Close=log(Close), High=log(High))
    gvisCandlestickChart(dt, xvar='Date', low='Low', open='Open', close='Close', high='High', 
                         options=list(legend='none', gvis.editor="Edit me!"))
  })
  
  output$table <- renderTable({
    input$submit
    isolate(dt <- data.frame(inputs = datasetInput()))
  })
  
  output$downloadData <- downloadHandler(
    dt <- data.frame(inputs = datasetInput()),
    filename = function() { 
      paste(dt, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dt, file)
    })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
      )
    })
  })
