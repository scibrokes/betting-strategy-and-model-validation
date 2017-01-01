suppressPackageStartupMessages(library("BBmisc"))
suppressAll(library('plyr'))
suppressAll(library('stringr'))
suppressAll(library('tidyverse'))
suppressAll(library("shiny"))
suppressAll(library('shinyBS'))
suppressAll(library('highcharter'))
suppressAll(library('DT'))
suppressAll(source("./function/compareKelly.R"))
suppressAll(source("./function/plotChart2.R"))

SOFund <<- read_rds(path = './SOFund.rds')
dateRange <<- data.frame(SOFund) %>% row.names %>% as.Date
SOFundDT <<- data.frame(DateUS = row.names(data.frame(SOFund)), SOFund) %>% tbl_df %>% 
  mutate(DateUS = as.Date(DateUS), Ordinary.Open = currency(Ordinary.Open), 
         Ordinary.High = currency(Ordinary.High), Ordinary.Low = currency(Ordinary.Low), 
         Ordinary.Close = currency(Ordinary.Close), 
         Ordinary.Volume = digits(Ordinary.Volume, 4, format = 'd', big.mark=','), 
         Ordinary.Adjusted = digits(Ordinary.Adjusted, 4, format = 'd', big.mark=','))

ui <- fluidPage(
  h1("Highcharter Demo"),
  sidebarLayout(
    sidebarPanel(
           selectInput("type", label = "Type", width = "100%",
                       choices = c(FALSE, "line", "column", "spline", "bar", "pie"), 
                       selected = "line"), 
           selectInput("stacked", label = "Stacked",  width = "100%",
                       choices = c(FALSE, "normal", "percent"), 
                       selected = "percent"),
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
                       selected = "hc_theme_economist()"), 

           actionButton("tabBut", "View Table")),
    
    mainPanel(
      highchartOutput("hcontainer", height = "500px"),
      bsModal("modalExample", "Data Table", "tabBut", size = "large",
              dataTableOutput("distTable")))))

server = function(input, output) {
  
  output$hcontainer <- renderHighchart({
    
    plotChart2(SOFund, type = 'single', chart.type2 = input$type, 
               chart.theme = input$hc_theme, stacked = input$stacked)
    
  })
  
  output$distTable <- renderDataTable({
    
    SOFundDT %>% datatable(
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
  }

shinyApp(ui = ui, server = server)


