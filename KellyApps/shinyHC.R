library("shiny")
library('shinyBS')
library('highcharter')
source("./function/compareKelly.R")
source("./function/plotChart2.R")

SOFund <<- read_rds(path = './SOFund.rds')
dateRange <<- data.frame(SOFund) %>% row.names %>% as.Date

ui <- fluidPage(
  h1("Highcharter Demo"),
  fluidRow(
    column(width = 4, class = "panel",
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
           dateRangeInput("dateRange", "Date Range:", 
                       start = range(dateRange)[1], 
                       end = range(dateRange)[2]),
           actionButton("tabBut", "View Table")),
    column(width = 8, highchartOutput("hcontainer", height = "500px"))))

server = function(input, output) {
  
  output$hcontainer <- renderHighchart({
    
    plotChart2(SOFund, type = 'single', chart.type2 = input$type, 
               chart.theme = input$hc_theme, stacked = input$stacked)
    
  })
  
  output$distTable <- renderDataTable({
    
    SOFund %>% datatable(
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
    
  }, options = list(pageLength = 10))
  }

shinyApp(ui = ui, server = server)


