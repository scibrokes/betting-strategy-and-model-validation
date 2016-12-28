library("shiny")
library("highcharter")

data(citytemp)

ui <- fluidPage(
  h1("Highcharter Demo"),
  fluidRow(
    column(width = 4, class = "panel",
           selectInput("type", label = "Type", width = "100%",
                       choices = c("line", "column", "bar", "spline"), 
                       selected = "bar"), 
           selectInput("stacked", label = "Stacked",  width = "100%",
                       choices = c(FALSE, "normal", "percent"), 
                       selected = "percent"),
           selectInput("hc_theme", label = "Theme",  width = "100%",
                       choices = c("538" = "hc_theme_538()", "economist" = "hc_theme_economist()",
                                   "darkunica" = "hc_theme_darkunica()", 
                                   "gridlight" = "hc_theme_gridlight()", 
                                   "sandsignika" = "hc_theme_sandsignika()",
                                   "null" = "hc_theme_null()", "handdrwran" = "hc_theme_handdrawn()", 
                                   "chalk" = "hc_theme_chalk"), 
                       selected = "hc_theme_economist()"), 
           sliderInput("bins",
                       "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30),
           actionButton("tabBut", "View Table")),
    column(width = 8, highchartOutput("hcontainer",height = "500px"))))

server = function(input, output) {
  
  output$hcontainer <- renderHighchart({
    
    hc <- paste("hc <- highchart() %>% 
                hc_chart(type = input$type) %>% 
                hc_plotOptions(
                series = list(showInLegend = TRUE)) %>% 
                hc_add_series(data = list(
                list(y = 3, name = \"cat 1\"),
                list(y = 4, name = \"cat 2\"))) %>% 
                hc_add_theme(", input$hc_theme, ")
                
                if (input$stacked != FALSE) {
                hc <- hc %>%
                hc_plotOptions(showInLegend=TRUE,dataLabels=FALSE)
                }")
        eval(parse(text = hc))
  })}

shinyApp(ui = ui, server = server)


