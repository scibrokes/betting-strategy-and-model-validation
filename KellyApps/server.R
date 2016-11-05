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

## ========= ShinyServer ================================
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = 'loading-content', anim = TRUE, animType = 'fade')    
  show('app-content')
  
  onclick('toggleModels', shinyjs::toggle(id = 'adjuster', anim = TRUE))
  onclick('toggleAdvanced', shinyjs::toggle(id = 'advanced', anim = TRUE))    
  onclick('update', shinyjs::html('time', date()))
  
  observeEvent(input$analysis, {
    y = ifelse(is.null(input$response)||is.na(input$response)||length(input$response) == 0, 'Return', input$response)
    x = ifelse(is.null(input$explan1)||is.na(input$explan1)||length(input$explan1) == 0, 'Stakes', input$explan1)
    
    model = vKelly()
    output$modelSummary = renderPrint({
      list(Summary = summary(model), Anova = anova(model))
    })
  })
  
  ## --------------- 1. K1 ------------------------
  ## Reserved Stakes Kelly Models.
  output$K1 <- renderTable()
  
  ## --------------- 2. K2 ------------------------
  ## Reserved EM Probabilities Kelly Models.
  output$K2 <- renderTable()
  
  ## --------------- 3. K1W1 ------------------------
  ## Reserved Stakes Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L)
  output$K1W1 <- renderTable()
  
  ## --------------- 4. K1W2 ------------------------
  ## Reserved Stakes Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L)
  output$K2W2 <- renderTable()
  
  ## --------------- 5. K2W1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L)
  output$K2W1 <- renderTable()
  
  ## --------------- 6. K2W1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L)
  output$K2W2 <- renderTable()
  
  ## --------------- 7. K1W1WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1W1WS1 <- renderTable()
  
  ## --------------- 8. K1W1WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1W1WS2 <- renderTable()
  
  ## --------------- 9. K1W2WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1W2WS1 <- renderTable()
  
  ## --------------- 10. K1W2WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1W2WS2 <- renderTable()
  
  ## --------------- 11. K2W1WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2W1M1 <- renderTable()
  
  ## --------------- 12. K2W1WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2W1M2 <- renderTable()
  
  ## --------------- 13. K2W2WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2W2M1 <- renderTable()
  
  ## --------------- 14. K2W2WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2W2M2 <- renderTable()
  
  ## --------------- 15. K1D1 ------------------------
  ## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
  output$K1D1 <- renderTable()
  
  ## --------------- 16. K1D2 ------------------------
  ## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
  output$K2D2 <- renderTable()
  
  ## --------------- 17. K2D1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
  output$K2D1 <- renderTable()
  
  ## --------------- 18. K2D2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
  output$K2D2 <- renderTable()
  
  ## --------------- 19. K1D1WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1D1WS1 <- renderTable()
  
  ## --------------- 20. K1D1WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1D1WS2 <- renderTable()
  
  ## --------------- 21. K1D2WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1D2WS1 <- renderTable()
  
  ## --------------- 22. K1D2WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1D2WS2 <- renderTable()
  
  ## --------------- 23. K2D1WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2D1WS1 <- renderTable()
  
  ## --------------- 24. K2D1WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2D1WS2 <- renderTable()
  
  ## --------------- 25. K2D2WS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2D2WS1 <- renderTable()
  
  ## --------------- 26. K2D2WS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2D2WS2 <- renderTable()
  
  ## --------------- 27. K1W1DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1W1DWS1 <- renderTable()
  
  ## --------------- 28. K1W1DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1W1DWS2 <- renderTable()
  
  ## --------------- 29. K1W2DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1W2DWS1 <- renderTable()
  
  ## --------------- 30. K1W2DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1W2DWS2 <- renderTable()
  
  ## --------------- 31. K2W1DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2W1DWS1 <- renderTable()
  
  ## --------------- 32. K2W1DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2W1DWS2 <- renderTable()
  
  ## --------------- 33. K2W2DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2W2DWS1 <- renderTable()
  
  ## --------------- 34. K2W2DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2W2DWS2 <- renderTable()
  
  ## --------------- 35. K1D1DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1D1DWS1 <- renderTable()
  
  ## --------------- 36. K1D1DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1D1DWS2 <- renderTable()
  
  ## --------------- 37. K1D2DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K1D2DWS1 <- renderTable()
  
  ## --------------- 38. K1D2DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K1D2DWS2 <- renderTable()
  
  ## --------------- 39. K2D1DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2D1DWS1 <- renderTable()
  
  ## --------------- 40. K2D1DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2D1DWS2 <- renderTable()
  
  ## --------------- 41. K2D2DWS1 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
  output$K2D2DWS1 <- renderTable()
  
  ## --------------- 42. K2D2DWS2 ------------------------
  ## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
  output$K2D2DWS2 <- renderTable()
  
  ## -------------------- render plot -----------------------------------------------
  output$table1A <- renderFormattable({
    suppressMessages(library('magrittr'))
    compare[[1]] %>% formattable(list(
      AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      df = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(-x))),
      
      residuals = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(x))),
      
      p.value = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %02d)', x, rank(x))),
      
      delta.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      delta.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x)))
      
    ))
  })
  
  output$table1B <- renderFormattable({
    compare[[3]] %>% formattable(list(
      AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      df = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(-x))),
      
      residuals = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(x))),
      
      p.value = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %02d)', x, rank(x))),
      
      delta.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      delta.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x)))
      
    ))
  })
  
  output$table1C <- renderFormattable({
    compare[[2]] %>% formattable(list(
      AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      df = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(-x))),
      
      residuals = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %02d)', x, rank(x))),
      
      p.value = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %02d)', x, rank(x))),
      
      delta.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      delta.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x)))
      
    ))
  })
  
  output$table2 <- renderFormattable({
    bestlm %>% formattable(list(
      AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      df = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(-x))),
      
      residuals = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.0f (rank: %.0f)', x, rank(x))),
      
      p.value = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      delta.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.AIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      delta.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      Loglik.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x))),
      
      weight.BIC = formatter('span', style = x ~ formattable::style(color = ifelse(rank(x) <= 3, 'blue', 'white')), x ~ sprintf('%.4f (rank: %.0f)', x, rank(x)))
      
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