library(shiny)

shinyServer(function(input, output) {
# odds plot for metrics 
  output$odds.plot <- renderPlotly({
    limits <- aes(ymax = odds_ratio + odds_ratio_se, ymin = odds_ratio - odds_ratio_se)
     print(ggplotly(  # print the output of ggplotly
         ggplot(conversion.metrics, aes(x = term, y = odds_ratio)) +
          geom_bar(stat = "identity") +
          geom_errorbar(limits, width = 0.15, color = "grey60") +
          theme_bw()
       ))
    })
  
  
  output$formstack.response.plot.global <- renderPlotly({
    
    dat <- data.frame(global.summary.frames[[as.numeric(input$slot.number)]])
    breakouts <- global.breakout.frames[[as.numeric(input$slot.number)]]$loc %>%  # positions of the breakouts 
      global.summary.frames[[as.numeric(input$slot.number)]]$timestamp[.]  # subset the date vector
    plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "grand_mean")
    print(ggplotly(plt)) # print the output of ggplotly
  }) 
})
