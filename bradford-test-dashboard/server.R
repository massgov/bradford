library(shiny)

shinyServer(function(input, output) {
  #### MAIN ####
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
  
  output$formstack.response.plot.global.home <- renderPlotly({
    dat <- data.frame(global.summary.frames[[as.numeric(input$global.slot.number.home)]])
    breakouts <- global.breakout.frames[[as.numeric(input$global.slot.number.home)]]$loc %>%  # positions of the breakouts 
      global.summary.frames[[as.numeric(input$global.slot.number.home)]]$timestamp[.]  # subset the date vector
    plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative")
    print(ggplotly(plt)) # print the output of ggplotly
  })
  
  output$formstack.volume.plot.global.area <- renderPlotly({
    time.unit <- c("day", "month", "week")
    plt <- formstack.master %>% 
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::group_by(timestamp, site) %>%
      dplyr::count() %>%
      ggplot(aes(x = timestamp, y = n, fill = site)) +
      geom_area() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Site Response Count - Formstack") +
      xlab("") +
      ylab("")
    print(ggplotly(plt))  
  })
  
  output$formstack.volume.plot.global.bar <- renderPlotly({
    time.unit <- c("day", "month", "week")
    plt <- formstack.master %>% 
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::count() %>%
      ggplot(aes(x = timestamp, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_bw() +
      ggtitle("Global Response Count - Formstack") +
      xlab("") +
      ylab("")
    print(ggplotly(plt))  
  })
  
  #### FORMSTACK - GLOBAL ####
  output$formstack.response.plot.global <- renderPlotly({
    dat <- data.frame(global.summary.frames[[as.numeric(input$global.slot.number)]])
    breakouts <- global.breakout.frames[[as.numeric(input$global.slot.number)]]$loc %>%  # positions of the breakouts 
      global.summary.frames[[as.numeric(input$global.slot.number)]]$timestamp[.]  # subset the date vector
    plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative")
    print(ggplotly(plt)) # print the output of ggplotly
  }) 
  
  output$formstack.volume.plot.funnels <- renderPlotly({
    plt <- formstack.master %>% 
      dplyr::group_by(site) %>%
      dplyr::summarise(n = n(),
                       Yes = (sum(info_found == "Yes") / n) * 100,
                       No = sum(info_found == "No") / n) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:10) %>%
      ggplot(aes(x = reorder(site, n), y = n, fill = Yes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_continuous(name = "% Found <br>Content") +
      #guides(fill = F) +
      xlab("") +
      ylab("") +
      ggtitle("Response Count") +
      theme_bw()
    print(ggplotly(plt))
  })
  
  output$formstack.affirmative.plot.overall <- renderPlotly({
    plt <- formstack.master %>% 
      dplyr::group_by(info_found) %>%
      dplyr::count() %>%
      ggplot(aes(x = info_found, y = n)) +
      geom_bar(stat = "identity") +
      xlab("") +
      ggtitle("Information Found?") +
      ylab("") +
      theme_bw()
    print(ggplotly(plt))
  })  
  
  output$formstack.os.plot.overall <- renderPlotly({
    plt <- formstack.master %>% 
      dplyr::group_by(os) %>%
      dplyr::count() %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:10) %>%
      ggplot(aes(x = reorder(os, n), y = n)) +
      geom_bar(stat = "identity") +
      ggtitle("OS, Top 10") +
      xlab("") +
      coord_flip() +
      ylab("") +
      theme_bw()
    print(ggplotly(plt))
  })  
  
  # formstack master 
  output$formstack.master <- renderDataTable(formstack.master %>%
                                               dplyr::select(-c(child_content_author, 
                                                                ip_addr:lat, time_of_day)),
                                             options = list(
                                               pageLength = 5,
                                               autoWidth = TRUE,
                                               dom = 'tp'))
  
  #### FORMSTACK - FUNNELS ####
  output$formstack.response.plot.funnels <- renderPlotly({
    dat <- data.frame(site.summary.frames[[as.numeric(input$funnel.slot.number)]]) %>%
      dplyr::filter(site == input$funnel.name)
    breakouts <- site.breakout.frames[[as.numeric(input$funnel.slot.number)]][[input$funnel.name]]$loc %>%  # positions of the breakouts 
      site.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]  # subset the date vector
    plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative")
    print(ggplotly(plt)) # print the output of ggplotly
  }) 
  
  output$formstack.volume.plot.funnel.endpoints <- renderPlotly({
    plt <- formstack.master  %>% 
      dplyr::filter(site == input$funnel.name) %>%
      dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
      dplyr::summarise(n = n(),
                       Yes = (sum(info_found == "Yes") / n) * 100,
                       No = sum(info_found == "No") / n) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:10) %>%
      ggplot(aes(x = reorder(content_author, n), y = n, fill = Yes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_continuous(name = "% Found <br>Content") +
      #guides(fill = F) +
      xlab("") +
      ylab("") +
      ggtitle("Response Count") +
      theme_bw()
    print(ggplotly(plt))
  })
  
  output$formstack.os.plot.funnel <- renderPlotly({
    plt <- formstack.master %>%
      dplyr::filter(site == input$funnel.name) %>%
      dplyr::group_by(os) %>%
      dplyr::count() %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:10) %>%
      ggplot(aes(x = reorder(os, n), y = n)) +
      geom_bar(stat = "identity") +
      ggtitle("OS, Top 10") +
      xlab("") +
      coord_flip() +
      ylab("") +
      theme_bw()
    print(ggplotly(plt))
  })  
  
  output$formstack.affirmative.plot.funnel <- renderPlotly({
    plt <- formstack.master %>% 
      dplyr::filter(site == input$funnel.name) %>%
      dplyr::group_by(info_found) %>%
      dplyr::count() %>%
      ggplot(aes(x = info_found, y = n)) +
      geom_bar(stat = "identity") +
      xlab("") +
      ggtitle("Information Found?") +
      ylab("") +
      theme_bw()
    print(ggplotly(plt))
  })
  
  output$formstack.table.funnel <- renderDataTable(formstack.master %>%
                                               dplyr::filter(site == input$funnel.name) %>%
                                               dplyr::select(-c(site, child_content_author, 
                                                                ip_addr:lat, time_of_day)),
                                             options = list(
                                               pageLength = 5,
                                               autoWidth = TRUE,
                                               dom = 'tp'))
})
