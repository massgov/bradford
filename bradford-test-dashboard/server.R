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
    time.unit <- c("month", "week")
    plt <- formstack.master %>% 
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::group_by(timestamp, site) %>%
      dplyr::count() %>%
     makeVolumeAreaPlot(x = "timestamp", y = "n", fill = "site",
                        plot.title = "Funnel Response Count - Formstack")
    print(ggplotly(plt))
  })
  
  output$formstack.volume.plot.home.bar <- renderPlotly({
    time.unit <- c("month", "week")
    plt <- formstack.master %>% 
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::count() %>%
      makeVolumeBarPlot(x = "timestamp", y = "n", plot.title = "Global Reponse Count - Formstack")
    print(ggplotly(plt))
  })
  
  #### USER SATISFACTION ####
  output$formstack.response.plot.exec <- renderPlotly({
    if (input$exec.funnel.name == "show.all") { # show global stats if all is selected 
      dat <- data.frame(global.summary.frames[[as.numeric(input$exec.slot.number)]]) 
      breakouts <- global.breakout.frames[[as.numeric(input$exec.slot.number)]]$loc %>%  # positions of the breakouts 
        global.summary.frames[[as.numeric(input$exec.slot.number)]]$timestamp[.]  # subset the date vector
      plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative", 
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time")
      return(print(ggplotly(plt)))
    } else {
      dat <- data.frame(site.summary.frames[[as.numeric(input$exec.slot.number)]]) %>%
        dplyr::filter(site == input$exec.funnel.name)
      breakouts <- site.breakout.frames[[as.numeric(input$exec.slot.number)]][[input$exec.funnel.name]]$loc %>%  # positions of the breakouts 
        site.summary.frames[[as.numeric(input$exec.slot.number)]]$timestamp[.]  # subset the date vector
      plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative", 
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time")
      return(print(ggplotly(plt))) # print the output of ggplotly
    }
  }) 
  
  output$formstack.volume.plot.exec.endpoints <- renderPlotly({
    if (input$exec.funnel.name == "show.all") {
      dat = formstack.master %>% 
        dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
        dplyr::summarise(n = n(),
                         Yes = round((sum(info_found == "Yes") / n) * 100, 2),
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(Yes)) %>%
        dplyr::slice(1:10)
    } else {
      dat = formstack.master %>% 
        dplyr::filter(site == input$exec.funnel.name) %>%
        dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
        dplyr::summarise(n = n(),
                         Yes = round((sum(info_found == "Yes") / n) * 100, 2),
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(Yes)) %>%
        dplyr::slice(1:10)
    }
    plt <- dat %>%
      ggplot(aes(x = reorder(content_author, Yes), y = Yes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("") +
      ylab("% of Users Satisfied") +
      ggtitle("User Satisfaction Across Content Group - Top 10") +
      theme_bw()
    print(ggplotly(plt))
  })
  
  output$formstack.os.plot.exec <- renderPlotly({
    if (input$exec.funnel.name == "show.all") {
      dat = formstack.master %>%
        dplyr::group_by(os) %>%
        dplyr::summarise(n = n(),
                         Yes = round((sum(info_found == "Yes") / n) * 100, 2),
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(Yes)) %>%
        dplyr::slice(1:10)
    } else {
      dat = formstack.master %>%
        dplyr::filter(site == input$exec.funnel.name) %>%
        dplyr::group_by(os) %>%
        dplyr::summarise(n = n(),
                         Yes = round((sum(info_found == "Yes") / n) * 100, 2),
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(Yes)) %>%
        dplyr::slice(1:10)
    }
    plt <- dat %>%
      ggplot(aes(x = reorder(os, Yes), y = Yes)) +
      geom_bar(stat = "identity") +
      ggtitle("User Satisfaction Across OS - Top 10") +
      xlab("") +
      coord_flip() +
      ylab("% of Users Satisfied") +
      theme_bw()
    print(ggplotly(plt))
  })  
  
  output$formstack.affirmative.plot.exec <- renderPlotly({
    if (input$exec.funnel.name == "show.all") {
      dat = formstack.master %>%
        dplyr::group_by(info_found) %>%
        dplyr::count()
    } else {
      dat = formstack.master %>% 
        dplyr::filter(site == input$exec.funnel.name) %>%
        dplyr::group_by(info_found) %>%
        dplyr::count()
    }
    plt <- dat %>%
      makeAffirmativeBarPlot(x = "info_found", y = "n", 
                             plot.title = "Satisfied and Unsatisfied Users")
    print(ggplotly(plt))
  })
  
  #### ANALYST - USER SATISFACTION FUNNELS ####
  output$formstack.response.plot.funnels <- renderPlotly({
    dat <- data.frame(site.summary.frames[[as.numeric(input$funnel.slot.number)]]) %>%
      dplyr::filter(site == input$funnel.name)
    breakouts <- site.breakout.frames[[as.numeric(input$funnel.slot.number)]][[input$funnel.name]]$loc %>%  # positions of the breakouts 
      site.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]  # subset the date vector
    plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative")
    print(ggplotly(plt)) # print the output of ggplotly
  }) 
  
  output$formstack.volume.plot.funnel.bar <- renderPlotly({
    time.unit <- c("month", "week")
    plt <- formstack.master %>% 
      dplyr::filter(site == input$funnel.name) %>%
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = time.unit[[as.numeric(input$funnel.slot.number)]])) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::count() %>%
      makeVolumeBarPlot(x = "timestamp", y = "n", ylab = "Response Count")
    print(ggplotly(plt))  
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
      ggtitle("Endpoint Response Count") +
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
      makeAffirmativeBarPlot(x = "info_found", y = "n", plot.title = "Info Found?")
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
  
  #### FORMSTACK - ENDPOINTS ####
  output$formstack.response.plot.endpoints <- renderPlotly({
    dat <- data.frame(referrer.summary.monthly) %>%
      dplyr::filter(referrer == input$endpoint.name)
    breakouts <- referrer.breakouts.monthly[[input$endpoint.name]]$loc %>%  # positions of the breakouts 
      referrer.summary.monthly$timestamp[.]  # subset the date vector
    plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative")
    print(ggplotly(plt)) # print the output of ggplotly
  }) 
  
  output$formstack.volume.plot.endpoint.bar <- renderPlotly({
    plt <- formstack.master %>% 
      dplyr::filter(referrer == input$endpoint.name) %>%
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = "month")) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::count() %>%
      makeVolumeBarPlot(x = "timestamp", y = "n", ylab = "Response Count")
    print(ggplotly(plt))  
  })
  
  
  output$formstack.os.plot.endpoint <- renderPlotly({
    plt <- formstack.master %>%
      dplyr::filter(referrer == input$endpoint.name) %>%
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
  
  output$formstack.affirmative.plot.endpoint <- renderPlotly({
    plt <- formstack.master %>% 
      dplyr::filter(referrer == input$endpoint.name) %>%
      dplyr::group_by(info_found) %>%
      dplyr::count() %>%
      makeAffirmativeBarPlot(x = "info_found", y = "n", plot.title = "Info Found?")
    print(ggplotly(plt))
  })
  
  output$formstack.table.endpoint <- renderDataTable(formstack.master %>%
                                                     dplyr::filter(referrer == input$endpoint.name) %>%
                                                     dplyr::select(-c(site, referrer, child_content_author, 
                                                                      ip_addr:lat, time_of_day)),
                                                   options = list(
                                                     pageLength = 5,
                                                     autoWidth = TRUE,
                                                     dom = 'tp'))
})
