library(shiny)

shinyServer(function(input, output) {
  #### MAIN ####
  # odds plot for metrics 
  output$odds.plot <- renderPlotly({
    limits = aes(ymax = odds_ratio + odds_ratio_se, ymin = odds_ratio - odds_ratio_se)
    print(ggplotly(  # print the output of ggplotly
      ggplot(conversion.metrics, aes(x = term, y = odds_ratio)) +
        geom_bar(stat = "identity") +
        ggtitle("Best Indicators That Someone Will Not Convert") +
        geom_errorbar(limits, width = 0.15, color = "grey60") +
        theme_bw()
    ))
  })
  
  output$formstack.response.plot.global.home <- renderPlotly({
    dat = data.frame(global.summary.frames[[as.numeric(input$global.slot.number.home)]])
    breakouts = global.breakout.frames[[as.numeric(input$global.slot.number.home)]]$loc %>%  # positions of the breakouts 
      global.summary.frames[[as.numeric(input$global.slot.number.home)]]$timestamp[.]  # subset the date vector
    plt = makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative",
                            plot.title = "Improvement in Satisfaction (Yes Rate) Over Time")
    print(ggplotly(plt)) # print the output of ggplotly
  })
  
  output$formstack.volume.plot.global.area <- renderPlotly({
    time.unit = c("month", "week")
    plt = formstack.master %>% 
      dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                      unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::group_by(timestamp, site) %>%
      dplyr::count() %>%
     makeVolumeAreaPlot(x = "timestamp", y = "n", fill = "site",
                        plot.title = "Funnel Response Count - Formstack")
    print(ggplotly(plt))
  })
  
  output$formstack.volume.plot.home.bar <- renderPlotly({
    time.unit = c("month", "week")
    plt = formstack.master %>% 
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
      dat = data.frame(global.summary.frames[[as.numeric(input$exec.slot.number)]]) 
      breakouts = global.breakout.frames[[as.numeric(input$exec.slot.number)]]$loc %>%  # positions of the breakouts 
        global.summary.frames[[as.numeric(input$exec.slot.number)]]$timestamp[.]  # subset the date vector
      plt = makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative", 
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time")
      return(print(ggplotly(plt)))
    } else {
      dat = data.frame(site.summary.frames[[as.numeric(input$exec.slot.number)]]) %>%
        dplyr::filter(site == input$exec.funnel.name)
      breakouts = site.breakout.frames[[as.numeric(input$exec.slot.number)]][[input$exec.funnel.name]]$loc %>%  # positions of the breakouts 
        site.summary.frames[[as.numeric(input$exec.slot.number)]]$timestamp[.]  # subset the date vector
      plt = makeBreakoutPlot(dat = dat, breakouts = breakouts,
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
    plt = dat %>%
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
    plt = dat %>%
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
  
  #### CONVERSIONS ####
  # conversion rate over time
  output$conversion.timeseries.plot <- renderPlotly({
    ga.conversions %>%
      dplyr::mutate(timestamp = lubridate::floor_date(
        lubridate::ymd_hms(hitTimestamp), unit = input$conversion.time.window
      )) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round((conversion / n) * 100), 2) %>%  # round to 2 decimal places
      ggplot(aes(x = timestamp, y = percent_convert)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      xlab("") +
      ylab("") +
      ggtitle("Percent of Sessions Converting")
  })

  # conversion percentages by key categorical variables
  
  output$conversion.device.plot <- renderPlotly({
    ga.conversions %>% 
      dplyr::select(deviceCategory, conversion) %>%
      dplyr::group_by(deviceCategory) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round((conversion / n) * 100), 2) %>% 
      ggplot(aes(x = deviceCategory, y = percent_convert)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      ggtitle("Conversions by Device Category") +
      xlab("") +
      ylab("Percent of Sessions Converting")
  })
  
  output$conversion.os.plot <- renderPlotly({
    ga.conversions %>% 
      dplyr::select(operatingSystem, conversion) %>%
      dplyr::group_by(operatingSystem) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round((conversion / n) * 100), 2) %>% 
      ggplot(aes(x = operatingSystem, y = percent_convert)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Conversions by Operating System") +
      xlab("") +
      ylab("")
  })
  
  output$conversion.browser.plot <- renderPlotly({
    ga.conversions %>% 
      dplyr::select(browser, conversion) %>%
      dplyr::group_by(browser) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round((conversion / n) * 100), 2) %>% 
      ggplot(aes(x = browser, y = percent_convert)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Conversions by Browser") +
      xlab("") +
      ylab("")
  })
  
  # session volume by key categorical variables
  output$volume.device.plot <- renderPlotly({
    ga.conversions %>% 
      dplyr::group_by(deviceCategory) %>%
      dplyr::count() %>% 
      ggplot(aes(x = deviceCategory, y = n)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      ggtitle("Sessions by Device Category") +
      xlab("") +
      ylab("Count")
  })
  
  output$volume.os.plot <- renderPlotly({
    ga.conversions %>% 
      dplyr::group_by(operatingSystem) %>%
      dplyr::count() %>% 
      ggplot(aes(x = operatingSystem, y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Sessions by Operating System") +
      xlab("") +
      ylab("")
  })
  
  output$volume.browser.plot <- renderPlotly({
    ga.conversions %>% 
      dplyr::group_by(browser) %>%
      dplyr::count() %>% 
      ggplot(aes(x = browser, y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Sessions by Browser") +
      xlab("") +
      ylab("")
  })
  
  #### ANALYST - USER SATISFACTION ####
  output$formstack.response.plot.funnels <- renderPlotly({
    
    if (input$funnel.name == "show.all") {
      dat <- data.frame(global.summary.frames[[as.numeric(input$funnel.slot.number)]]) 
      breakouts <- global.breakout.frames[[as.numeric(input$funnel.slot.number)]]$loc %>%  # positions of the breakouts 
        global.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]  # subset the date vector
      plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative", 
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time")
      return(print(ggplotly(plt)))
    } else {
      dat <- data.frame(site.summary.frames[[as.numeric(input$funnel.slot.number)]]) %>%
        dplyr::filter(site == input$funnel.name)
      breakouts <- site.breakout.frames[[as.numeric(input$funnel.slot.number)]][[input$funnel.name]]$loc %>%  # positions of the breakouts 
        site.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]
      plt <- makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative")
      return(print(ggplotly(plt))) # print the output of ggplotly
    }
  }) 
  
  output$formstack.volume.plot.funnel.bar <- renderPlotly({
    time.unit <- c("month", "week") # time unit for flooring dates 
    if (input$funnel.name == "show.all") {
      dat = formstack.master %>% 
        dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                        unit = time.unit[[as.numeric(input$funnel.slot.number)]])) %>%
        dplyr::group_by(timestamp) %>%
        dplyr::count()
    } else {
      dat = formstack.master %>% 
        dplyr::filter(site == input$funnel.name) %>%
        dplyr::mutate(timestamp = lubridate::floor_date(submit_time, 
                                                        unit = time.unit[[as.numeric(input$funnel.slot.number)]])) %>%
        dplyr::group_by(timestamp) %>%
        dplyr::count()
    }
    plt = makeVolumeBarPlot(df = dat, x = "timestamp", y = "n", ylab = "Response Count")
    print(ggplotly(plt))  
  })
  
  output$formstack.volume.plot.funnel.endpoints <- renderPlotly({
    if (input$funnel.name == "show.all") {
      dat = formstack.master %>%
        dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
        dplyr::summarise(n = n(),
                         Yes = (sum(info_found == "Yes") / n) * 100,
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::slice(1:10)
    } else {
      dat = formstack.master  %>% 
        dplyr::filter(site == input$funnel.name) %>%
        dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
        dplyr::summarise(n = n(),
                         Yes = (sum(info_found == "Yes") / n) * 100,
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::slice(1:10)
    }
    plt = dat %>%
      ggplot(aes(x = reorder(content_author, n), y = n, fill = Yes)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_continuous(name = "% Found <br>Content") +
      xlab("") +
      ylab("") +
      ggtitle("Endpoint Response Count") +
      theme_bw()
    print(ggplotly(plt))
  })
  
  output$formstack.os.plot.funnel <- renderPlotly({
    if (input$funnel.name == "show.all") {
      dat = formstack.master %>%
        dplyr::group_by(os) %>%
        dplyr::summarise(n = n(),
                         Yes = (sum(info_found == "Yes") / n) * 100,
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::slice(1:10)
    } else {
      dat = formstack.master %>%
        dplyr::filter(site == input$funnel.name) %>%
        dplyr::group_by(os) %>%
        dplyr::summarise(n = n(),
                         Yes = (sum(info_found == "Yes") / n) * 100,
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::slice(1:10)
    }
    plt = dat %>%
      ggplot(aes(x = reorder(os, n), y = n, fill = Yes)) +
      geom_bar(stat = "identity") +
      ggtitle("OS, Top 10") +
      xlab("") +
      coord_flip() +
      scale_fill_continuous(name = "% Found <br>Content") +
      ylab("") +
      theme_bw()
    print(ggplotly(plt))
  })  
  
  output$formstack.affirmative.plot.funnel <- renderPlotly({
    if (input$funnel.name == "show.all") {
      dat = formstack.master %>% 
        dplyr::group_by(info_found) %>%
        dplyr::count()
    } else {
      dat = formstack.master %>% 
        dplyr::filter(site == input$funnel.name) %>%
        dplyr::group_by(info_found) %>%
        dplyr::count()
    }
    plt = dat %>%
      makeAffirmativeBarPlot(x = "info_found", y = "n", plot.title = "Info Found?")
    print(ggplotly(plt))
  })
  
  output$formstack.table.funnel <- renderDataTable(
    if (input$funnel.name == "show.all") {
      formstack.master %>%
        dplyr::select(-c(site, child_content_author, ip_addr:lat, time_of_day))
    } else {
      formstack.master %>%
        dplyr::filter(site == input$funnel.name) %>%
        dplyr::select(-c(site, child_content_author, ip_addr:lat, time_of_day))
    },
    options = list(
      pageLength = 5,
      autoWidth = TRUE,
      dom = 'tp')
    )
  })