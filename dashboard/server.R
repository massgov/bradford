library(shiny)

shinyServer(function(input, output) {
  #### MAIN ####
  output$user.satisfaction.total <- renderValueBox({
    user.satisfaction = factorPercentage(factor.vec = formstack.master$info_found,
                                         factor.value = "Yes") %>%
      prettyPercent(round.n = 1)  # flip to char and ensure % sign is included
    valueBox(
      user.satisfaction,
      "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })

  output$conversions.valuebox.home <- renderValueBox({
    conversion.rate = factorPercentage(factor.vec = ga.conversions$conversion,
                                       factor.value = "Conversion") %>%
      prettyPercent(round.n = 1)
    valueBox(
      conversion.rate,
      "Conversions", icon = icon("ok", lib = "glyphicon"),
      color = "yellow"
    )
  })

  output$home.client.count.valuebox <- renderValueBox({
    n.clients = length(unique(ga.conversions$clientID))
    valueBox(n.clients,
             subtitle = "Unique Clients",
             icon = icon("user", lib = "glyphicon"),
             color = "yellow")
  })

  output$home.session.count.valuebox <- renderValueBox({
    n.sessions = length(unique(ga.conversions$sessionID))
    valueBox(n.sessions,
             subtitle = "Sessions",
             icon = icon("transfer", lib = "glyphicon"),
             color = "yellow")
  })

  output$formstack.response.plot.global.home <- renderPlotly({
    dat = data.frame(global.summary.frames[[as.numeric(input$global.slot.number.home)]])
    # subset by positions of the breakouts in loc slot
    breakouts = global.breakout.frames[[as.numeric(input$global.slot.number.home)]]$loc %>%
      global.summary.frames[[as.numeric(input$global.slot.number.home)]]$timestamp[.]

    makeBreakoutPlot(dat = dat, breakouts = breakouts,
                            x = "timestamp", y = "prop_affirmative",
                            plot.title = "Improvement in Satisfaction (Yes Rate) Over Time") %>%
    printGGplotly() # print the output of ggplotly
  })

  output$formstack.volume.plot.global.area <- renderPlotly({
    time.unit = c("month", "week")
    formstack.master %>%
      dplyr::mutate(
        check_timeperiod = flagIncompleteTimeperiod(reference.vector = submit_time,
                                                    time.unit = time.unit[[as.numeric(input$global.slot.number.home)]]),
        timestamp = lubridate::floor_date(submit_time,
                                          unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::filter(check_timeperiod != F) %>%
      dplyr::group_by(timestamp, site) %>%
      dplyr::count() %>%
      makeVolumeAreaPlot(x = "timestamp", y = "n", fill = "site",
                         plot.title = "Funnel Response Count - Formstack") %>%
      printGGplotly()
  })

  output$formstack.volume.plot.home.bar <- renderPlotly({
    time.unit = c("month", "week")
    formstack.master %>%
      dplyr::mutate(check_timeperiod = flagIncompleteTimeperiod(reference.vector = submit_time,
                                                                time.unit = time.unit[[as.numeric(input$global.slot.number.home)]]),
      timestamp = lubridate::floor_date(submit_time,
                                        unit = time.unit[[as.numeric(input$global.slot.number.home)]])) %>%
      dplyr::filter(check_timeperiod != F) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::count() %>%
      makeVolumeBarPlot(x = "timestamp", y = "n",
                        plot.title = "Global Reponse Count - Formstack") %>%
    printGGplotly()
  })

  #### USER SATISFACTION ####
  output$user.satisfaction.funnel <- renderValueBox({
    if (input$exec.funnel.name != "show.all") {
      formstack.master = formstack.master %>%
        dplyr::filter(site == input$exec.funnel.name)
    }
    user.satisfaction = factorPercentage(factor.vec = formstack.master$info_found,
                                         factor.value = "Yes") %>%
    prettyPercent(round.n = 1)
    valueBox(
      user.satisfaction, # flip to char and ensure % sign is included
      subtitle = "Satisfied",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })

  output$mean.submission.formstack <- renderValueBox({
    if (input$exec.funnel.name != "show.all") {
      formstack.master = formstack.master %>%
        dplyr::filter(site == input$exec.funnel.name)
    }
    if (input$exec.slot.number == 2) {  # if weekly
      formstack.count = formstack.master %>%
        dplyr::mutate(
          check_timeperiod = flagIncompleteTimeperiod(reference.vector = submit_time,
                                                      time.unit = "week"),
          timestamp = lubridate::floor_date(submit_time,
                                            unit = "week")) %>%
        dplyr::filter(check_timeperiod != F) %>%
        dplyr::group_by(timestamp) %>%
        meanCount(round.n = 0)
      valueBox(
        formstack.count,
        subtitle = "Average Submissions / Week",
        icon = icon("list", lib = "glyphicon"),
        color = "yellow"
      )
    } else {
      formstack.count = formstack.master %>%
        dplyr::mutate(
          check_timeperiod = flagIncompleteTimeperiod(reference.vector = submit_time,
                                                      time.unit = "month"),
          timestamp = lubridate::floor_date(submit_time,
                                            unit = "month")) %>%
        dplyr::filter(check_timeperiod != F) %>%
        dplyr::group_by(timestamp) %>%
        meanCount(round.n = 0)
      valueBox(
        formstack.count,
        subtitle = "Average Submissions / Month",
        icon = icon("list", lib = "glyphicon"),
        color = "yellow"
      )
    }
  })

  output$formstack.response.plot.exec <- renderPlotly({
    if (input$exec.funnel.name == "show.all") { # show global stats if all is selected
      dat = data.frame(global.summary.frames[[as.numeric(input$exec.slot.number)]])

      breakouts = global.breakout.frames[[as.numeric(input$exec.slot.number)]]$loc %>%
        global.summary.frames[[as.numeric(input$exec.slot.number)]]$timestamp[.]

      makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative",
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time") %>%

      printGGplotly()
    } else {
      dat = data.frame(site.summary.frames[[as.numeric(input$exec.slot.number)]]) %>%
        dplyr::filter(site == input$exec.funnel.name)

      breakouts = site.breakout.frames[[as.numeric(input$exec.slot.number)]][[input$exec.funnel.name]]$loc %>%  # positions of the breakouts
        site.summary.frames[[as.numeric(input$exec.slot.number)]]$timestamp[.]  # subset the date vector

      makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative",
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time") %>%

      printGGplotly()
    }
  })

  output$formstack.volume.plot.exec.endpoints <- renderPlotly({
    if (input$exec.funnel.name == "show.all") {
      dat = formstack.master %>%
        dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
        dplyr::summarise(n = n(),
                         Yes = round( (sum(info_found == "Yes") / n) * 100, 2),
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(Yes)) %>%
        dplyr::slice(1:10)
    } else {
      dat = formstack.master %>%
        dplyr::filter(site == input$exec.funnel.name) %>%
        dplyr::group_by(content_author) %>%  # can also be enpoint (should be?)
        dplyr::summarise(n = n(),
                         Yes = round( (sum(info_found == "Yes") / n) * 100, 2),
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

    printGGplotly(plt)
  })

  output$formstack.os.plot.exec <- renderPlotly({
    if (input$exec.funnel.name == "show.all") {
      dat = formstack.master %>%
        dplyr::group_by(os) %>%
        dplyr::summarise(n = n(),
                         Yes = round( (sum(info_found == "Yes") / n) * 100, 2),
                         No = sum(info_found == "No") / n) %>%
        dplyr::arrange(desc(Yes)) %>%
        dplyr::slice(1:10)
    } else {
      dat = formstack.master %>%
        dplyr::filter(site == input$exec.funnel.name) %>%
        dplyr::group_by(os) %>%
        dplyr::summarise(n = n(),
                         Yes = round( (sum(info_found == "Yes") / n) * 100, 2),
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

      printGGplotly(plt)
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
    dat %>%
      makeAffirmativeBarPlot(x = "info_found", y = "n",
                             plot.title = "Satisfied and Unsatisfied Users") %>%
    printGGplotly()
  })

  #### CONVERSIONS ####
  output$conversions.valuebox <- renderValueBox({
    conversion.rate = factorPercentage(factor.vec = ga.conversions$conversion,
                                                factor.value = "Conversion") %>%
      prettyPercent(round.n = 1)
    valueBox(
      conversion.rate,
      subtitle = "Conversions",
      icon = icon("ok", lib = "glyphicon"),
      color = "yellow"
    )
  })

  output$session.count.valuebox <- renderValueBox({
    if (input$conversion.time.window == "day") {
      n.sessions = ga.conversions %>%
        dplyr::group_by(lubridate::floor_date(hit_time_utc, unit = "day")) %>%
        meanCount(round.n = 0)

      valueBox(
        n.sessions,
        subtitle = "Average Sessions / Day",
        icon = icon("transfer", lib = "glyphicon"),
        color = "yellow"
      )
    } else if (input$conversion.time.window == "week") {
      n.sessions = ga.conversions %>%
        dplyr::mutate(check_timeperiod = flagIncompleteTimeperiod(reference.vector = hit_time_utc,
                                                                  time.unit = "week"),
                      timestamp = lubridate::floor_date(hit_time_utc,
                                                        unit = "week")) %>%
        dplyr::filter(check_timeperiod != F) %>%
        dplyr::group_by(timestamp) %>%
        meanCount(round.n = 0)

      valueBox(
        n.sessions,
        subtitle = "Average Sessions / Week",
        icon = icon("transfer", lib = "glyphicon"),
        color = "yellow"
      )
    } else {
      n.sessions = ga.conversions %>%
        dplyr::mutate(check_timeperiod = flagIncompleteTimeperiod(reference.vector = hit_time_utc,
                                                                  time.unit = "month"),
                      timestamp = lubridate::floor_date(hit_time_utc,
                                                        unit = "month")) %>%
        dplyr::filter(check_timeperiod != F) %>%
        dplyr::group_by(timestamp) %>%
        meanCount(round.n = 0)

      valueBox(
        n.sessions,
        subtitle = "Average Sessions / Month",
        icon = icon("transfer", lib = "glyphicon"),
        color = "yellow"
      )
    }
  })

  output$conversions.client.count.valuebox <- renderValueBox({
    n.clients = length(unique(ga.conversions$clientID))
    valueBox(n.clients,
             subtitle = "Unique Clients",
             icon = icon("user", lib = "glyphicon"),
             color = "yellow")
  })

  # conversion rate over time
  output$conversion.timeseries.plot <- renderPlotly({
    plt = ga.conversions %>%
      dplyr::mutate(check_timeperiod = flagIncompleteTimeperiod(reference.vector = hit_time_utc,
                                                                time.unit = input$conversion.time.window),
                    timestamp = lubridate::floor_date(hit_time_utc,
                                                      unit = input$conversion.time.window)) %>%
      dplyr::filter(check_timeperiod != F) %>%
      dplyr::group_by(timestamp) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round( (conversion / n) * 100), 2) %>%  # round to 2 decimal places
      ggplot(aes(x = timestamp, y = percent_convert)) +
      geom_line(group = 1) +
      geom_point() +
      theme_bw() +
      xlab("") +
      ylab("") +
      ggtitle("Percent of Sessions Converting")
    printGGplotly(plt)
  })

  # conversion percentages by key categorical variables

  output$conversion.device.plot <- renderPlotly({
    plt = ga.conversions %>%
      dplyr::select(deviceCategory, conversion) %>%
      dplyr::group_by(deviceCategory) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round( (conversion / n) * 100), 2) %>%
      ggplot(aes(x = deviceCategory, y = percent_convert)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      ggtitle("Conversions by Device Category") +
      xlab("") +
      ylab("Percent of Sessions Converting")
    printGGplotly(plt)
  })

  output$conversion.os.plot <- renderPlotly({
    plt = ga.conversions %>%
      dplyr::select(operatingSystem, conversion) %>%
      dplyr::group_by(operatingSystem) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round( (conversion / n) * 100), 2) %>%
      ggplot(aes(x = operatingSystem, y = percent_convert)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Conversions by Operating System") +
      xlab("") +
      ylab("")

    printGGplotly(plt)
  })

  output$conversion.browser.plot <- renderPlotly({
   plt =  ga.conversions %>%
      dplyr::select(browser, conversion) %>%
      dplyr::group_by(browser) %>%
      dplyr::summarise(n = n(),
                       conversion = sum(conversion == "Conversion"),
                       percent_convert = round( (conversion / n) * 100), 2) %>%
      ggplot(aes(x = browser, y = percent_convert)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Conversions by Browser") +
      xlab("") +
      ylab("")

   printGGplotly(plt)
  })

  # session volume by key categorical variables
  output$volume.device.plot <- renderPlotly({
    plt = ga.conversions %>%
      dplyr::group_by(deviceCategory) %>%
      dplyr::count() %>%
      ggplot(aes(x = deviceCategory, y = n)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      ggtitle("Sessions by Device Category") +
      xlab("") +
      ylab("Count")

    printGGplotly(plt)
  })

  output$volume.os.plot <- renderPlotly({
    plt = ga.conversions %>%
      dplyr::group_by(operatingSystem) %>%
      dplyr::count() %>%
      ggplot(aes(x = operatingSystem, y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Sessions by Operating System") +
      xlab("") +
      ylab("")

    printGGplotly(plt)
  })

  output$volume.browser.plot <- renderPlotly({
   plt =  ga.conversions %>%
      dplyr::group_by(browser) %>%
      dplyr::count() %>%
      ggplot(aes(x = browser, y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_bw() +
      ggtitle("Sessions by Browser") +
      xlab("") +
      ylab("")

   printGGplotly(plt)
  })
  #### FUNNEL PERFORMANCE ####
  output$funnel.path.count.valuebox <- renderValueBox({
    funnel.session.count = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["n"]]

    valueBox(funnel.session.count,
             subtitle = "Sessions in Funnel",
             icon = icon("transfer", lib = "glyphicon"),
             color = "yellow")
  })

  output$funnel.conversion.rate.valuebox <- renderValueBox({
    hash.selected = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["pathHash"]]
    # calculate the conversion rate for a given funnel hash
    funnel.conversion.rate = ga.conversions %>%
      dplyr::filter(pathHash == hash.selected) %>%
      dplyr::summarise(
        n = n(),
        n_conversion = sum(conversion == "Conversion"),
        conversion_rate = prettyPercent(num = n_conversion / n,
                                        round.n = 2, is.percent.points = F)
        ) %>%
      .[["conversion_rate"]]

    valueBox(funnel.conversion.rate,
             subtitle = "Funnel Conversion Rate",
             icon = icon("ok", lib = "glyphicon"),
             color = "yellow")
  })

  output$funnel.client.count.valuebox <- renderValueBox({
    hash.selected = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["pathHash"]]
    # calculate n clients for current hash
    funnel.client.count = ga.conversions %>%
      dplyr::filter(pathHash == hash.selected) %>%
      summarise(n_clients = length(unique(clientID)))  %>%
      .[["n_clients"]]

    valueBox(funnel.client.count,
             subtitle = "Unique Clients",
             icon = icon("user", lib = "glyphicon"),
             color = "yellow")
  })

  output$funnel.median.client.session.valuebox <- renderValueBox({
    hash.selected = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["pathHash"]]

    funnel.client.session.median = ga.conversions %>%
      dplyr::filter(pathHash == hash.selected) %>%
      dplyr::group_by(clientID) %>%
      dplyr::count() %>%
      .[["n"]] %>%
      median()

      valueBox(funnel.client.session.median,
               subtitle = "Median Sessions / Client",
               icon = icon("tasks", lib = "glyphicon"),
               color = "yellow")
  })

  output$funnel.path.plot <- renderPlot({
    # this has to be ggplot for now because of a lack of support for geom_GeomLabel() within plotly
    ga.session.hashes.top20[[as.numeric(input$funnel.slot.n)]] %>%
      dplyr::mutate(path_step = 1:nrow(.),
                    fixed_y = rep(1, nrow(.))) %>%
      ggplot(aes(x = path_step, y = fixed_y, color = pagePath, label = pagePath)) +
      geom_path(group = 1,
                show.legend = F) +
      theme_minimal() +
      geom_label(aes(fill = factor(pagePath)), size = 4, colour = "white", fontface = "bold")
      xlim(1,
           padXlim( # one through padded value from padXlim
             plot.item.count = nrow(ga.session.hashes.top20[[as.numeric(input$funnel.slot.n)]]),
             offset = .5,
             item.limit = 3)) +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      guides(color = F, size = F, fill = F) +
      xlab("") +
      ylab("")
  })

  output$funnel.medium.plot <- renderPlotly({
    hash.selected = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["pathHash"]]

    plt = ga.conversions %>%
      dplyr::filter(pathHash == hash.selected) %>%
      dplyr::mutate(conversion = forcats::fct_relevel(conversion, "Conversion")) %>%
      dplyr::group_by(medium, conversion) %>%
      dplyr::count() %>%
      ggplot(aes(x = medium, y = n, fill = conversion)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw() +
      guides(fill = F) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = cb.palette) +
      ggtitle("Traffic Medium") +
      xlab("") +
      ylab("")

    print(ggplotly(plt) %>% layout(showlegend = FALSE))  # need to do this to hide legend
  })

  output$funnel.device.plot <- renderPlotly({
    hash.selected = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["pathHash"]]

    plt = ga.conversions %>%
      dplyr::filter(pathHash == hash.selected) %>%
      dplyr::mutate(conversion = forcats::fct_relevel(conversion, "Conversion")) %>%
      dplyr::group_by(deviceCategory, conversion) %>%
      dplyr::count() %>%
      ggplot(aes(x = deviceCategory, y = n, fill = conversion)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw() +
      guides(fill = F) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = cb.palette) +
      ggtitle("Device Category") +
      xlab("") +
      ylab("")

    print(ggplotly(plt) %>% layout(showlegend = FALSE))
  })

  output$funnel.browser.plot <- renderPlotly({
    hash.selected = ga.path.hashes.top20 %>%
      dplyr::slice(as.numeric(input$funnel.slot.n)) %>%
      .[["pathHash"]]

    plt = ga.conversions %>%
      dplyr::filter(pathHash == hash.selected) %>%
      dplyr::mutate(conversion = forcats::fct_relevel(conversion, "Conversion")) %>%
      dplyr::group_by(browser, conversion) %>%
      dplyr::count() %>%
      ggplot(aes(x = browser, y = n, fill = conversion)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw() +
      guides(fill = F) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = cb.palette) +
      ggtitle("Browser") +
      xlab("") +
      ylab("")
  print(ggplotly(plt) %>% layout(showlegend = FALSE))
  })

#### ANALYST USER SATISFACTION ####
  output$formstack.response.plot.funnels <- renderPlotly({
    if (input$funnel.name == "show.all") {
      dat = data.frame(global.summary.frames[[as.numeric(input$funnel.slot.number)]])

      breakouts = global.breakout.frames[[as.numeric(input$funnel.slot.number)]]$loc %>%  # positions of the breakouts
        global.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]  # subset the date vector

      makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative",
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time") %>%
      printGGplotly()
    } else {
      dat = data.frame(site.summary.frames[[as.numeric(input$funnel.slot.number)]]) %>%
        dplyr::filter(site == input$funnel.name)

      breakouts = site.breakout.frames[[as.numeric(input$funnel.slot.number)]][[input$funnel.name]]$loc %>%  # positions of the breakouts
        site.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]

      makeBreakoutPlot(dat = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative") %>%
      printGGplotly() # print the output of ggplotly
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
    makeVolumeBarPlot(df = dat, x = "timestamp", y = "n", ylab = "Response Count") %>%
    printGGplotly()
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

    printGGplotly(plt)
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

    printGGplotly(plt)
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
    dat %>%
      makeAffirmativeBarPlot(x = "info_found", y = "n", plot.title = "Info Found?") %>%
    printGGplotly()
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
      dom = "tp")
    )
  })