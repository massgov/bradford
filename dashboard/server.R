library(shiny)
library(shinyURL)

shinyServer(function(input, output) {
  shinyURL.server()
  #### VISITOR SUCCESS ####
  visitor.success.subset.data <- reactive({
    ga.conversions %>%
      dplyr::filter(hitTimestamp >= input$visitor.success.daterange[1],
                    hitTimestamp <= input$visitor.success.daterange[2]) %>%
                    {
                      # anon function to filter by service_type
                      if ("All" %in% c(input$visitor.success.type, input$visitor.success.type.selector)) {
                        return(.)
                      } else {
                        dplyr::filter(., service_type == input$visitor.success.type.selector) %>%
                          return()
                      }
                    } 
  })
  
  visitor.success.timeseries.data <- reactive({
    # add additional group_by hit_timestamp
    visitor.success.subset.data() %>%
      dplyr::group_by_(.dots = c("hit_timestamp", input$visitor.success.group.by)) %>%
      dplyr::count()
  })
  
  visitor.success.aggregate.data <- reactive({
    visitor.success.subset.data() %>%
      dplyr::group_by_(.dots = input$visitor.success.group.by) %>%
      dplyr::count()
  })
  
  output$type.selection.options <- renderUI({
    unique.subtypes <- dat %>%  # data which is 2 vector data frame 1) every type and 2) corresponding subtypes 
      dplyr::filter(type == input$visitor.success.type) %>%
      unique(.$subtype)
    selectInput(inputId = "visitor.success.type.selector",
                label = "Filter by Type (if applicbable)",
                choices = c("All" = "all", unique.subtypes),
                selected = "all")
  })
  
  # file download 
  output$visitor.success.download.timeseries <- downloadHandler(
    filename = function() {
      paste("vs-timeseries-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(visitor.success.timeseries.data(), file)
      }
    )
  
  output$visitor.success.download.aggregate <- downloadHandler(
    filename = function() {
      paste("vs-aggregate-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(visitor.success.aggregate.data(), file)
    }
  )
  
  # Plots 
  output$visitor.success.grouped.pareto <- renderPlotly({
    makeGroupedPareto(df = data.frame(), x = NULL, y = NULL) %>%
      printGGplotly()
  })
  
  output$visitor.success.grouped.timeseries <- renderPlotly({
    makeGroupedTimeseries(df = data.frame(), x = NULL, y = NULL) %>%
      printGGplotly()
  })
  #### ANALYST - USER SATISFACTION ####
  output$formstack.response.plot.funnels <- renderPlotly({
    if (input$funnel.name == "show.all") {
      dat = data.frame(global.summary.frames[[as.numeric(input$funnel.slot.number)]]) 
      
      breakouts = global.breakout.frames[[as.numeric(input$funnel.slot.number)]]$loc %>%  # positions of the breakouts 
        global.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]  # subset the date vector
      
      makeBreakoutPlot(df = dat, breakouts = breakouts,
                              x = "timestamp", y = "prop_affirmative", 
                              plot.title = "Improvement in Satisfaction (Yes Rate) Over Time") %>%
      printGGplotly()
    } else {
      dat = data.frame(site.summary.frames[[as.numeric(input$funnel.slot.number)]]) %>%
        dplyr::filter(site == input$funnel.name)
      
      breakouts = site.breakout.frames[[as.numeric(input$funnel.slot.number)]][[input$funnel.name]]$loc %>%  # positions of the breakouts 
        site.summary.frames[[as.numeric(input$funnel.slot.number)]]$timestamp[.]
      
      makeBreakoutPlot(df = dat, breakouts = breakouts,
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
      dom = 'tp')
    )
  })