library(shiny)
library(shinyURL)

shinyServer(function(input, output) {
  shinyURL.server()
  #### VISITOR SUCCESS ####
  # DATA MUNGING
  # subset by timeseries start and end
  visitor.success.subset.timeseries <- reactive({  # creates data frame subset by time selection
    ga.conversions %>%
      dplyr::filter(lubridate::date(date) >= input$visitor.success.daterange[1],
                    lubridate::date(date) <= input$visitor.success.daterange[2])
  })

  # subset by radio button selector and date
  visitor.success.aggregate.data <- reactive({

    # @connor I know this isn't properly functional, but it feels cleaner this way, can refactor if needed
    df = visitor.success.subset.timeseries()
      
    # Filter by dropdown
    if (input$visitor.success.type %in% COL.SELECT.TYPES & input$visitor.success.type.selector != 'All') {

          filtered.df = dplyr::filter_(df , paste0(input$visitor.success.type," == '",input$visitor.success.type.selector,"'"))       
      
      } else {

        filtered.df = df
      }
  
    # Add in columns that might need to be grouped (subtopic, topic, etc.)
    for (group.by in input$visitor.success.group.by){

      # Add columns to group by if they are associated with parents
      if (group.by %in% NON.COL.GROUP){

          join.df = drupal.node.descendants %>% dplyr::filter_(paste0(PARENT.TYPE," == '", group.by,"'"))
          filtered.df = dplyr::left_join(filtered.df, join.df, by = c("page_id" = "descendant_id")) %>%
                          dplyr::select(-content_type) %>%
                          # Fill na's, can have this be a filter
                          dplyr::mutate(title = ifelse(is.na(title), 'None', title))
                          

          names(filtered.df)[names(filtered.df) == 'title'] = gsub(' ','_',group.by)
      }

    }
    
    # Group dataframe from selected, always including date
    if (is.null(input$visitor.success.group.by)) {
        
        group.vars = c('date')
      
      } else{ 
        
        renamed.cols = unlist(lapply(input$visitor.success.group.by, FUN = function(x) {gsub(' ','_',x)}))
        group.vars = unlist(c('date', renamed.cols))

          # Create a single string in a column
          if( length(renamed.cols) > 1){
            filtered.df$group_factor = apply(filtered.df[, renamed.cols],
                                 MARGIN = 1, FUN = paste, collapse = " - ")
          }
          else{

          filtered.df$group_factor <- filtered.df[[renamed.cols]]

        }
      }
    
    # Group by all and conversions and return
     filtered.df %>%
        dplyr::group_by(date, group_factor) %>%
        dplyr::summarise(conversions = sum(conversions)) %>%
        return(.)
      
  })

  # REACTIVE UI
  #   filter by type

  output$type.selection.options <- renderUI({
    unique.subtypes <- visitor.success.subset.timeseries() %>%  # data which is 2 vector data frame 1) every type and 2) corresponding subtypes
    {
      if (input$visitor.success.type %in% COL.SELECT.TYPES) {
        unique(.[[input$visitor.success.type]]) %>%
          return(.)
      } else {
        return(NULL)
      }
    }
    selectInput(inputId = "visitor.success.type.selector",
                label = "Filter by Type (if applicable)",
                choices = c("All", unique.subtypes))
})

  # FILE DOWNLOADS
  output$visitor.success.download.timeseries <- downloadHandler(
    filename = function() {  # for some reason shiny expects it this way
      paste("vs-timeseries-", lubridate::today(tzone = "America/New_York"), ".csv", sep = "")
    },
    content = function(file) {
      visitor.success.aggregate.data() %>%
        
        getTopOrBottomK(., 
                        group.col = 'group_factor',
                        data.col = 'conversions',
                        k = as.numeric(input$visitor.success.select.k),
                        get.top = input$visitor.success.top.bottom) %>%
        dplyr::group_by(date) %>% 
        dplyr::mutate(day_pct = conversions / sum(conversions)) %>%
        dplyr::ungroup() %>%

      readr::write_csv(., file)
    }
  )

  output$visitor.success.download.aggregate <- downloadHandler(
    filename = function() {
      paste("vs-aggregate-", lubridate::today(tzone = "America/New_York"), ".csv", sep = "")
    },
    content = function(file) {
      
      visitor.success.aggregate.data() %>%
          groupAndOrder(.,
                        group.col = 'group_factor',
                        data.col = 'conversions',
                        percent = input$visitor.success.units,
                        top.k = as.numeric(input$visitor.success.select.k),
                        get.top.k = input$visitor.success.top.bottom,
                        filter.na = FALSE) %>%
          readr::write_csv(., file)
    }
  )

  # PLOTS
  #   Pareto
  output$visitor.success.grouped.pareto <- renderPlotly({
    if (is.null(input$visitor.success.group.by)) {  # if we have nothing to group on return a blank plot
      makeBlankPlot() %>%
        printGGplotly(.)
    }  
    else{
      visitor.success.aggregate.data() %>%

          groupAndOrder(.,
                    group.col = 'group_factor',
                    data.col = 'conversions',
                    percent = input$visitor.success.units,
                    top.k = as.numeric(input$visitor.success.select.k),
                    get.top.k = input$visitor.success.top.bottom,
                    filter.na = FALSE) %>%

          buildParetoChart(grouped.df = .,
                       x.lab = 'Grouped',
                       y.lab = 'Conversion Count',
                       title = "Successes",
                       cumul.line = TRUE,
                       percent = input$visitor.success.units)

        } %>%
        printGGplotly(.)
      })

  # grouped timeseries
  output$visitor.success.grouped.timeseries <- renderPlotly({


    if (is.null(input$visitor.success.group.by)) {  # if we have nothing to group on return a blank plot
      makeBlankPlot() %>%
        printGGplotly(.)
    }  
    else{

      # Set Y col for percent
      if(input$visitor.success.units){
        y.col = 'day_pct'
        } else{
          y.col = 'conversions'
        }
      
      visitor.success.aggregate.data() %>%

              getTopOrBottomK(., 
                                group.col = 'group_factor',
                                data.col = 'conversions',
                                k = as.numeric(input$visitor.success.select.k),
                                get.top = input$visitor.success.top.bottom) %>%
                dplyr::group_by(date) %>% 
                dplyr::mutate(day_pct = conversions / sum(conversions)) %>%
                dplyr::ungroup() %>%
                makeGroupedTimeseries(df = .,
                                            x = CONVERSION.DATE,
                                            y = y.col,
                                            fill = 'group_factor')

          } %>%
        printGGplotly(.)

        })

  #### SUCCESS RATE ####
  output$topic.conversions <- renderPlotly({
    
    topic.conversions %>%
      groupAndOrder(.,
                    group.col = RATE.GROUP.COL,
                    data.col = 'conversions',
                    percent = input$success.rate.percent,
                    top.pct = (as.numeric(input$pct.cutoffs) / 100)) %>%
      buildParetoChart(grouped.df = .,
                       x.lab = 'Topics',
                       y.lab = 'Conversions',
                       title = "Successes on Top Topics",
                       cumul.line = TRUE,
                       percent = input$success.rate.percent) %>%
      printGGplotly()
  })

  output$topic.conversion.rate <- renderPlotly({
    plt = topic.conversions %>% 
            dplyr::inner_join(., topic.sessions) %>%
            dplyr::mutate(conversion_rate = round(conversions / sessions,1) * 100) %>%   
            dplyr::arrange(., desc(sessions)) %>%
            dplyr::filter(sessions > quantile(sessions, 1 - (as.numeric(input$pct.cutoffs) / 100))) %>% # Top X% based on sessions
            ggplot(., aes_string(x = RATE.GROUP.COL, y = 'conversion_rate')) +
            geom_bar(stat = 'identity') + 
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = 'Topics', y = 'Conversion Rate (%)', title = 'Success Rate for Top Topics') + 
            scale_y_continuous(labels=scales::percent)

    printGGplotly(plt)
  })

  output$topic.sessions <- renderPlotly({

    topic.sessions %>%
      groupAndOrder(.,
                    group.col = RATE.GROUP.COL,
                    data.col = 'sessions',
                    percent = input$success.rate.percent,
                    top.pct = (as.numeric(input$pct.cutoffs) / 100)) %>%
      buildParetoChart(grouped.df = .,
                       x.lab = 'Topics',
                       y.lab = 'Sessions',
                       title = "Visits on Top Topics",
                       cumul.line = TRUE,
                       percent = input$success.rate.percent) %>%
      printGGplotly()
  })
})
