library(shiny)
library(shinydashboard)
library(shinyURL)

shinyUI(navbarPage(
  theme = "custom.css",
  # Application title
  title = "KPI Dashboard",
  #### VISITOR SUCCESS ####
  tabPanel(
    "Visitor Success",
    fluidPage(
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            dateRangeInput(
              inputId = "visitor.success.daterange",
              label = "Select a Date Range",
              start = "2017-01-01",
              end = yesterday,  # sourced from global.R
              startview = "month"
            ),
            radioButtons(
              inputId = "visitor.success.units",
              label = "Select Unit",
              choices = c("Percent" = TRUE,
                          "Number" = FALSE),
              selected = TRUE,
              inline = T
            ),
            radioButtons(
              inputId = "visitor.success.type",
              label =  "Filter C1s to Visualize",
              choices = c("All" = "all",
                          "Page Type" = "page.type",
                          "Service Type" = "service.type",
                          "Event Type" = "event.type"),
              selected = "all",
              inline = T
            ),
            uiOutput("type.selection.options"),
            checkboxGroupInput(
              inputId = "visitor.success.group.by",
              label = "Group by:",
              choices = c("Site Section Landing" = "site_section",
                          "Topic" = "topic",
                          "Sub-Topic" = "subtopic",
                          "Event Type" = "event_action",
                          "Referrer" = "source",
                          "Page Type" = "content_type"),
              selected = "site_section",
              inline = F
            ),
            br(),
            h5("Only Display"),
            splitLayout(
              cellWidths = c("50%", "50%"),
              selectInput(
                inputId = "visitor.success.top.bottom",
                label = NULL,
                choices = c("Top" = "top",
                            "Bottom" = "bottom"),
                selected = "top"
              ),
              numericInput(
                inputId = "visitor.success.select.k",
                label = NULL,
                value = 5,
                min = 1,
                max = 5
              )
            ),
            # URL generator
            shinyURL.ui(display = T, copyURL = T, tinyURL = T)
          ),
          mainPanel(
            plotlyOutput("visitor.success.grouped.pareto"),
            downloadButton("visitor.success.download.aggregate", "Download Plot Data"),
            br(),
            plotlyOutput("visitor.success.grouped.timeseries"),
            downloadButton("visitor.success.download.timeseries", "Download Plot Data")
          )
        )
      )
    )
  ),
  
  #### SUCCESS RATE ####
  tabPanel(title = "Success Rate",
           fluidRow(column(6,
                           selectInput("pct.cutoffs","View Top X% of Topics by Visits",pct.cutoffs, selected = 80)),
              radioButtons(
             inputId = "success.rate.percent",
             label = "Select Unit",
             choices = c("Percent" = TRUE,
                         "Number" = FALSE),
             selected = FALSE,
             inline = T
           )
          ),
           fluidRow(
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("topic.sessions"),
               plotlyOutput("topic.conversions"))),
           br(),
           plotlyOutput("topic.conversion.rate"))
))

