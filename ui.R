library(shiny)
library(shinydashboard)
library(shinyURL)
library(shinyjs)


shinyUI(navbarPage(
  theme = "custom.css",
  # Application title
  title = "KPI Dashboard",
  #### VISITOR SUCCESS ####
  tabPanel(
    "Visitor Success",
    fluidPage(
      useShinyjs(),  # Include shinyjs
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
            splitLayout(
            cellWidths = c("50%", "50%"),
              div(selectInput(
                inputId = "visitor.success.type",
                label =  "Filter C1s",
                choices = c("All" = "All",
                            "Page Type" = PAGE.TYPE,
                            "Service Type" = "service.type",
                            "Event Type" = EVENT.TYPE),
                selected = "All"
              ),
              br(),
              br()),
              shinyjs::hidden(div(
                id = "advanced",
                uiOutput("type.selection.options")))
            ),
            checkboxGroupInput(
              inputId = "visitor.success.group.by",

              label = "Group by:",
              choices = c("Site Section Landing" = SITE.SECTION,
                          "Topic" = TOPIC,
                          "Sub-Topic" = SUB.TOPIC,
                          "Event Type" = EVENT.TYPE,
                          "Referrer" = REFERRER,
                          "Page Type" = PAGE.TYPE),
              selected = SITE.SECTION,
              inline = F
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              selectInput(
                inputId = "visitor.success.top.bottom",
                label = "Limit groups displayed to",
                choices = c("Top" = TRUE,
                            "Bottom" = FALSE),
                selected = TRUE
              ),
              numericInput(
                inputId = "visitor.success.select.k",
              label = br(),
                value = 5,
                min = 1,
                max = 5
              )
            ),
          radioButtons(
            inputId = "visitor.success.units",
            label = "Display Unit",
            choices = c("Percent" = TRUE,
                        "Number" = FALSE),
            selected = TRUE,
            inline = T
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
    fluidPage(
           fluidRow(column(6,
                           selectInput("pct.cutoffs", "View Top X% of Topics by Visits", pct.cutoffs, selected = 80)),
              radioButtons(
             inputId = "success.rate.percent",
             label = "Select Unit",
             choices = c("Percent" = TRUE,
                         "Number" = FALSE),
             selected = FALSE,
             inline = T
           ),
           hr(),
           fluidRow(
             splitLayout(
               cellWidths = c("49%", "49%"),
               plotlyOutput("topic.sessions"),
               plotlyOutput("topic.conversions"))),
           br(),
           plotlyOutput("topic.conversion.rate")))),
  
  #### FAQ ####
  tabPanel(title = "FAQ and Help",
           includeHTML("faq/test.html"))
))



