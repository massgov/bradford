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
        selectInput(
          "conversion.time.window",
          "Time Frame:",
          c("Daily" = "day",
            "Weekly" = "week",
            "Monthly" = "month"
            )
        )
      ),
      splitLayout(
        cellWidths = c("25%", "25%", "25%", "25%"),
        valueBoxOutput("conversions.valuebox"),
        valueBoxOutput("session.count.valuebox"),
        valueBoxOutput("conversions.client.count.valuebox")
        ),
      plotlyOutput("conversion.timeseries.plot"),
      splitLayout(
        cellWidths = c("40%", "30%", "28%"),
        plotlyOutput("conversion.device.plot"),
        plotlyOutput("conversion.os.plot"),
        plotlyOutput("conversion.browser.plot")
      ),
      splitLayout(
        cellWidths = c("40%", "30%", "28%"),
        plotlyOutput("volume.device.plot"),
        plotlyOutput("volume.os.plot"),
        plotlyOutput("volume.browser.plot")
      )
    )
  ),
  #### FUNNEL PERFORMANCE ####
  tabPanel("Funnel Performance",
    selectInput(
      "funnel.slot.n",
      "Top 20 Funnels:",
       c("1" = 1,
         "2" = 2,
         "3" = 3,
         "4" = 4,
         "5" = 5,
         "6" = 6,
         "7" = 7,
         "8" = 8,
         "9" = 9,
         "10" = 10,
         "11" = 11,
         "12" = 12,
         "13" = 13,
         "14" = 14,
         "15" = 15,
         "16" = 16,
         "17" = 17,
         "18" = 18,
         "19" = 19,
         "20" = 20),
      selected = 1
    ),
    splitLayout(
      cellWidths = c("25%", "25%", "25%", "25%"),
      valueBoxOutput("funnel.conversion.rate.valuebox"),
      valueBoxOutput("funnel.path.count.valuebox"),
      valueBoxOutput("funnel.client.count.valuebox"),
      valueBoxOutput("funnel.median.client.session.valuebox")
    ),
    plotOutput("funnel.path.plot"),
    splitLayout(
      cellWidths = c("33%", "33%", "33%"),
      plotlyOutput("funnel.medium.plot"),
      plotlyOutput("funnel.device.plot"),
      plotlyOutput("funnel.browser.plot")
    )
  ),
  #### ANALYST ####
  navbarMenu(
    "Analyst",
    tabPanel("Info",
             htmlOutput("help.html")),
    "----",
    "",
    tabPanel(
      "User Satisfaction",
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
                choices = c("All" = "all",
                            "Page Type" = "page.type",
                            "Service Type" = "service.type",
                            "Event Type" = "event.type"),
                selected = "all"
              ),
              br(),
              br()),
              shinyjs::hidden(div(
                id = "advanced",
                uiOutput("type.selection.options")))
            ),
            checkboxGroupInput(
              inputId = "visitor.success.group.by",
            label = "Group By",
              choices = c("Site Section Landing" = "site_section",
                          "Topic" = "topic",
                          "Sub-Topic" = "subtopic",
                          "Event Type" = "event_action",
                          "Referrer" = "source",
                          "Page Type" = "content_type"),
              selected = "site_section",
              inline = F
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              selectInput(
                inputId = "visitor.success.top.bottom",
              label = "Limit groups displayed to",
                choices = c("Top" = "top",
                            "Bottom" = "bottom"),
                selected = "top"
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
            choices = c("Percent" = "percent",
                        "Number" = "number"),
            selected = "percent",
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
           plotlyOutput("topic.conversion.rate"))
))))