library(shiny)
library(shinydashboard)
library(shinyURL)

shinyUI(navbarPage(
  # Application title
  title = "KPI Dashboard",
  #### VISITOR SUCCESS ####
  tabPanel(
    "Visitor Success",
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
            choices = c("Percent" = "percent", 
                        "Number" = "number"),
            selected = "percent", 
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
          downloadButton("visitor.success.download.aggregate", "Download Plot Data"),
          plotlyOutput("visitor.success.grouped.pareto"),
          downloadButton("visitor.success.download.timeseries", "Download Plot Data"),
          plotlyOutput("visitor.success.grouped.timeseries")
        )
      )
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
        splitLayout(
          cellWidths = c("50%", "50%"),
          selectInput(
            "funnel.slot.number",
            "Time Frame:",
            c("Weekly" = 2,
              "Monthly" = 1),
            selected = 2
          ),
          selectInput(
            "funnel.name",
            "Funnel:",
            c(
              "All" = "show.all",
              "Admin and Finance" = "anf",
              "Courts" = "courts",
              "Dept of Revenue" = "dor",
              "Education" = "edu"
            ),
            selected = "show.all"
          )
        ),
        plotlyOutput("formstack.response.plot.funnels"),
        plotlyOutput("formstack.volume.plot.funnel.bar"),
        splitLayout(
          cellWidths = c("50%", "30%", "18%"),
          plotlyOutput("formstack.volume.plot.funnel.endpoints"),
          plotlyOutput("formstack.os.plot.funnel"),
          plotlyOutput("formstack.affirmative.plot.funnel")
        ),
        br(),
        dataTableOutput("formstack.table.funnel")
      )
    )
  )
))

