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
            start = "2017-02-01",
            end = NULL,
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
          selectInput(
            inputId = "visitor.success.type.selector", 
            label = "Filter by Type (if applicbable)",
            choices = c("All" = "all"),
            selected = "all"
          ),
          checkboxGroupInput(
            inputId = "visitor.success.group.by",
            label = "Group by:", 
            choices = c("Site Section" = "site.section.landing",
                        "Topic" = "topic", 
                        "Subtopic" = "subtopic",
                        "Service Type" = "service.type",
                        "Service" = "service",
                        "Event Type" = "event.type",
                        "Guide" = "guide",
                        "Provider/Org" = "provider.org",
                        "Owner" = "owner",
                        "Referrer" = "referrer",
                        "Page Type" = "page.type"),
            selected = NULL, 
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
          )
          
        ),
        mainPanel(
          
        )
      )
    )
  ),
 
    #### VISITOR SUCCESS ####
    tabPanel(title = "Success Rate",
             plotlyOutput("topic.visits")),
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

