library(shiny)

shinyUI(navbarPage(
  # Application title
  title = "Bradford Test Dash",
  
  tabPanel("Home",
           fluidRow(
             selectInput(
               "global.slot.number.home",
               "Time Frame:",
               c(
                 "Weekly" = 2,
                 "Monthly" = 1
                 ),
               selected = 2
             ),
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("odds.plot"),
               # put a plot re: conversions over time here
               plotlyOutput("formstack.response.plot.global.home")
             ),
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("formstack.volume.plot.global.area"),
               plotlyOutput("formstack.volume.plot.home.bar")
             )
           )),
  tabPanel("User Satisfaction",
           fluidRow(
             splitLayout(
               cellWidths = c("50%", "50%"),
               selectInput(
                 "exec.slot.number",
                 "Time Frame:",
                 c("Weekly" = 2,
                   "Monthly" = 1),
                 selected = 2
               ),
               selectInput(
                 "exec.funnel.name",
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
             plotlyOutput("formstack.response.plot.exec"),
             splitLayout(
               cellWidths = c("50%", "30%", "18%"),
               plotlyOutput("formstack.volume.plot.exec.endpoints"),
               plotlyOutput("formstack.os.plot.exec"),
               plotlyOutput("formstack.affirmative.plot.exec")
             )
           )),
  tabPanel("Conversions",
    fluidRow(
      splitLayout(
        cellWidths = c("50%", "50%"),
        selectInput(
          "conversion.time.window",
          "Time Frame:",
          c("Daily" = "day",
            "Weekly" = "week",
            "Monthly" = "month"
            )
        )
      ),
      plotlyOutput("conversion.timeseries.plot"),
      splitLayout(
        cellWidths = c("50%", "30%", "18%"),
        plotlyOutput("conversion.device.plot"),
        plotlyOutput("conversion.os.plot"),
        plotlyOutput("conversion.browser.plot")
      ),
      splitLayout(
        cellWidths = c("50%", "30%", "18%"),
        plotlyOutput("volume.device.plot"),
        plotlyOutput("volume.os.plot"),
        plotlyOutput("volume.browser.plot")
      )
    )
  ),
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

