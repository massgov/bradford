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
                 "Daily" = 1,
                 "Weekly" = 3,
                 "Monthly" = 2
               ),
               selected = 3
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
                 "exec.name",
                 "Funnel:",
                 c(
                   "Admin and Finance" = "anf",
                   "Courts" = "courts",
                   "Dept of Revenue" = "dor",
                   "Education" = "edu"
                 ),
                 selected = "anf"
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
  navbarMenu(
    "Analyst",
    tabPanel("Info"),
    "----",
    "",
    tabPanel(
      "Global",
      fluidRow(
        selectInput(
          "global.slot.number",
          "Time Frame:",
          c(
            "Daily" = 1,
            "Weekly" = 3,
            "Monthly" = 2
          ),
          selected = 2
        ),
        plotlyOutput("formstack.response.plot.global"),
        plotlyOutput("formstack.volume.plot.global.bar"),
        splitLayout(
          cellWidths = c("50%", "30%", "18%"),
          plotlyOutput("formstack.volume.plot.funnels"),
          plotlyOutput("formstack.os.plot.overall"),
          plotlyOutput("formstack.affirmative.plot.overall")
        ),
        br(),
        dataTableOutput("formstack.master")
      )
    ),
    tabPanel(
      "Funnels",
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
              "Admin and Finance" = "anf",
              "Courts" = "courts",
              "Dept of Revenue" = "dor",
              "Education" = "edu"
            ),
            selected = "anf"
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
    ),
    tabPanel(
      "Endpoints",
      fluidRow(
        selectInput(
          "endpoint.name",
          "Endpoint:",
          c(
            "/lwd/unemployment-insur/programs-and-services-for-claimants/training-opportunities-program/" = "http://www.mass.gov/lwd/unemployment-insur/programs-and-services-for-claimants/training-opportunities-program/",
            "/lwd/unemployment-insur/business-support/worksharing/" = "http://www.mass.gov/lwd/unemployment-insur/business-support/worksharing/",
            "/portal/employment/finding-a-job/" = "http://www.mass.gov/portal/employment/finding-a-job/"
          ),
          selected = "http://www.mass.gov/portal/employment/finding-a-job/"
        )
      ),
      plotlyOutput("formstack.response.plot.endpoints"),
      plotlyOutput("formstack.volume.plot.endpoint.bar"),
      splitLayout(
        cellWidths = c("44%", "44%"),
        plotlyOutput("formstack.os.plot.endpoint"),
        plotlyOutput("formstack.affirmative.plot.endpoint")
      ),
      br(),
      dataTableOutput("formstack.table.endpoint")
    )
  )
))

