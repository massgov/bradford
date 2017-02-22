library(shiny)
library(shinydashboard)

shinyUI(navbarPage(
  # Application title
  title = "Bradford Test Dash",
  #### HOME ####
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
               cellWidths = c("25%", "25%", "25%", "25%"),
               valueBoxOutput("user.satisfaction.total"),
               valueBoxOutput("conversions.valuebox.home"),
               valueBoxOutput("home.client.count.valuebox"),
               valueBoxOutput("home.session.count.valuebox")
             ),
               #put a plot re: conversions over time here
             plotlyOutput("formstack.response.plot.global.home"),
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("formstack.volume.plot.global.area"),
               plotlyOutput("formstack.volume.plot.home.bar")
             )
           )),
  #### USER SATISFACTION ####
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
             splitLayout(
               cellWidths = c("25%", "25%", "25%", "25%"),
               valueBoxOutput("user.satisfaction.funnel"),
               valueBoxOutput("mean.submission.formstack")
               ),
             plotlyOutput("formstack.response.plot.exec"),
             splitLayout(
               cellWidths = c("50%", "30%", "18%"),
               plotlyOutput("formstack.volume.plot.exec.endpoints"),
               plotlyOutput("formstack.os.plot.exec"),
               plotlyOutput("formstack.affirmative.plot.exec")
             )
           )),
  #### CONVERSIONS ####
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

