library(shiny)

shinyUI(navbarPage(

  # Application title
  title = "Bradford Test Dash",

  tabPanel("Home",
           fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                plotlyOutput("odds.plot")
                                # put a plot re: conversions over time here
                                )
                    )
           ),
  navbarMenu("Formstack Responses",
             tabPanel("Info"),
             "----",
             "",
             tabPanel("Funnels", 
                      fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"),
                          selectInput("funnel.slot.number", "Time Frame:", 
                                      c("Weekly" = 2,
                                        "Monthly" = 1), 
                                      selected = 1),
                          selectInput("funnel.name", "Funnel:",
                                      c("Admin and Finance" = "anf",
                                        "Courts" = "courts",
                                        "Dept of Revenue" = "dor",
                                        "Education" = "edu"), selected = "anf")
                          ),
                        plotlyOutput("formstack.response.plot.funnels")
                      )),
             tabPanel("Endpoints"),
             tabPanel("Global", 
                      fluidRow(  
                        selectInput("global.slot.number", "Time Frame:", 
                                    c("Daily" = 1,
                                      "Weekly" = 3,
                                      "Monthly" = 2), 
                                    selected = 2),
                        plotlyOutput("formstack.response.plot.global")
                        
                        )
          )
      )
  )
)
