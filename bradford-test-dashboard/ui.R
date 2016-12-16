library(shiny)

shinyUI(navbarPage(

  # Application title
  title = "Bradford Test Dash",

  tabPanel("Plot",
           fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                plotlyOutput("odds.plot")
                                # put a plot re: conversions over time here
                                )
                    )
           ),
  tabPanel("Summary"),
  tabPanel("Table"),
  navbarMenu("Formstack Responses",
             tabPanel("Info"),
             "----",
             "",
             tabPanel("Funnels"),
             tabPanel("Endpoints"),
             tabPanel("Global", 
                      fluidRow(  
                        selectInput("slot.number", "Time Frame:", 
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
