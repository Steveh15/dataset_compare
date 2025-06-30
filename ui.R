#
# ui <- fluidPage(
#
#   # Application title
#   titlePanel("ADPP compare"),
#   compare_ui
# )



library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- function(request) {

  dashboardPage(
    dashboardHeader(title = "ADPP Compare"),
    dashboardSidebar(disable = TRUE),  # or add sidebar content here
    dashboardBody(
      useShinyjs(),

      tabsetPanel(
        tabPanel("App",
                 fluidRow(
                   titlePanel("ADPP Compare"),

                   compare_ui

                 )
        ),
        tabPanel("Information About the App",
                 includeMarkdown("info.md")
        )
      )
    )
  )
}
