#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

rm(list = ls())

library(shiny)
library(dplyr)
library(shinyBS)
library(DT)



# shinyWidgets::popo
# shinyWidgets::shinyWidgetsGallery()



df1 <- haven::read_xpt(file.path("data", "df1.xpt"))
df2 <- haven::read_xpt(file.path("data", "df2.xpt"))

source("utils/compare_datasets_function.R")
source("utils/functions.R")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ADPP compare"),

        sidebarLayout(
          sidebarPanel(
            fileInput("dataset1", "Upload First Dataset (.xpt):", accept = c(".xpt")),
            fileInput("dataset2", "Upload Second Dataset (.xpt):", accept = c(".xpt")),
            # checkboxInput("unique_keys_check", "Define unique keys for comparison?", value = FALSE),
            # uiOutput("unique_keys_checkbox"),
            checkboxInput("unique_keys_check", "Define unique keys for comparison?", value = FALSE),
            uiOutput("validation_message"),
            uiOutput("key_selector_ui"),
            uiOutput("compare_btn_ui"),

            # actionButton("compare_btn", "Compare Datasets")

          ),

          mainPanel(

            uiOutput("key_ui"),
            # verbatimTextOutput("comparison_result"),
            uiOutput("rows_comparison_html"), # Placeholder for accordion content
            uiOutput("compare_columns_html"), # Placeholder for accordion content
            # bsPopover("rows_comparison_html"),
            # DTOutput("iris_table")

            )
          )
    )


# --- DEV SETTINGS
# ##########################################

auto_load_datasets <- TRUE


############################################
############################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # # Reactive values to hold the datasets
  # dataset1 <- reactive({
  #   # req(input$dataset1)
  #   if (is.null(input$dataset1)) {
  #     return(NULL)  # Return NULL if no file is uploaded
  #   }
  #   haven::read_xpt(input$dataset1$datapath)
  #   # df1
  # })
  #
  # dataset2 <- reactive({
  #   # req(input$dataset2)
  #   if (is.null(input$dataset2)) {
  #     return(NULL)  # Return NULL if no file is uploaded
  #   }
  #   haven::read_xpt(input$dataset2$datapath)
  #   # df2
  # })



  dataset1 <- reactive({
    df1
  })

  dataset2 <- reactive({
    df2
  })




  output$compare_btn_ui <- renderUI({


    # Disable if datasets don't exist, or if unique keys have been requested but the keys aren't valid
    if (is.null(dataset1()) || is.null(dataset2()) || (input$unique_keys_check & !valid_keys())  ) {
      actionButton("compare_btn", "Compare Datasets", disabled = TRUE)
    } else {
      # Both datasets are present, enable the button
      actionButton("compare_btn", "Compare Datasets")
    }


  })




  # Find common variables between the two datasets
  common_vars <- reactive({
    req(dataset1(), dataset2())
    intersect(colnames(dataset1()), colnames(dataset2()))
  })


  # Dynamically render dropdown menus for selecting keys
  output$key_selector_ui <- renderUI({
    req(input$unique_keys_check)
    selectInput("key_vars", label = NULL,
                choices = common_vars(), multiple = TRUE)
  })


  valid_keys <- reactive({

    if(is.null(dataset1()) || is.null(dataset2()) || (is.null(input$key_vars) || length(input$key_vars) == 0) ){
      return(FALSE)
    } else{
      return(check_keys(dataset1(), keys = input$key_vars) & check_keys(dataset2(), keys = input$key_vars))
    }

  })


  output$validation_message <- renderUI({
    req(input$unique_keys_check, dataset1(), dataset2())

    if(is.null(input$key_vars) || length(input$key_vars) == 0){
      msg <- tags$b("Select Key Variables: ")
    } else{

      check1 <- check_keys(dataset1(), keys = input$key_vars)
      check2 <- check_keys(dataset2(), keys = input$key_vars)

      if(check1 & check2){
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: green;", "Keys are valid")
        )
      } else if(!check1 & !check2){
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: red;", "Keys do not define unique rows in either dataset")
        )
      } else if(check1 & !check2){
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: red;", "Keys do not define unique rows in dataset 2")
        )
      } else if(!check1 & check2){
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: red;", "Keys do not define unique rows in dataset 1")
        )
      }

    }

    return(msg)
  })






  comparison_result <- reactiveVal(NULL)


  observeEvent(input$compare_btn, {

    req(dataset1)
    req(dataset2)


    if(input$unique_keys_check & valid_keys()){
      compare_list <- compareDatasets(dataset1(), dataset2(), input$key_vars)
    } else{
      compare_list <- compareDatasets(dataset1(), dataset2())
    }

    # Use your comparison function here
    # compare_list <- compareDatasets(dataset1(), dataset2())

    # Set the value of the comparison_result reactive here
    comparison_result(compare_list)
  })

  # Display the comparison result
  output$comparison_result <- renderPrint({
    comparison_result()
  })



  output$rows_comparison_html <- renderUI({
    req(comparison_result()$row_count_diff)
    comparison_result()$row_count_diff$html_output
  })


  output$compare_columns_html <- renderUI({
    req(comparison_result()$column_diff)
    # comparison_result()$row_count_diff$html_output
    comparison_result()$column_diff$html_output
  })


  output$key_ui <- renderUI({

    paste0(input$unique_keys_check, " | ", valid_keys(), " : ", paste0(input$key_vars, collapse = ", "))

  })

  output$iris_table <- renderDT({
    datatable(iris, options = list(pageLength = 5)) # Display with pagination
  })


}

# Run the application
shinyApp(ui = ui, server = server)


