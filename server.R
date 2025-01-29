
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Reactive values to hold the datasets
  dataset1 <- reactive({

    if(input$load_test_data){
      print("Hello!")
      return (df1)
    }
    else if (is.null(input$dataset1)) {
      return(NULL)  # Return NULL if no file is uploaded
    }
    haven::read_xpt(input$dataset1$datapath)

  })

  dataset2 <- reactive({

    if(input$load_test_data){
      print("Hello 2!")
      return (df2)
    }
    else if (is.null(input$dataset2)) {
      return(NULL)  # Return NULL if no file is uploaded
    }
    haven::read_xpt(input$dataset2$datapath)

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
    comparison_result()$column_diff$html_output
  })


  output$rounding_check_html <- renderUI({
    req(comparison_result()$rounding_check)
    comparison_result()$rounding_check$html_output
  })


  #
  # --- Download button login
  #
  ##############################################################################

  output$download_ui <- renderUI({
    req(comparison_result()$html_report)  # Ensure report exists
    downloadButton("download_report", "Download HTML Report")
  })

  output$download_report <- downloadHandler(
    filename = function() {
      "comparison_report.html"
    },
    content = function(file) {
      writeLines(comparison_result()$html_report, file)
    }
  )


}
