

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observe({
    if (!dev_mode) {
      shinyjs::hide("load_test_data_wrapper")
    } else {
      shinyjs::show("load_test_data_wrapper")
    }
  })





  # Reactive values to hold the datasets
  dataset1 <- reactive({
    if (input$load_test_data) {
      return (df1)
    }
    else if (is.null(input$dataset1)) {
      return(NULL)  # Return NULL if no file is uploaded
    }
    haven::read_xpt(input$dataset1$datapath)

  })

  dataset2 <- reactive({
    if (input$load_test_data) {
      return (df2)
    }
    else if (is.null(input$dataset2)) {
      return(NULL)  # Return NULL if no file is uploaded
    }
    haven::read_xpt(input$dataset2$datapath)

  })


  output$compare_btn_ui <- renderUI({
    # Disable if datasets don't exist, or if unique keys have been requested but the keys aren't valid
    if (is.null(dataset1()) ||
        is.null(dataset2()) ||
        (input$unique_keys_check & !valid_keys())) {
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


  selected_keys <- reactiveVal(NULL)


  # Dynamically render dropdown menus for selecting keys
  output$key_selector_ui <- renderUI({
    req(input$unique_keys_check)
    selectInput(
      "key_vars",
      label = NULL,
      choices = common_vars(),
      multiple = TRUE
    )
  })


  valid_keys <- reactive({
    if (is.null(dataset1()) ||
        is.null(dataset2()) ||
        (is.null(input$key_vars) || length(input$key_vars) == 0)) {
      return(FALSE)
    } else{
      return(
        check_keys(dataset1(), keys = input$key_vars) &
          check_keys(dataset2(), keys = input$key_vars)
      )
    }

  })


  output$validation_message <- renderUI({
    req(input$unique_keys_check, dataset1(), dataset2())

    if (is.null(input$key_vars) || length(input$key_vars) == 0) {
      msg <- tags$b("Select Key Variables: ")
    } else{
      check1 <- check_keys(dataset1(), keys = input$key_vars)
      check2 <- check_keys(dataset2(), keys = input$key_vars)

      if (check1 & check2) {
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: green;", "Keys are valid")
        )
      } else if (!check1 & !check2) {
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: red;", "Keys do not define unique rows in either dataset")
        )
      } else if (check1 & !check2) {
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: red;", "Keys do not define unique rows in dataset 2")
        )
      } else if (!check1 & check2) {
        msg <-  tagList(
          tags$b("Select Key Variables: "),
          tags$span(style = "color: red;", "Keys do not define unique rows in dataset 1")
        )
      }

    }

    return(msg)
  })






  comparison_data <- reactiveVal(NULL)

  comparison_result <- reactiveVal(NULL)
  # comparisonDate <- reactiveVal(NULL)
  comparisonDate <- reactiveValues(
    date = "",
    time = ""
  )

  observeEvent(input$compare_btn, {
    req(dataset1)
    req(dataset2)



    if (input$unique_keys_check & valid_keys()) {
      selected_keys(input$key_vars)
      compare_list <- compareDatasets(dataset1(), dataset2(), selected_keys())
    } else{
      selected_keys(NULL)
      compare_list <- compareDatasets(dataset1(), dataset2())
    }
    comparisonDate$date <- Sys.Date()
    comparisonDate$time <- Sys.time()
    comparison_data(list(dataset1, dataset2))
    comparison_result(compare_list)
  })

  # Display the comparison result
  output$comparison_result <- renderPrint({
    comparison_result()
  })

  #
  # --- UI components
  #
  ##############################################################################


  output$row_count_check_output <- renderUI({
    req(comparison_result()$row_count_check)

    row_count_check_ui(result = comparison_result()$row_count_check,
                       unique_keys = selected_keys())
  })


  output$column_count_check_output <- renderUI({
    req(comparison_result()$column_count_check)

    column_count_check_ui(result = comparison_result()$column_count_check,
                          unique_keys = selected_keys())
  })

  output$rounding_check_output <- renderUI({
    req(comparison_result()$rounding_check)

    rounding_check_ui(result = comparison_result()$rounding_check)
  })

  output$value_check_output <- renderUI({
    req(comparison_result()$value_check)

    value_check_ui(result = comparison_result()$value_check,
                   unique_keys = selected_keys())
  })




  #
  # --- Comments
  #
  ##############################################################################

  comments <- reactiveValues(
    structure_and_content = "",
    row_count = "",
    column_count = "",
    rounding = "",
    value = ""
  )
  active_comment_id <- reactiveVal(NULL)

  comment_text <- reactiveVal(NULL)






  output$report_block <- renderUI({
    req(comparison_result())  # only proceeds when comparison_result is not NULL



    tagList(


      tags$h1("ADPP/ADPP-like comparison report"),

      tags$h2("Comparison Run Information"),

      report_metadata_ui(
        comparison_date = comparisonDate$date,
        comparison_time = comparisonDate$time,
        dataset1_name = input$dataset1$name,
        dataset2_name = input$dataset2$name,
        selected_keys = selected_keys()
      ),

      tags$hr(style = "border-top: 2px solid #888; margin-top: 20px; margin-bottom: 20px;"),

      tags$h2("Structure and Content Checks"),

      # structure_content_check_html(dataset1(), dataset2()),
      comparison_result()$results_structure_ui,

      tags$h3("Structure and Content Checks Comment", actionButton("structure_and_content_btn", "Edit Comment")),
      uiOutput("structure_and_content_display"),

      tags$hr(style = "border-top: 2px solid #888; margin-top: 20px; margin-bottom: 20px;"),

      tags$h2("Row-Level Checks"),



      tags$hr(),
      tags$h1("Old Stuff"),
      tags$h3(
        "Number of Rows Comparison",
        actionButton("row_count_check_comment_btn", "Edit Comment")
      ),
      uiOutput("row_count_check_output"),
      uiOutput("row_count_check_comment_display"),



      tags$h3(
        "Number of Columns comparison",
        actionButton("column_count_check_comment_btn", "Edit Comment")
      ),
      uiOutput("column_count_check_output"),
      uiOutput("column_count_check_comment_display"),

      tags$h2("Content Checks"),

      tags$h2("Row-Level Checks"),

      tags$h3(
        "Decimal Place Check",
        actionButton("rounding_check_comment_btn", "Edit Comment")
      ),
      uiOutput("rounding_check_output"),
      uiOutput("rounding_check_comment_display"),



      tags$h3(
        "Values Check",
        actionButton("value_check_comment_btn", "Edit Comment")
      ),
      uiOutput("value_check_output"),
      uiOutput("value_check_comment_display")
    )
  })

  # --- Stucture and Content checks comment ####################################


  output$structure_and_content_display <- renderUI({
    if (nzchar(comments$structure_and_content)) {
      tags$div(tags$strong("Comment:"), tags$p(comments$structure_and_content))
    }
  })


  observeEvent(input$structure_and_content_btn, {
    active_comment_id("structure_and_content")
    showModal(comment_modal(comments$structure_and_content))
  })

  ##############################################################################
  output$row_count_check_comment_display <- renderUI({
    if (nzchar(comments$row_count)) {
      tags$div(tags$strong("Comment:"), tags$p(comments$row_count))
    }
  })

  output$column_count_check_comment_display <- renderUI({
    if (nzchar(comments$column_count)) {
      tags$div(tags$strong("Comment:"),
               tags$p(comments$column_count))
    }
  })

  output$rounding_check_comment_display <- renderUI({
    if (nzchar(comments$rounding)) {
      tags$div(tags$strong("Comment:"), tags$p(comments$rounding))
    }
  })

  output$value_check_comment_display <- renderUI({
    if (nzchar(comments$value)) {
      tags$div(tags$strong("Comment:"), tags$p(comments$value))
    }
  })




  observeEvent(input$row_count_check_comment_btn, {
    active_comment_id("row_count")
    showModal(comment_modal(comments$row_count))
  })

  observeEvent(input$column_count_check_comment_btn, {
    active_comment_id("column_count")
    showModal(comment_modal(comments$column_count))
  })

  observeEvent(input$rounding_check_comment_btn, {
    active_comment_id("rounding")
    showModal(comment_modal(comments$rounding))
  })

  observeEvent(input$value_check_comment_btn, {
    active_comment_id("value")
    showModal(comment_modal(comments$value))
  })



  comment_modal <- function(current_value) {
    modalDialog(
      title = "Edit Comment",
      textAreaInput(
        "comment_text",
        "Enter your comment:",
        value = current_value,
        width = "100%"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_comment_btn", "Save")
      ),
      easyClose = TRUE
    )
  }

  observeEvent(input$save_comment_btn, {
    id <- active_comment_id()
    # req(id, input$comment_text)
    req(id, !is.null(input$comment_text))

    comments[[id]] <- input$comment_text
    removeModal()
  })


  #
  # --- Download button login
  #
  ##############################################################################

  output$download_ui <- renderUI({
    req(comparison_result())  # Ensure report exists
    downloadButton("download_report", "Download HTML Report")
  })

  output$download_report <- downloadHandler(
    filename = function() {
      "comparison_report.html"
      # paste0("adpp_comparison_report_",format(Sys.Date(), "%Y_%m_%d"),"T",format(Sys.time(), "%H_%M"),".html")
    },
    content = function(file) {
      # Save to a temporary file first
      temp_report <- tempfile(fileext = ".html")

      rmarkdown::render(
        input = "report_template.Rmd",
        output_file = temp_report,
        params = list(
          unique_keys = selected_keys(),
          comparison_result = comparison_result(),
          row_count_result = comparison_result()$row_count_check,
          column_count_result = comparison_result()$column_count_check,
          rounding_result = comparison_result()$rounding_check,
          value_result = comparison_result()$value_check,
          comments = reactiveValuesToList(comments),
          date_time = reactiveValuesToList(comparisonDate),
          datasets = list(dataset1_name = input$dataset1$name, dataset2_name = input$dataset2$name)

        ),
        envir = new.env(parent = globalenv())  # Prevents polluting global env
      )

      # Copy final report to the file Shiny wants to return
      file.copy(temp_report, file, overwrite = TRUE)
    }
  )
}
