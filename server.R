

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # --- Dataset Input
  #
  ##############################################################################

  output$file_path_input_1 <- renderUI({
    if (isTRUE(input$include_paths)) {
      tagList(
        textInput("file_path_1", "File path for Dataset 1")
      )
    }
  })

  output$file_path_input_2 <- renderUI({
    if (isTRUE(input$include_paths)) {
      tagList(
        textInput("file_path_2", "File path for Dataset 2")
      )
    }
  })


  # Reactive values to hold the datasets
  dataset1 <- reactive({
    req(input$dataset1)
    haven::read_xpt(input$dataset1$datapath)

  })

  dataset2 <- reactive({
    req(input$dataset2)
    haven::read_xpt(input$dataset2$datapath)

  })


  # --- Unique Keys Definition
  #
  ##############################################################################

  # Find common variables between the two datasets
  common_vars <- reactive({
    req(dataset1(), dataset2())
    intersect(colnames(dataset1()), colnames(dataset2()))
  })

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





  # --- Perform Comparison
  #
  ##############################################################################

  comparison_data <- reactiveVal(NULL)
  comparison_result <- reactiveVal(NULL)


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


  observeEvent(input$compare_btn, {
    req(dataset1)
    req(dataset2)

    if (isTRUE(input$include_paths)) {
      path1 <- input$file_path_1
      path2 <- input$file_path_2
    } else{
      path1 <- ""
      path2 <- ""
    }


    metadata_list <- list(
      path1 = path1,
      path2 = path2,
      name1 = "name1.xpt",
      name2 = "name2.xpt",
      datetime = Sys.time()
      )

    if (input$unique_keys_check & valid_keys()) {
      compare_list <- compareDatasets(
        df1 = dataset1(),
        df2 = dataset2(),
        unique_keys = input$key_vars,
        metadata = metadata_list
        )
    } else{
      compare_list <- compareDatasets(
        df1 = dataset1(),
        df2 = dataset2(),
        unique_keys = NULL,
        metadata = metadata_list
      )
    }

    comparison_data(list(dataset1, dataset2))
    comparison_result(compare_list)
  })

  # Display the comparison result
  output$comparison_result <- renderPrint({
    comparison_result()
  })


  #
  # --- Comments
  #
  ##############################################################################

  comments <- reactiveValues(
    structure_and_content = "",
    row_level = ""
  )

  active_comment_id <- reactiveVal(NULL)
  comment_text <- reactiveVal(NULL)



  output$report_block <- renderUI({
    req(comparison_result())  # only proceeds when comparison_result is not NULL

    tagList(


      tags$h1("ADPP/ADPP-like comparison report"),

      tags$h2("Comparison Run Information"),

      report_metadata_ui(
        metadata = comparison_result()$metadata,
        unique_keys = comparison_result()$unique_keys
      ),


      tags$hr(style = "border-top: 2px solid #888; margin-top: 20px; margin-bottom: 20px;"),

      tags$h2("Structure and Content Checks"),

      structure_content_check_html(comparison_result()$structure_checks),

      tags$h3("Structure and Content Checks Comment", actionButton("structure_and_content_btn", "Edit Comment")),
      uiOutput("structure_and_content_display"),

      tags$hr(style = "border-top: 2px solid #888; margin-top: 20px; margin-bottom: 20px;"),

      tags$h2("Row-Level Checks"),

      row_level_check_html(comparison_result()$row_level_checks, comparison_result()$unique_keys),
      if(!is.null(comparison_result()$unique_keys)){
        tagList(
            tags$h3("Row-Level Checks", actionButton("row_level_btn", "Edit Comment")),
            uiOutput("row_level_display")
        )
      }

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

  output$row_level_display <- renderUI({
    if (nzchar(comments$row_level)) {
      tags$div(tags$strong("Comment:"), tags$p(comments$row_level))
    }
  })

  observeEvent(input$row_level_btn, {
    active_comment_id("row_level")
    showModal(comment_modal(comments$row_level))
  })

  ##############################################################################

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
        paste0("adpp_comparison_report_",format(Sys.Date(), "%Y_%m_%d"),"T",format(Sys.time(), "%H_%M"),".html")
    },
    content = function(file) {
      # Save to a temporary file first
      temp_report <- tempfile(fileext = ".html")

      rmarkdown::render(
        input = "report_template.Rmd",
        output_file = temp_report,
        params = list(
          comparison_result = comparison_result(),
          comments = reactiveValuesToList(comments)
        ),
        envir = new.env(parent = globalenv())  # Prevents polluting global env
      )

      # Copy final report to the file Shiny wants to return
      file.copy(temp_report, file, overwrite = TRUE)
    }
  )
}
