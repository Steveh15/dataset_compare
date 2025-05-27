


sidebar_content <- tagList(

  # checkboxInput("load_test_data", "Load test data", value = FALSE),

    div(id = "load_test_data_wrapper",
        checkboxInput("load_test_data", "Load test data", value = FALSE)
    ),
  fileInput("dataset1", "Upload ADPP (.xpt):", accept = c(".xpt")),
  fileInput("dataset2", "Upload ADPP-like (.xpt):", accept = c(".xpt")),
  checkboxInput("unique_keys_check", "Define unique keys for row-level checks?", value = FALSE),
  uiOutput("validation_message"),
  uiOutput("key_selector_ui"),
  uiOutput("compare_btn_ui"),
  tags$div(style = "margin-top: 20px;"),
  uiOutput("download_ui")

  # uiOutput("comment_display"),
  # actionButton("edit_comment_btn", "Edit Comment")

)


compare_ui <-  sidebarLayout(

  sidebarPanel(
    sidebar_content)
  ,
  mainPanel(
    uiOutput("report_block")
  )

)


