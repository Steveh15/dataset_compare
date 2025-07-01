


sidebar_content <- tagList(
  checkboxInput("include_paths", "Include folder paths for report?", value = FALSE),
  uiOutput("file_path_input_1"),
  fileInput("dataset1", "Upload ADPP (.xpt):", accept = c(".xpt")),
  uiOutput("dataset_1_validation_message"),
  uiOutput("file_path_input_2"),
  fileInput("dataset2", "Upload ADPP-like (.xpt):", accept = c(".xpt")),
  uiOutput("dataset_2_validation_message"),
  checkboxInput("unique_keys_check", "Define unique keys for row-level checks?", value = FALSE),
  uiOutput("validation_message"),
  uiOutput("key_selector_ui"),
  uiOutput("compare_btn_ui"),
  tags$div(style = "margin-top: 20px;"),
  uiOutput("download_ui"),


)


compare_ui <-  sidebarLayout(

  sidebarPanel(
    sidebar_content
    )
  ,
  mainPanel(
    uiOutput("report_block")
  )

)


