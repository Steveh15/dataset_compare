

compare_ui <-  sidebarLayout(
  sidebarPanel(
    checkboxInput("load_test_data", "Load test data", value = FALSE),
    fileInput("dataset1", "Upload First Dataset (.xpt):", accept = c(".xpt")),
    fileInput("dataset2", "Upload Second Dataset (.xpt):", accept = c(".xpt")),
    checkboxInput("unique_keys_check", "Define unique keys for comparison?", value = FALSE),
    uiOutput("validation_message"),
    uiOutput("key_selector_ui"),
    uiOutput("compare_btn_ui"),
    uiOutput("download_ui")


  ),

  mainPanel(

    uiOutput("row_count_check_output"),
    uiOutput("column_count_check_output"),
    uiOutput("rounding_check_output")

  )
)
