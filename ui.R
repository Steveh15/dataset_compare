
ui <- fluidPage(

  # Application title
  titlePanel("ADPP compare"),

  sidebarLayout(
    sidebarPanel(
      checkboxInput("load_test_data", "Load test data", value = FALSE),
      fileInput("dataset1", "Upload First Dataset (.xpt):", accept = c(".xpt")),
      fileInput("dataset2", "Upload Second Dataset (.xpt):", accept = c(".xpt")),
      checkboxInput("unique_keys_check", "Define unique keys for comparison?", value = FALSE),
      uiOutput("validation_message"),
      uiOutput("key_selector_ui"),
      uiOutput("compare_btn_ui"),



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
