




files <- list.files("utils/comparison_functions", full.names = TRUE, pattern = "\\.R$")
sapply(files, source)



generate_html_content <- function(...) {
  html_content <- paste0(
    "<html><head><title>Comparison Report</title></head><body>",
    "<h1>Dataset Comparison Report</h1>",
    paste0("<h4>Generated: ", date(), "</h4>"),
    paste(..., collapse = "<br>"),
    "</body></html>"
  )
  return(html_content)
}


compareDatasets <- function(df1, df2, unique_keys = NULL) {
  results <- list()
  results$column_diff <- compare_columns(df1, df2, unique_keys)
  results$row_count_diff <- compare_row_counts(df1, df2, unique_keys)
  results$rounding_check <- rounding_check(df1, df2)

  results$html_report <- generate_html_content(
    results$column_diff$html_output,
    results$row_count_diff$html_output,
    results$rounding_check$html_output
  )

  return(results)
}
