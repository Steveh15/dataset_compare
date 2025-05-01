




files <- list.files("utils/comparison_functions", full.names = TRUE, pattern = "\\.R$")
sapply(files, source)


generate_html_content <- function(...) {
  htmltools::tagList(
    tags$html(
      tags$head(
        tags$title("Comparison Report")
      ),
      tags$body(
        tags$h1("Dataset Comparison Report"),
        tags$h4(paste("Generated:", date())),
        ...
      )
    )
  )
}


compareDatasets <- function(df1, df2, unique_keys = NULL) {
  results <- list()


  results$row_count_check <- row_count_check(df1, df2, unique_keys)
  results$column_count_check <- column_count_check(df1, df2, unique_keys)
  results$rounding_check <- rounding_check(df1, df2)


  return(results)
}
