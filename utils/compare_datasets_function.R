




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
  results$value_check <- value_check(df1, df2, unique_keys)


  return(results)
}




report_metadata_ui <- function(
    comparison_date,
    comparison_time,
    dataset1_name,
    dataset2_name,
    selected_keys = NULL) {
  tagList(

    tags$table(style = "width: auto; margin-bottom: 1em;", tags$tbody(
      tags$tr(
        tags$th("Generated:", style = "text-align: left; padding-right: 10px;"),

        tags$td(paste0(format(comparison_date, "%d-%b-%Y"), " ", format(comparison_time, "%H:%M")))

      ),
      tags$tr(
        tags$th("Dataset 1:", style = "text-align: left; padding-right: 10px;"),
        tags$td(dataset1_name)
      ),
      tags$tr(
        tags$th("Dataset 2:", style = "text-align: left; padding-right: 10px;"),
        tags$td(dataset2_name)
      ),
      tags$tr(
        tags$th("Unique Keys:", style = "text-align: left; padding-right: 10px;"),
        tags$td(if (!is.null(selected_keys) &&
                    length(selected_keys) > 0) {
          paste(selected_keys, collapse = ", ")
        } else {
          tags$em("No unique keys defined")
        })
      )
    ))
  )
}
