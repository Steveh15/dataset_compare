
library(htmltools)

compare_row_counts <- function(df1, df2, unique_keys = NULL) {
  df1_rows <- nrow(df1)
  df2_rows <- nrow(df2)
  row_diff <- df1_rows - df2_rows

  missing_rows <- NULL

  if (!is.null(unique_keys)) {
    d1 <- df1[, unique_keys, drop = FALSE]
    d2 <- df2[, unique_keys, drop = FALSE]
    d1$res <- TRUE
    d2$res <- TRUE

    missing_rows <- merge(d1, d2, by = unique_keys, all = TRUE)
    missing_rows <- missing_rows[is.na(missing_rows$res.x) | is.na(missing_rows$res.y), ]
  }

  return(list(
    id = "compare_row_counts",
    df1_rows = df1_rows,
    df2_rows = df2_rows,
    row_diff = row_diff,
    missing_rows = missing_rows
  ))
}

render_row_count_ui <- function(result, unique_keys = NULL) {
  summary_block <- tags$div(
    tags$h3("Number of Rows Comparison"),
    tags$p(paste("Dataset 1 has", result$df1_rows, "rows.")),
    tags$p(paste("Dataset 2 has", result$df2_rows, "rows.")),
    tags$p(paste("Difference in number of rows:", result$row_diff))
  )

  if (!is.null(result$missing_rows)) {
    table_block <- tagList(
      tags$h3("Record comparison"),
      DT::datatable(
        result$missing_rows,
        options = list(pageLength = 5),
        class = "display",
        rownames = FALSE,
        width = 500,
        colnames = c(unique_keys, "Present in DF1", "Present in DF2")
      )
    )
  } else {
    table_block <- NULL
  }

  tagList(summary_block, table_block)
}

render_row_count_report <- function(result) {
  summary_block <- htmltools::tagList(
    tags$h2("Number of Rows Comparison"),
    tags$p(paste("Dataset 1 has", result$df1_rows, "rows.")),
    tags$p(paste("Dataset 2 has", result$df2_rows, "rows.")),
    tags$p(paste("Difference in number of rows:", result$row_diff))
  )

  if (!is.null(result$missing_rows)) {
    table_block <- htmltools::tagList(
      tags$h3("Record comparison"),
      knitr::kable(result$missing_rows, format = "html")  # safe for .Rmd
    )
  } else {
    table_block <- NULL
  }

  tagList(summary_block, table_block)
}

# compare_row_counts <- function(df1, df2, unique_keys = NULL) {
#
#
#   df1_rows <- nrow(df1)
#   df2_rows <- nrow(df2)
#   row_diff <- nrow(df1) - nrow(df2)
#
#   missing_rows <- NULL
#   html_output <- NULL
#
#   if(!is.null(unique_keys)){
#     d1 <- df1[, unique_keys]
#     d2 <- df2[, unique_keys]
#     d1$res <- TRUE
#     d2$res <- TRUE
#
#     missing_rows <- merge(d1, d2, by = unique_keys, all = TRUE)
#     missing_rows <- missing_rows[is.na(missing_rows$res.x) | is.na(missing_rows$res.y), ]
#
#     dt_widget <- datatable(
#       missing_rows,
#       options = list(pageLength = 5),
#       class = "display",
#       rownames = FALSE,
#       width = 500,
#       colnames = c(unique_keys, "Present in DF1", "Present in DF2")
#     )
#
#     # Convert the widget to raw HTML
#     html_output <- tagList(
#       tags$h3("Record comparison"),  # Add a heading
#       as.tags(dt_widget)  # Include the widget
#     )
#
#     # Return the HTML content
#     # HTML(html_output)
#
#   }
#
#
#   return_list <-
#     list(
#       id = "compare_row_counts",
#       df1_rows = df1_rows,
#       df2_rows = df2_rows,
#       row_diff = row_diff,
#       missing_rows = missing_rows,
#       html_output = tags$div(
#         tags$h3("Number of Rows Comparison"),
#         tags$p(paste("Dataset 1 has", df1_rows, "rows.")),
#         tags$p(paste("Dataset 2 has", df2_rows, "rows.")),
#         tags$p(paste("Difference in number of rows:", row_diff)),
#         html_output
#       )
#
#
#     )
# }
#
