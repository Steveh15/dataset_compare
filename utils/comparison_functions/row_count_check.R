


row_count_check <- function(df1, df2, unique_keys = NULL) {
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

row_count_check_ui <- function(result, unique_keys = NULL) {
  summary_block <- tags$div(
    tags$p(paste("Dataset 1 has", result$df1_rows, "rows.")),
    tags$p(paste("Dataset 2 has", result$df2_rows, "rows.")),
    tags$p(paste("Difference in number of rows:", result$row_diff))
  )

  if (!is.null(result$missing_rows)) {
    table_block <- tagList(
      tags$h4("Record comparison"),
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

row_count_check_markdown <- function(result, unique_keys = NULL) {
  row_count_check_ui(result, unique_keys)
}

