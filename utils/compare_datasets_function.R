

compare_row_counts <- function(df1, df2) {


  df1_rows <- nrow(df1)
  df2_rows <- nrow(df2)
  row_diff <- nrow(df1) - nrow(df2)

  return_list <-
  list(
    id = "compare_row_counts",
    df1_rows = df1_rows,
    df2_rows = df2_rows,
    row_diff = row_diff,
    html_output = tags$div(
      tags$h3("Number of Rows Comparison"),
      tags$p(paste("Dataset 1 has", df1_rows, "rows.")),
      tags$p(paste("Dataset 2 has", df2_rows, "rows.")),
      tags$p(paste("Difference in number of rows:", row_diff))
    )


    )
}







compare_columns <- function(df1, df2) {

  columns1 <- data.frame(name = colnames(df1), type = sapply(df1, get_class))
  columns2 <- data.frame(name = colnames(df2), type = sapply(df2, get_class))

  in_df1_not_df2 <- setdiff(columns1$name, columns2$name)
  in_df2_not_df1 <- setdiff(columns2$name, columns1$name)

  column_compare <- merge(columns1, columns2, by = "name")
  type_mismatches <- column_compare[column_compare$type.x != column_compare$type.y,]


  diff <- list(
    id = "compare_column_vars",
    in_df1_not_df2 = in_df1_not_df2,
    in_df2_not_df1 = in_df2_not_df1,
    # type_mismatches = type_mismatches,

    html_output = tags$div(
      tags$h3("Number of Columns comparison"),
      tags$p(paste("Columns in Dataset 1 only: ", paste0(in_df1_not_df2, collapse = ", "))),
      tags$p(paste("Columns in Dataset 2 only: ", paste0(in_df2_not_df1, collapse = ", "))),
      tags$table(
        tags$thead(
          tags$tr(lapply(c("Variable", "Dataset 1", "Dataset 2"), tags$th))
        ),
        tags$tbody(
          lapply(seq_len(nrow(res)), function(i) {
            tags$tr(lapply(res[i, ], function(x) tags$td(x)))
          })
        )
      )
    )
  )
  return(diff)
}


compareDatasets <- function(df1, df2) {
  results <- list()
  results$column_diff <- compare_columns(df1, df2)
  results$row_count_diff <- compare_row_counts(df1, df2)
  return(results)
}

# Source the comparison functions
# files <- list.files("R/comparison_functions", full.names = TRUE, pattern = "\\.R$")
# sapply(files, source)
