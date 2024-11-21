

compare_row_counts <- function(df1, df2) {
  list(id = "compare_row_counts", df1_rows = nrow(df1), df2_rows = nrow(df2), row_diff = nrow(df1) - nrow(df2))
}


compare_columns <- function(df1, df2) {
  columns1 <- data.frame(name = colnames(df1), type = sapply(df1, get_class))
  columns2 <- data.frame(name = colnames(df2), type = sapply(df2, get_class))

  diff <- list(
    id = "compare_column_vars",
    in_df1_not_df2 = setdiff(columns1$name, columns2$name),
    in_df2_not_df1 = setdiff(columns2$name, columns1$name),
    type_mismatches = columns1[columns1$name %in% columns2$name &
                                 columns1$type != columns2[match(columns1$name, columns2$name), "type"], ]
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
