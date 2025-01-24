

library(htmlTable)


files <- list.files("utils/comparison_functions", full.names = TRUE, pattern = "\\.R$")
sapply(files, source)

compareDatasets <- function(df1, df2, unique_keys = NULL) {
  results <- list()
  results$column_diff <- compare_columns(df1, df2, unique_keys)
  results$row_count_diff <- compare_row_counts(df1, df2, unique_keys)
  results$rounding_check <- rounding_check(df1, df2)
  return(results)
}
