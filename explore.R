



check_keys <- function(df1, keys = c("USUBJID")) {
  # Check if the dataset is a data.frame
  if (!is.data.frame(df1)) {
    stop("Input must be a data.frame.")
  }
  # Check if keys is a character vector
  if (!is.character(keys)) {
    stop("Keys must be a character vector.")
  }
  # Check if the specified keys exist in the dataset
  missing_keys <- setdiff(keys, colnames(df1))
  if (length(missing_keys) > 0) {
    stop(paste("The following keys are missing from the dataset:", paste(missing_keys, collapse = ", ")))
  }

  # Count occurrences of unique combinations of the keys
  counts <- as.data.frame(table(df1[keys]))$Freq

  # Check if any count is greater than 1
  if (any(counts > 1)) {
    return(FALSE) # Keys do not define unique rows
  }

  return(TRUE) # Keys define unique rows
}


check_keys(1234)
check_keys(df1, keys = df2)
check_keys(df1, keys = "Pie")

check_keys(df1)
check_keys(df1, keys = c("USUBJID", "ID"))







compare_keys <- function(df1, df2, keys = "USUBJID"){

  if (!is.data.frame(df1)) {
    stop("df1 must be a data.frame.")
  }
  if (!is.data.frame(df2)) {
    stop("df2 must be a data.frame.")
  }
  if (!is.character(keys)) {
    stop("Keys must be a character vector.")
  }
  check_df1 <- check_keys(df1, keys)
  check_df2 <- check_keys(df2, keys)


  df1miss <- NA
  df2miss <- NA

  list(df1_unique = check_df1, df2_unique = check_df2, df1_missing = df1miss, df2_missing = df2miss)

}


compare_keys(df1, df2, keys = c("USUBJID", "ID"))

compare_keys(1234, df2)
compare_keys(df1, TRUE)
compare_keys(df1, df2, TRUE)


compare_keys(df1, df2)

compare_keys(df1)

keys = c("USUBJID", "ID")
a <- df1 %>% select(any_of(keys))
b <- df2 %>% select(any_of(keys))

# Composite keys
a$composite_key <- do.call(paste, c(a[keys], sep = "_"))
b$composite_key <- do.call(paste, c(b[keys], sep = "_"))

keys_a_not_in_b <- sum(!a$composite_key %in% b$composite_key)
keys_b_not_in_a <- sum(!b$composite_key %in% a$composite_key)

keys_a_not_in_b
keys_b_not_in_a





library(htmlTable)

compare_list <- compareDatasets(df1, df2)


print(compare_list)





columns1 <- data.frame(name = colnames(df1), type = sapply(df1, get_class))
columns2 <- data.frame(name = colnames(df2), type = sapply(df2, get_class))


column_compare <- merge(columns1, columns2, by = "name")
res <- column_compare[column_compare$type.x != column_compare$type.y,]


colnames(res) <- c("Custom Header 1", "Custom Header 2", "Custom Header 3")

# Convert to HTML table
html_output <- htmlTable(res)
html_output




custom_headers <- c("Variable", "Dataset 1", "Dataset 2")

# Create the HTML table
html_output <- tags$table(
  tags$thead(
    tags$tr(lapply(custom_headers, tags$th))
  ),
  tags$tbody(
    lapply(seq_len(nrow(res)), function(i) {
      tags$tr(lapply(res[i, ], function(x) tags$td(x)))
    })
  )
)

# Render as HTML
html_output

