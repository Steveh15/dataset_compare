
library(htmltools)

compare_row_counts <- function(df1, df2, unique_keys = NULL) {


  df1_rows <- nrow(df1)
  df2_rows <- nrow(df2)
  row_diff <- nrow(df1) - nrow(df2)

  missing_rows <- NULL
  html_output <- NULL

  if(!is.null(unique_keys)){
    d1 <- df1[, keys]
    d2 <- df2[, keys]
    d1$res <- TRUE
    d2$res <- TRUE

    missing_rows <- merge(d1, d2, by = keys, all = TRUE)
    missing_rows <- missing_rows[is.na(missing_rows$res.x) | is.na(missing_rows$res.y), ]

    dt_widget <- datatable(
      missing_rows,
      options = list(pageLength = 5),
      class = "display",
      rownames = FALSE,
      width = 500,
      colnames = c(unique_keys, "Present in DF1", "Present in DF2")
    )

    # Convert the widget to raw HTML
    html_output <- as.tags(dt_widget)

    # Return the HTML content
    # HTML(html_output)

  }


  return_list <-
    list(
      id = "compare_row_counts",
      df1_rows = df1_rows,
      df2_rows = df2_rows,
      row_diff = row_diff,
      missing_rows = missing_rows,
      html_output = tags$div(
        tags$h3("Number of Rows Comparison"),
        tags$p(paste("Dataset 1 has", df1_rows, "rows.")),
        tags$p(paste("Dataset 2 has", df2_rows, "rows.")),
        tags$p(paste("Difference in number of rows:", row_diff)),
        tags$p(paste(unique_keys, collapse = ", ")),
        html_output
      )


    )
}
#
# keys <- c("USUBJID", "ID")
# res <- compare_row_counts(df1, df2, unique_keys = keys)
# miss <- res$missing_rows
#
#
# d1 <- df1[, keys]
# d2 <- df2[, keys]
# d1$res <- 1
# d2$res <- 1
#
# missing_rows <- merge(d1, d2, by = keys, all = TRUE)
#
# missing_rows_1 <- missing_rows[is.na(missing_rows$res.x) | is.na(missing_rows$res.y), ]
# test <- missing_rows %>% filter(is.na(res.y) | is.na(res.x))
#
# missing_rows %>% filter(is.na(USUBJID))
# is.na(missing_rows$res.x) | is.na(missing_rows$res.y)
#
#
# ?datatable()

