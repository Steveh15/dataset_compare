
compare_columns <- function(df1, df2, unique_keys = NULL) {

  columns1 <- data.frame(name = colnames(df1), type = sapply(df1, get_class))
  columns2 <- data.frame(name = colnames(df2), type = sapply(df2, get_class))

  in_df1_not_df2 <- setdiff(columns1$name, columns2$name)
  in_df2_not_df1 <- setdiff(columns2$name, columns1$name)

  column_compare <- merge(columns1, columns2, by = "name")
  type_mismatches <- column_compare[column_compare$type.x != column_compare$type.y,]

  names(type_mismatches) <-c("Variable", "Dataset 1", "Dataset 2")

  diff <- list(
    id = "compare_column_vars",
    in_df1_not_df2 = in_df1_not_df2,
    in_df2_not_df1 = in_df2_not_df1,
    type_mismatches = type_mismatches,

    html_output = tags$div(
      tags$h3("Number of Columns comparison"),
      tags$p(paste("Columns in Dataset 1 only: ", paste0(in_df1_not_df2, collapse = ", "))),
      tags$p(paste("Columns in Dataset 2 only: ", paste0(in_df2_not_df1, collapse = ", "))),
      tags$h4("Column type comparison"),
      htmlTable(
        type_mismatches,
        rnames = FALSE,
        css.cell = "padding: 5px; text-align: left;", # Cell styles
        css.table = "border: 1px solid black; width: 40%;" # Table styles
      )
    )
  )


  return(diff)
}
