

value_check  <- function(df1, df2, unique_keys = NULL, tolerance = 1e-3) {


  if (is.null(unique_keys)) {

    return(
      list(
        id = "value_check",
        value_compare = NULL
      )
    )

  } else{

    check1 <- df1 %>% select(all_of(unique_keys), AVAL)
    check2 <- df2 %>% select(all_of(unique_keys), AVAL)

    compare <- check1 %>%
      full_join(check2, by = unique_keys) %>%
      mutate(
        tolcheck = abs(AVAL.x - AVAL.y) > tolerance,
        miss_check = (is.na(AVAL.x) & !is.na(AVAL.y)) | (is.na(AVAL.y) & !is.na(AVAL.x))
      ) %>%
      filter(tolcheck | miss_check)

    # names(dp_compare) <-c("PARAMCD", "Dataset 1", "Dataset 2")

    list(
      id = "value_check",
      value_compare = compare
    )
  }




}


value_check_ui <- function(result, unique_keys = NULL) {

  if(is.null(unique_keys)){
    ui <- tags$div(
      tags$p("Cannot perform check without defined unique keys", style = "color: red;")
    )
  } else{

    ui <- tagList(
          tags$h4("Record comparison"),
          DT::datatable(
            result$value_compare,
            options = list(pageLength = 5),
            class = "display",
            rownames = FALSE,
            width = 500,
            colnames = c(unique_keys, "AVAL DF1", "AVAL DF2", "Value is different", "Value is missing")
          )
        )
  }

  return (ui)


}


value_check_markdown <- function(result, unique_keys = NULL) {
  value_check_ui(result, unique_keys)
}
