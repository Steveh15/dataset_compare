



rounding_check <- function(df1, df2, unique_keys = NULL) {

  dp_list <- list(df1, df2) %>% lapply(function(x)
    x %>%
      rowwise() %>%
      mutate(
        dp = get_decimal_places(AVAL)
      ) %>%
      group_by(PPTESTCD) %>%
      summarise(
        dp = max(dp)
      )
  )


  dp_compare <- merge(dp_list[[1]], dp_list[[2]], by = "PPTESTCD", all = TRUE) %>%
    filter(dp.x != dp.y)

  names(dp_compare) <-c("PPTESTCD", "Dataset 1", "Dataset 2")

  list(
    dp_compare = dp_compare,
    html_output = tags$div(

      tags$h3(
        "Decimal Place Check",
        tags$span(
          id = "info_box",  # ID for the box to apply tooltip
          style = "display: inline-block; border: 1px solid #007bff; padding: 4px 8px; border-radius: 4px; cursor: pointer;",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          # title = "This is your tooltip text",  # Tooltip content
          "\u2139"  # Unicode for the "i" icon
        ),

        bsPopover(
          id = "info_box",  # Matches the ID above
          title = "Rounding Check",
          content = "Explanation of rounding check",
          placement = "right",
          trigger = "hover"
        )
      ),


      tags$p("The following parameters have different maximum decimal places"),

      htmlTable(
        dp_compare,
        rnames = FALSE,
        css.cell = "padding: 5px; text-align: left;", # Cell styles
        css.table = "border: 1px solid black; width: 40%;" # Table styles
      )

    )
  )

}

