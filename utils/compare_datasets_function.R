
rounding_check <- function(df1, df2, precision = 10) {

  dp_list <- list(df1, df2) %>% lapply(function(x)
    x %>%
      rowwise() %>%
      mutate(
        dp = get_decimal_places(AVAL, precision = precision)
      ) %>%
      group_by(PARAMCD) %>%
      summarise(
        dp = max(dp, na.rm = TRUE)
      )
  )

  dp_compare <- merge(dp_list[[1]], dp_list[[2]], by = "PARAMCD", all = TRUE) %>%
    filter(dp.x != dp.y)

  names(dp_compare) <-c("PARAMCD", "ADPP", "ADPP-like")

  return(dp_compare)

}



compareDatasets <- function(df1, df2, unique_keys = NULL, metadata = NULL) {
  results <- list()


  results$adpp <- df1
  results$adpp_like <- df2
  results$unique_keys <- unique_keys
  results$metadata <- metadata



  if(!is.null(unique_keys)){
    compare_summary <- summary(arsenal::comparedf(df1, df2, by = unique_keys))
  } else{
    compare_summary <- summary(arsenal::comparedf(df1, df2))
  }

  # --- Structural Checks
  #############################################################################

  results$structure_checks <- list()

  # --- Difference in number of rows

  df1_rows <- nrow(df1)
  df2_rows <- nrow(df2)
  results$structure_checks$row_difference <- c("df1_rows" =  df1_rows, "df2_rows" = df2_rows, "row_diff" = abs(df1_rows - df2_rows))


  # --- Difference in Columns
  col_differences <- compare_summary$vars.ns.table

  cols_adpp_only <- col_differences[col_differences$version == "x", ]$variable
  cols_adppl_only <- col_differences[col_differences$version == "y", ]$variable
  results$structure_checks$col_difference <- list("n_differences" = length(cols_adpp_only) + length(cols_adppl_only), "adpp_only" = cols_adpp_only, "adppl_only" = cols_adppl_only)


  # --- Difference in Columns Types

  col_type_differences <- compare_summary$vars.nc.table[, c("var.x", "class.x", "class.y")]
  names(col_type_differences) <- c("Variable", "ADPP", "ADPP-like")
  results$structure_checks$col_type_difference <- col_type_differences


  # --- Difference in rounding of AVAL
  results$structure_checks$rounding_differences <- rounding_check(df1,df2, precision = 10)


  # --- Row level checks
  #############################################################################


  if(!is.null(unique_keys)){

    row_level_checks <- list()

    # --- Mismatched rows check

    mismatched_rows <- compare_summary$obs.table %>% as_tibble() %>%
      mutate(
        version = if_else(version == "x", "ADPP", "ADPP-like")
      )
    names(mismatched_rows) <- c("ADPP_or_ADPPL", unique_keys, "row_n")

    row_level_checks$mismatched_rows <- mismatched_rows


    # --- AVAL differences check

    aval_diffs_1 <- compare_summary$diffs.table %>%
      filter(var.x == "AVAL") %>%
      unnest(
        cols = c(values.x, values.y)
      ) %>%
      mutate(

        diff = abs(values.x - values.y),
        tol1 = diff >= 1e-3,
        tol2 = diff >= 1e-6,
        tol3 = diff >= 1e-9,
        tol4 = diff < 1e-9,

        tol_na =  (is.na(values.x) & !is.na(values.y) ) | (!is.na(values.x) & is.na(values.y) ),

        tol0 = any(c(tol_na, tol1, tol2, tol3), na.rm = TRUE),

        tol = case_when(
          tol_na ~ "Missing",
          tol1 ~ ">= 1e-3",
          !tol1 & tol2  ~ ">= 1e-6",
          !tol2 & tol3 ~ ">= 1e-9",
          !tol3 & tol4 ~ "< 1e-9",

        )

      ) %>%
      filter(tol1 | tol2 | tol3 | tol_na)


    aval_summary <- aval_diffs_1 %>%
      summarise(
        tol0_sum = sum(tol0, na.rm = TRUE),
        tol1_sum = sum(tol1, na.rm = TRUE),
        tol2_sum = sum(tol2 & !tol1, na.rm = TRUE),
        tol3_sum = sum(tol3 & !tol2, na.rm = TRUE),
        tol4_sum = sum(tol4 & !tol3, na.rm = TRUE),
        tolna_sum = sum(tol_na, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything()) %>%
      mutate(
        name = case_match(
          name,
          "tol0_sum"   ~ "Total",
          "tol1_sum"   ~ "∆ ≥ 1e-3",
          "tol2_sum"   ~ "1e-3 > ∆ ≥ 1e-6",
          "tol3_sum"   ~ "1e-6 > ∆ ≥ 1e-9",
          "tol4_sum"   ~ "∆ < 1e-9",
          "tolna_sum"  ~ "AVAL missing in one dataset"
        ),
        value = as.character(value)
      )


    aval_diffs <- aval_diffs_1 %>%
      mutate(
        aval_adpp = format(values.x, digits = 9, nsmall = 3) %>% trimws(),
        aval_adppl = format(values.y, digits = 9, nsmall = 3) %>% trimws(),
        across(c("aval_adpp", "aval_adppl"), ~if_else(. == "NA", ".", .))
      ) %>%
      select(all_of(unique_keys), aval_adpp, aval_adppl, tol)

    row_level_checks$aval_comparison <- list()

    row_level_checks$aval_comparison$aval_summary <- aval_summary
    row_level_checks$aval_comparison$aval_diffs <- aval_diffs

    # --- All Other Variables Comparison

    other_vars_comparison <- list()

    other_vars_comparison$all_diffs <- compare_summary$diffs.table %>%
      filter(var.x != "AVAL") %>%
      mutate(
        across( c("values.x", "values.y"),~ if_else(is.na(.), NA, as.character(.)))
      ) %>%
      select(all_of(unique_keys), var = var.x, adpp = values.x, adppl = values.y) %>%
      as_tibble()

    other_vars_comparison$unique_diffs <- other_vars_comparison$all_diffs %>%
      distinct(var, adpp, adppl) %>%
      as_tibble()


    row_level_checks$other_vars_comparison <- other_vars_comparison


    # --- Combine into results object
    results$row_level_checks <- row_level_checks
  } else{
    results$row_level_checks <- NULL
  }



  # --- Return
  #############################################################################


  return(results)

}


#############################################################################
# --- Metadata UI
#############################################################################
#############################################################################


report_metadata_ui <- function(
    metadata,
    unique_keys = NULL
    # comparison_datetime,
    # dataset1_name,
    # dataset2_name,
) {
  tagList(

    tags$table(style = "width: auto; margin-bottom: 1em;", tags$tbody(
      tags$tr(
        tags$th("Generated:", style = "text-align: left; padding-right: 10px;"),

        tags$td(paste0(format(metadata$datetime, "%d-%b-%Y"), " ", format(metadata$datetime, "%H:%M")))

      ),
      tags$tr(
        tags$th("Dataset 1:", style = "text-align: left; padding-right: 10px;"),
        tags$td(paste0(metadata$path1, "/", metadata$name1))
      ),
      tags$tr(
        tags$th("Dataset 2:", style = "text-align: left; padding-right: 10px;"),
        tags$td(paste0(metadata$path1, "/", metadata$name1))
      ),
      tags$tr(
        tags$th("Unique Keys:", style = "text-align: left; padding-right: 10px;"),
        tags$td(if (!is.null(unique_keys) &&
                    length(unique_keys) > 0) {
          paste(unique_keys, collapse = ", ")
        } else {
          tags$em("No unique keys defined")
        })
      )
    ))
  )
}



#############################################################################
# --- Structure check UI
#############################################################################
#############################################################################


structure_content_check_html <- function(structure_checks) {


  html_output <- tagList(

    tags$h3("Differences in number of rows"),

    tags$div(
      tags$p(paste("ADPP has", structure_checks$row_difference["df1_rows"], "rows.")),
      tags$p(paste("ADPP-like has", structure_checks$row_difference["df2_rows"], "rows.")),
      if(structure_checks$row_difference["row_diff"] > 0){
        tags$p(style = "font-weight: bold;",paste("Difference in number of rows:", structure_checks$row_difference["row_diff"]))
      } else{
        tags$p(style = "font-weight: bold;",paste("Difference in number of rows:", structure_checks$row_difference["row_diff"]))
      }

    ),

    tags$h3("Differences in columns"),

    if(structure_checks$col_difference$n_differences == 0){
      tags$p(paste("There is no difference in the columns of ADPP and ADPP-like"))
    } else{

      tags$div(
        tags$p(
          tags$b("Columns appearing in ADPP only: "),
          paste0(structure_checks$col_difference$adpp_only, collapse = ", ")
        ),
        tags$p(
          tags$b("Columns appearing in ADPP-like only: "),
          paste0(structure_checks$col_difference$adppl_only, collapse = ", ")
        )
      )
    },


    tags$h3("Differences in column types"),

    if(nrow(structure_checks$col_type_difference) == 0){
      tags$p(paste("There are no differences in column types between ADPP and ADPP-like"))
    } else{
      tags$div(
        tags$p(paste("The following columns have different data types between ADPP and ADPP-like")),
        htmlTable(
          structure_checks$col_type_difference,
          rnames = FALSE,
          css.cell = "padding: 5px; text-align: left;", # Cell styles
          css.table = "border: 1px solid black; width: 40%;" # Table styles
        )
      )


    },

    tags$h3("Differences in rounding of AVAL"),

    if(nrow(structure_checks$rounding_differences) == 0){
      tags$p(paste("No differences in the rounding of AVAL has been detected between ADPP and ADPP-like"))
    } else{
      tags$div(
        tags$p(paste("A maximum number of decimal places in AVAL has been detected in the following PARAMCD in the ADPP and ADPP-like datasets")),
        htmlTable(
          structure_checks$rounding_differences,
          rnames = FALSE,
          css.cell = "padding: 5px; text-align: left;", # Cell styles
          css.table = "border: 1px solid black; width: 40%;" # Table styles
        )
      )
    }
  )

  return(html_output)

}

#############################################################################
# --- row level check UI
#############################################################################
#############################################################################



row_level_check_html <- function(row_level_checks, unique_keys = NULL, markdown = FALSE){

  if(is.null(unique_keys)){

    html_output <- tags$p("Unique keys have not been defined. No row-level checks have been performed.")
  } else{

    html_output <- tagList(
      tags$p("Unique keys have been defined."),

      tags$h3("Unmatched Records"),

      if(nrow(row_level_checks$mismatched_rows) == 0){
        tags$p(paste("There are no mismatched records in the ADPP and ADPP-like datasets."))
      } else{
        tags$div(
          tags$p(paste("There are records which appear in only ADPP or ADPP-like but not both.")),
          DT::datatable(
            row_level_checks$mismatched_rows,
            options = list(pageLength = 5),
            class = "display",
            rownames = FALSE,
            width = 500,
            colnames = c("ADPP or ADPP-like", unique_keys, "Row number")
          )
        )
      },


      tags$h2("AVAL comparison"),

      if(nrow(row_level_checks$aval_comparison$aval_diffs) == 0){
        tags$p("No differences were detected in AVAL between ADPP or ADPP-like.")
      } else{
        tags$div(
          tags$p("Differences were detected in AVAL between ADPP or ADPP-like."),
          tags$h3("AVAL differences summary"),
          htmlTable(
            row_level_checks$aval_comparison$aval_summary,
            rnames = FALSE,
            css.cell = "padding: 5px; text-align: left;", # Cell styles
            css.table = "border: 1px solid black; width: 40%;" # Table styles
          ),
          tags$h3("AVAL differences table"),
          DT::datatable(
            row_level_checks$aval_comparison$aval_diffs,
            options = list(pageLength = 5),
            class = "display",
            rownames = FALSE,
            width = 500,
            colnames = c(unique_keys, "AVAL ADPP", "AVAL ADPP-like", "Difference detected")
          )

        )
      },


      if(!markdown){

        tags$div(
          tags$h2("All Other Variables Comparison"),

          if(nrow(row_level_checks$other_vars_comparison$all_diffs) == 0){
            tags$p("No differences were detected in AVAL between ADPP or ADPP-like.")
          } else{
            tabsetPanel(
              tabPanel("All Differences",
                       DT::datatable(
                         row_level_checks$other_vars_comparison$all_diffs,
                         options = list(pageLength = 5),
                         class = "display",
                         rownames = FALSE,
                         width = 500,
                         colnames = c(unique_keys, "Variable name", "ADPP", "ADPP-like")
                       )),
              tabPanel("Distinct Difference Only",
                       DT::datatable(
                         row_level_checks$other_vars_comparison$unique_diffs,
                         options = list(pageLength = 5),
                         class = "display",
                         rownames = FALSE,
                         width = 500,
                         colnames = c("Variable name", "ADPP", "ADPP-like")
                       ))
            )
          }
        )

      }


    )
  }




  return(html_output)

}
