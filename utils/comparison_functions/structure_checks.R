


#
# c1 <- arsenal::comparedf(df1, df2)
#
# # c1 <- arsenal::comparedf(test1_1, test1_2)
#
#
# sum <- summary(c1)
#
# sum$frame.summary.table
#
# sum$comparison.summary.table
#
# vars <- sum$vars.ns.table
#
# types <- sum$vars.nc.table
#
#
#
# types %>% select(var.x, class.x, class.y)
#
#
# types[, c("var.x", "class.x", "class.y")]
#
# vars %>% filter(version == "x")
#
# vars[vars$version == "x",]
#
# paste0(vars[vars$version == "x",]$variable, collapse = ", ")
# paste0(vars[vars$version == "y",]$variable, collapse = ", ")
#
# vars[vars$version == "x",]
#
# sum$frame.summary.table
#
#
# vars <- sum$vars.ns.table
# vars
#
# nrow(vars)

structure_content_check_html <- function(df1, df2, name1 = "Dataset 1", name2 = "Dataset 2") {

  df1_rows <- nrow(df1)
  df2_rows <- nrow(df2)
  row_diff <- abs(df1_rows - df2_rows)

  sum <- summary(arsenal::comparedf(df1, df2))

  col_differences <- sum$vars.ns.table

  col_type_differences <- sum$vars.nc.table[, c("var.x", "class.x", "class.y")]
  names(col_type_differences) <- c("Variable", "ADPP", "ADPP-like")


  rounding_differences <- rounding_check_new(df1,df2, precision = 10)

  html_output <- tagList(

    tags$h3("Differences in number of rows"),

    tags$div(
      tags$p(paste("ADPP has", df1_rows, "rows.")),
      tags$p(paste("ADPP-like has", df2_rows, "rows.")),
      if(row_diff > 0){
        tags$p(style = "font-weight: bold; color: red;",paste("Difference in number of rows:", row_diff))
      } else{
        tags$p(style = "font-weight: bold; color: green;",paste("Difference in number of rows:", row_diff))
      }

    ),

    tags$h3("Differences in columns"),

    if(nrow(col_differences) == 0){
      tags$p(paste("There is no difference in the columns of ADPP and ADPP-like"))
    } else{

      tags$div(
        tags$p(
          tags$b("Columns appearing in ADPP only: "),
          paste0(col_differences[col_differences$version == "x", ]$variable, collapse = ", ")
        ),
        tags$p(
          tags$b("Columns appearing in ADPP-like only: "),
          paste0(col_differences[col_differences$version == "y", ]$variable, collapse = ", ")
        )
      )
    },


    tags$h3("Differences in column types"),

    if(nrow(col_type_differences) == 0){
      tags$p(paste("There are no differences in column types between ADPP and ADPP-like"))
    } else{
      tags$div(
        tags$p(paste("The following columns have different data types between ADPP and ADPP-like")),
        htmlTable(
          col_type_differences,
          rnames = FALSE,
          css.cell = "padding: 5px; text-align: left;", # Cell styles
          css.table = "border: 1px solid black; width: 40%;" # Table styles
        )
      )


    },


    tags$h3("Differences in rounding of AVAL"),

    if(nrow(rounding_differences) == 0){
      tags$p(paste("No differences in the rounding of AVAL has been detected between ADPP and ADPP-like"))
    } else{
      tags$div(
        tags$p(paste("A maximum number of decimal places in AVAL has been detected in the following PARAMCD in the ADPP and ADPP-like datasets")),
        htmlTable(
          rounding_differences,
          rnames = FALSE,
          css.cell = "padding: 5px; text-align: left;", # Cell styles
          css.table = "border: 1px solid black; width: 40%;" # Table styles
        )
      )


    },


  )




  return(html_output)
}
