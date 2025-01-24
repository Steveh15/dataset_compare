


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
      tags$h3("Decimal Place Check"),
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

# rounding_check(df1, df2)

#
# get_decimal_places <- function(x) {
#
#   num_str <- as.character(x)
#
#   sapply(strsplit(num_str, "\\."), function(x){
#     if(length(x) > 1){
#       return(nchar(gsub("0+$", "", x[2])))
#     } else{
#       return(0)
#     }
#   })
#
# }
#
#
#
#
#
# dp_list <- list(df1, df2) %>% lapply(function(x)
#   x %>%
#     rowwise() %>%
#     mutate(
#       dp = get_decimal_places(AVAL)
#     ) %>%
#     group_by(PPTESTCD) %>%
#     summarise(
#       dp = max(dp)
#     )
# )
#
#
# dp_compare <- merge(dp_list[[1]], dp_list[[2]], by = "PPTESTCD", all = TRUE) %>%
#   filter(dp.x != dp.y)
#
#
# dp_compare
# # ?merge
# #
# # df1
# #
# # list_res[[1]]
# #
# # get_decimal_places("Hello")
# #
# #
# # grepl("12322", "\\.")
# #
# #
# #
# # as.character(x)
# #
# # get_decimal_places(x)
# # get_decimal_places(0.001)
#
#
# ####################################
# ###################################
# # TEST AREA
#
#
# x <- c(1, 1, 1, 1.1)
#
#
# x <- c(1, 1.1, 1.12, 1.123, 1.12345, 1.123456789, 1.0000000000001)
#
# # num_str <- format(x, scientific = FALSE, trim = TRUE, digits = 12)
#
# num_str <- as.character(x)
#
# num_str
#
# # num_str <- format(x, scientific = FALSE, trim = TRUE)
# # Split the string by the decimal point, retaining the part after the point
#
# # sapply(strsplit(num_str, "\\."), function(x){
# # if(length(x) > 1){
# #   return(nchar(gsub("0+$", "", x[2])))
# # } else{
# #   return(0)
# # }
# # })
# #
# # parts <- sapply(strsplit(num_str, "\\."), function(x) x[2])
# # parts
# #
# # res <- sapply(parts, function(x) nchar(gsub("0+$", "", x)))
# # names(res) <- NULL
# #
# #
# #
# # print(res)
# ####################################
# ###################################
