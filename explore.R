



keys = c("USUBJID", "ID")


c1 <- arsenal::comparedf(df1, df2)

c2 <- arsenal::comparedf(df1, df2, by = keys)


summary(c1)






date() %>% format("%Y")

Sys.Date()

paste0("adpp_comparison_report_",format(Sys.Date(), "%Y_%m_%d"), "T", format(Sys.time(), "%H_%M"))





################################################################
#
# --- Test area
#
################################################################

res <- compareDatasets(df1, df2)


writeLines(res$html_report, "report.html")


################################################################
################################################################




keys <- c("USUBJID", "ID")
res <- compare_row_counts(df1, df2, unique_keys = keys)
miss <- res$missing_rows



# df1$PPTESTCD %>% unique()
# df1$PARAMCD %>% unique()




3/8


dbinom(3,3,0.5)
