



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
