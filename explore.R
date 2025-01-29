



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
