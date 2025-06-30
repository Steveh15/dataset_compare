


unique_keys <- c("USUBJID", "ID")

metadata<- list(
  path1 = "path1",
  path2 = "path2",
  name1 = "name1.xpt",
  name2 = "name2.xpt",
  datetime = Sys.time()
)

res <- compareDatasets(df1, df2, unique_keys, metadata = metadata)


comments <- list()
comments$structure_and_content = ""
comments$row_level = ""

res
temp_report <- tempfile(fileext = ".html")


rmarkdown::render(
  input = "report_template.Rmd",
  output_file = temp_report,
  params = list(
    comparison_result = res,
    comments = comments


  ),
  envir = new.env(parent = globalenv())  # Prevents polluting global env
)


browseURL(temp_report)
