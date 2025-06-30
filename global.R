
library(shiny)
library(dplyr)
library(stringr)
library(shinyBS)
library(htmlwidgets)
library(shinyWidgets)

# For testing
# library(pharmaverseadam)
library(tidyr)

# library(shinyBS)
library(DT)
library(htmlTable)

rm(list = ls())


source("utils/generate_test_data.R")


### App version

app_contact_person <- "Stephen Mackie"
app_contact_person_email <- "test"
app_version<- 1.0
app_release_date <- "Today"

knit("info.Rmd", quiet = TRUE)


# df1 <- haven::read_xpt(file.path("data", "df1.xpt"))
# df2 <- haven::read_xpt(file.path("data", "df2.xpt"))

source("utils/compare_datasets_function.R")
source("utils/functions.R")
source("compare_ui.R")



