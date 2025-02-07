
library(shiny)
library(dplyr)
library(stringr)
library(shinyBS)

# For testing
library(pharmaverseadam)

# library(shinyBS)
library(DT)
library(htmlTable)


source("utils/generate_test_data.R")

# df1 <- haven::read_xpt(file.path("data", "df1.xpt"))
# df2 <- haven::read_xpt(file.path("data", "df2.xpt"))

source("utils/compare_datasets_function.R")
source("utils/functions.R")
source("compare_ui.R")
