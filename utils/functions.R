


get_class <- function(x) {
  if (inherits(x, "Date")) {
    return("Date")  # or other custom behavior
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return("POSIXct")  # or other custom behavior
  } else {
    return(class(x))  # default behavior for other types
  }
}



check_keys <- function(df1, keys = c("USUBJID")) {
  # Check if the dataset is a data.frame
  if (!is.data.frame(df1)) {
    stop("Input must be a data.frame.")
  }
  # Check if keys is a character vector
  if (!is.character(keys)) {
    stop("Keys must be a character vector.")
  }
  # Check if the specified keys exist in the dataset
  missing_keys <- setdiff(keys, colnames(df1))
  if (length(missing_keys) > 0) {
    stop(paste("The following keys are missing from the dataset:", paste(missing_keys, collapse = ", ")))
  }

  # Count occurrences of unique combinations of the keys
  counts <- as.data.frame(table(df1[keys]))$Freq

  # Check if any count is greater than 1
  if (any(counts > 1)) {
    return(FALSE) # Keys do not define unique rows
  }

  return(TRUE) # Keys define unique rows
}




get_decimal_places <- function(x) {

  num_str <- as.character(x)

  sapply(strsplit(num_str, "\\."), function(x){
    if(length(x) > 1){
      return(nchar(gsub("0+$", "", x[2])))
    } else{
      return(0)
    }
  })


}
