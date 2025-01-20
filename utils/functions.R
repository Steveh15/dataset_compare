


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
  # Convert the number to a character string with significant digits
  num_str <- format(x, scientific = FALSE, trim = TRUE, digits = 10)

  # Split the string by the decimal point
  parts <- strsplit(num_str, "\\.")[[1]]

  # Check if there are digits after the decimal point
  if (length(parts) > 1) {
    return(nchar(gsub("0+$", "", parts[2])))  # Remove trailing zeros and count remaining digits
  } else {
    return(0)  # No decimal places
  }
}
