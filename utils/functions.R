


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





# --- NOTE: so as to not pick up floating point errors this function only considers the first 10 decimal places
get_decimal_places <- function(x, precision = 10) {
  sapply(x, function(val) {
    if (is.na(val)) return(NA_integer_)

    # Round to 10 decimal places
    val_rounded <- round(val, precision)

    # Convert to character string with fixed 10 dp (no scientific notation)
    str <- formatC(val_rounded, format = "f", digits = precision)

    # Extract decimal part and trim trailing zeros
    dec_part <- strsplit(str, "\\.")[[1]][2]
    dec_trimmed <- sub("0+$", "", dec_part)

    nchar(dec_trimmed)
  })
}




