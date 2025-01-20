


rounding_check <- function(df1, df2, unique_keys = NULL) {

}





d1 <- c(0.1, 0.12, 0.123, 0.1234)

d2 <- c(0.1, 0.12, 0.123, 0.1234001)


all.equal(d1, d2)



get_decimal_places <- function(x) {
  if (x %% 1 == 0) {
    # If the number is an integer, it has 0 decimal places
    return(0)
  } else {
    # Count the number of digits after the decimal point
    decimal_part <- sub("^[^\\.]*\\.", "", format(x, scientific = FALSE))
    return(nchar(decimal_part))
  }
}

# Examples:
get_decimal_places(1.1)       # Output: 1
get_decimal_places(1.12)      # Output: 2
get_decimal_places(1.00001)   # Output: 5
get_decimal_places(1.000001)   # Output: 6
get_decimal_places(1.0000001)   # Output: 5
get_decimal_places(42)        # Output: 0



get_decimal_places(42.00000000000001)        # Output: 0


format(42.00000000000001, scientific = FALSE)

sub("^[^\\.]*\\.", "", format(42.00000000000000001, scientific = FALSE))



 x <- 42.00000000000001


 if (x %% 1 == 0) {
   # If the number is an integer, it has 0 decimal places
   print("Is an Integer")
   res <- 0
   print(res)
 } else {
   # Count the number of digits after the decimal point
   decimal_part <- sub("^[^\\.]*\\.", "", format(x, scientific = FALSE))
   res <- nchar(decimal_part)

   print("Is not an integer")
   print(res)
 }

 ?g

 x <- 42.000001

char <- format(x, scientific = FALSE)
print(char)




data.frame(x = c(42.00000000000000001, 42.00000000000000001, 42.000001))

?grep

 grep("\\.", "42")













 get_decimal_places <- function(x) {
   # Convert the number to a character string with significant digits
   num_str <- format(x, scientific = FALSE, trim = TRUE)

   # Split the string by the decimal point
   parts <- strsplit(num_str, "\\.")[[1]]

   # Check if there are digits after the decimal point
   if (length(parts) > 1) {
     return(nchar(gsub("0+$", "", parts[2])))  # Remove trailing zeros and count remaining digits
   } else {
     return(0)  # No decimal places
   }
 }



 get_decimal_places(1.0000001)  # Returns 7
 get_decimal_places(1.1)        # Returns 1
 get_decimal_places(1.12)       # Returns 2
 get_decimal_places(1.0000100)    # Returns 5
 get_decimal_places(1)          # Returns 0

 num_str <- format(1.00001, scientific = FALSE, trim = TRUE)
 strsplit(num_str, "\\.")

 ?gsub



 library(dplyr)





 get_decimal_places_2 <- function(x) {
   if (x %% 1 == 0) {
     # If the number is an integer, it has 0 decimal places
     return(0)
   } else {
     # Count the number of digits after the decimal point
     decimal_part <- sub("^[^\\.]*\\.", "", format(x, scientific = FALSE))
     return(nchar(decimal_part))
   }
 }



d1 <- df1 %>% as_tibble()


test <-  d1 %>%
  select(USUBJID, PARAMCD,PPTESTCD, AVAL) %>%
  rowwise() %>%
  mutate(
    dp = get_decimal_places(AVAL),
    dp2 = get_decimal_places_2(AVAL)
  )


test %>%
  group_by(PARAMCD) %>%
  summarise(
    mindp = min(dp),
    maxdp = max(dp)
  )
  # count(PARAMCD, dp)



get_decimal_places(4.00000000)
















x <- 52.67780165
as.character(x)
"52.67780165"
format(x, scientific = FALSE, trim = FALSE, digits = 10)
"52.6778"
?format

num_str <- format(x, scientific = FALSE, trim = FALSE)
num_str
# Split the string by the decimal point
parts <- strsplit(num_str, "\\.")[[1]]
parts


# Check if there are digits after the decimal point
if (length(parts) > 1) {
  print(nchar(gsub("0+$", "", parts[2])))  # Remove trailing zeros and count remaining digits
} else {
  print(0)  # No decimal places
}


get_decimal_places(52.67780165)

x <- 1.000000000000001
x
as.character(x)
