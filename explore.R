

val <- 12.123+0.0004
val

format(val, digits = 6)

format(val, digits = 9, nsmall = 9)

format(val, digits = 12)

format(val, digits = 15)

?format()

as.character(val)
as.character(0.0000000000001)

keys = c("USUBJID", "ID")


c1 <- arsenal::comparedf(df1, df2)

c1

c1$frame.summary


c1$vars.summary



vals1 <- df1 %>%
  select(all_of(c(keys, "AVAL")))

vals2 <- df2 %>%
  select(all_of(c(keys, "AVAL")))


merged <- vals1 %>%
  full_join(vals2, by = keys) %>%
  mutate(
      tol1  = abs(AVAL.x - AVAL.y) > 1e-3,
      tol2  = abs(AVAL.x - AVAL.y) > 1e-6,
      tol3  = abs(AVAL.x - AVAL.y) > 1e-9
  ) %>%
  filter(tol1 | tol2 | tol3) %>%
  print()

# c2 <- arsenal::comparedf(df1, df2, by = keys)
#
# dif <- summary(c2)$diffs.table
#
# test <- dif %>%
#   filter(var.x == "AVAL") %>%
#   unnest() %>%
#   mutate(
#     AVAL.x = as.numeric(values.x),
#     AVAL.y = as.numeric(values.y),
#     tol1  = abs(AVAL.x - AVAL.y) > 1e-3,
#     tol2  = abs(AVAL.x - AVAL.y) > 1e-6,
#     tol3  = abs(AVAL.x - AVAL.y) > 1e-10
#   ) %>%
#   print()

# tolcheck = abs(AVAL.x - AVAL.y) > tolerance,





summary(c2)


rounding_check(df1, df2)

get_decimal_places


t1 <- df1 %>% select(USUBJID, ID, PARAMCD, AVAL) %>% mutate(dp = get_decimal_places(AVAL, precision = 10))
t2 <- df2 %>% select(USUBJID, ID, PARAMCD, AVAL) %>% mutate(dp = get_decimal_places(AVAL, precision = 10))


t1 %>%
  group_by(PARAMCD) %>%
  summarise(
    min_dp = min(dp),
    max_dp = max(dp)
  )

t2 %>%
  group_by(PARAMCD) %>%
  summarise(
    min_dp = min(dp),
    max_dp = max(dp)
    )


test <- t2 %>%
  filter(PARAMCD == "AUCALL", dp == 6) %>%
  print()


options(digits = 7)

test$AVAL

t2$AVAL

df1$AVAL[1]


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
