
# Example data for testing

set.seed(123)

df1_1 <- pharmaverseadam::adpp %>%
  group_by(USUBJID) %>%
  mutate(ID = row_number()) %>%
  select(USUBJID, ID, everything())


df2 <- df1_1[  -sample(1:nrow(df1_1), 25), ] %>%
  select(-c(DTHFL, DTHDTC)) %>%
  mutate(
    AGE = as.character(AGE)
  )


df1 <- df1_1[-sample(1:nrow(df1_1), 15), ] %>%
  select(-c(DMDY, TRTSDTM, TRTSTMF))




haven::write_xpt(df1, file.path("data", "df1.xpt"))
haven::write_xpt(df2, file.path("data", "df2.xpt"))


