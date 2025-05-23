
# Example data for testing

set.seed(123)

df1_1 <- pharmaverseadam::adpp %>%
  group_by(USUBJID) %>%
  mutate(ID = row_number()) %>%
  select(USUBJID, ID, everything())


df2 <- df1_1[  -sample(1:nrow(df1_1), 25), ] %>%
  select(-c(DTHFL, DTHDTC)) %>%
  mutate(
    AGE = as.character(AGE),
    AVAL = round(AVAL, 3)
  )


samp1 <- sample(1:nrow(df2), 40)
samp2 <- sample(1:nrow(df2), 30)

df2$AVAL[samp1] <- df2$AVAL[samp1]*runif(40, min = 0.5, max = 1.5)
df2$edit <- NA
df2$edit[samp1] <- 1



df2 <- df2 %>% mutate(
  AVAL = round(AVAL, 3)
)

# df2$AVAL[samp2] <- NA




df1 <- df1_1[-sample(1:nrow(df1_1), 15), ] %>%
  select(-c(DMDY, TRTSDTM, TRTSTMF))


test1_1 <- df1
test1_2 <- df1

haven::write_xpt(df1, file.path("data", "df1.xpt"))
haven::write_xpt(df2, file.path("data", "df2.xpt"))


haven::write_xpt(test1_1, file.path("data", "test1_1.xpt"))
haven::write_xpt(test1_2, file.path("data", "test1_2.xpt"))


#
#
#
# df2 %>% count(PPTESTCD)
