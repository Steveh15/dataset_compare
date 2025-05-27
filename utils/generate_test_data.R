
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
    # AVAL = round(AVAL, 3)
  )



samps <- sample(1:nrow(df2), 120)

samp1 <- samps[1:40]
samp2 <- samps[41:80]
samp3 <- samps[81:120]


df2$AVAL[samp1] <- df2$AVAL[samp1]*runif(40, min = 0.5, max = 1.5)
df2$AVAL[samp2] <- NA
df2$AVAL[samp3] <- round(df2$AVAL[samp3],3)
df2$edit <- NA
df2$edit[samps] <- 1

# samp1 <-
#
# samp1 <- sample(1:nrow(df2), 40)
# samp2 <- sample(1:nrow(df2), 30)
# samp3 <- sample(1:nrow(df2), 60)
#
# df2$AVAL[samp1] <- df2$AVAL[samp1]*runif(40, min = 0.5, max = 1.5)
# df2$edit <- NA
# df2$edit[samp1] <- 1
#
# df2$AVAL[samp3] <- round(df2$AVAL[samp3],3)


#
# df2 <- df2 %>% mutate(
#   AVAL = round(AVAL, 3)
# )

# df2$AVAL[samp2] <- NA



df2 <- df2 %>%
 mutate(
    AVAL = case_when(
      PPTESTCD == "AUCALL" ~ round(AVAL, 3),
      PPTESTCD == "CLST" ~ round(AVAL, 4),
      PPTESTCD == "LAMZNPT" ~ round(AVAL, 0),
      .default = AVAL
    )
  )


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
