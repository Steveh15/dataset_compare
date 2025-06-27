


unique_keys <- c("USUBJID", "ID")


res <- compareDatasets(df1, df2, unique_keys, metadata = NULL)



unique_keys <- c("USUBJID", "ID")


compare_summary <- summary(arsenal::comparedf(df1, df2, by = unique_keys))

row_level_checks <- list()

# --- Mismatched rows check

mismatched_rows <- compare_summary$obs.table %>% as_tibble() %>%
  mutate(
    version = if_else(version == "x", "ADPP", "ADPP-like")
  )
names(mismatched_rows) <- c("ADPP_or_ADPPL", unique_keys, "row_n")

row_level_checks$mismatched_rows <- mismatched_rows


# --- AVAL differences check

aval_diffs_1 <- compare_summary$diffs.table %>%
  filter(var.x == "AVAL") %>%
  unnest(
    cols = c(values.x, values.y)
  ) %>%
  mutate(

    tol1  = abs(values.x - values.y) >= 1e-3,
    tol2  = abs(values.x - values.y) >= 1e-6,
    tol3  = abs(values.x - values.y) >= 1e-9,

    tol_na =  (is.na(values.x) & !is.na(values.y) ) | (!is.na(values.x) & is.na(values.y) ),

    tol0 = any(c(tol_na, tol1, tol2, tol3), na.rm = TRUE),

    tol = case_when(
      tol_na ~ "Missing",
      tol1 ~ ">= 1e-3",
      !tol1 & tol2  ~ ">= 1e-6",
      !tol2 & tol3 ~ ">= 1e-9"

    )

  ) %>%
  filter(tol1 | tol2 | tol3 | tol_na)


aval_summary <- aval_diffs_1 %>%
  summarise(
    tol0_sum = sum(tol0, na.rm = TRUE),
    tol1_sum = sum(tol1, na.rm = TRUE),
    tol2_sum = sum(tol2 & !tol1, na.rm = TRUE),
    tol3_sum = sum(tol3 & !tol2, na.rm = TRUE),
    tolna_sum = sum(tol_na, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything()) %>%
  mutate(
    name = case_match(
      name,
      "tol0_sum"   ~ "Total",
      "tol1_sum"   ~ "∆ ≥ 1e-3",
      "tol2_sum"   ~ "1e-3 > ∆ ≥ 1e-6",
      "tol3_sum"   ~ "1e-6 > ∆ ≥ 1e-9",
      "tolna_sum"  ~ "AVAL missing in one dataset"
    ),
    value = as.character(value)
  )


aval_diffs <- aval_diffs_1 %>%
  mutate(
    aval_adpp = format(values.x, digits = 9, nsmall = 3) %>% trimws(),
    aval_adppl = format(values.y, digits = 9, nsmall = 3) %>% trimws(),
    across(c("aval_adpp", "aval_adppl"), ~if_else(. == "NA", ".", .))
  ) %>%
  select(all_of(unique_keys), aval_adpp, aval_adppl, tol)

row_level_checks$aval_comparison <- list()

row_level_checks$aval_comparison$aval_summary <- aval_summary
row_level_checks$aval_comparison$aval_diffs <- aval_diffs

# --- All Other Variables Comparison

other_vars_comparison <- list()

other_vars_comparison$all_diffs <- compare_summary$diffs.table %>%
  filter(var.x != "AVAL") %>%
  mutate(
    across( c("values.x", "values.y"),~ if_else(is.na(.), NA, as.character(.)))
  ) %>%
  select(all_of(unique_keys), var = var.x, adpp = values.x, adppl = values.y) %>%
  as_tibble()

other_vars_comparison$unique_diffs <- other_vars_comparison$all_diffs %>%
  distinct(var, adpp, adppl) %>%
  as_tibble()


row_level_checks$other_vars_comparison <- other_vars_comparison





