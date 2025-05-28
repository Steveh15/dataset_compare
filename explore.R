
unique_keys <- c("USUBJID", "ID")


comp <- summary(arsenal::comparedf(df1, df2, by = unique_keys))

# --- Mismatched rows check

mismatched_rows <- comp$obs.table %>% as_tibble() %>%
  mutate(
    version = if_else(version == "x", "ADPP", "ADPP-like")
  )
names(mismatched_rows) <- c("ADPP or ADPP-like", unique_keys, "Row number")

# --- AVAL differences check

aval_diffs <- comp$diffs.table %>%
  filter(var.x == "AVAL") %>%
  unnest(
    cols = c(values.x, values.y)
  ) %>%
  mutate(

    diff = abs(values.x - values.y),
    tol1 = diff >= 1e-3,
    tol2 = diff >= 1e-6,
    tol3 = diff >= 1e-9,
    tol4 = diff < 1e-9,

    tol_na =  (is.na(values.x) & !is.na(values.y) ) | (!is.na(values.x) & is.na(values.y) ),

    tol0 = any(c(tol_na, tol1, tol2, tol3, tol4), na.rm = TRUE),

    tol = case_when(
      tol_na ~ "Missing",
      tol1 ~ ">= 1e-3",
      !tol1 & tol2  ~ ">= 1e-6",
      !tol2 & tol3 ~ ">= 1e-9",
      !tol3 & tol4 ~ "< 1e-9",

    )

  ) %>%
  filter(tol1 | tol2 | tol3 | tol4 | tol_na)

aval_summary <- aval_diffs %>%
  summarise(
    tol0_sum = sum(tol0, na.rm = TRUE),
    tol1_sum = sum(tol1, na.rm = TRUE),
    tol2_sum = sum(tol2 & !tol1, na.rm = TRUE),
    tol3_sum = sum(tol3 & !tol2, na.rm = TRUE),
    tol4_sum = sum(tol4 & !tol3, na.rm = TRUE),
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
      "tol4_sum"   ~ "∆ < 1e-9",
      "tolna_sum"  ~ "AVAL missing in one dataset"
    ),
    value = as.character(value)
  )

aval_summary_bold <- aval_summary
aval_summary_bold[1, ] <- lapply(aval_summary_bold[1, ], function(x) paste0("<strong>", x, "</strong>"))

names(aval_summary_bold) <- c("Difference detected", "n")

aval_diff_table <- aval_diffs %>%
  select(all_of(unique_keys), values.x, values.y, tol) %>%
  mutate(
    values.x = format(values.x, digits = 9, nsmall = 3) %>% trimws(),
    values.y = format(values.y, digits = 9, nsmall = 3) %>% trimws(),
    across(c("values.x", "values.y"), ~if_else(. == "NA", ".", .))
  )


# --- Other differences check



other_diffs <- comp$diffs.table %>%
  filter(var.x != "AVAL") %>%
  mutate(
    # values.x = if_else(is.na(values.x), NA, as.character(values.x))

    across( c("values.x", "values.y"),~ if_else(is.na(.), NA, as.character(.)))
  ) %>%
  # as_tibble() %>%
  select(all_of(unique_keys), var.x, values.x, values.y)

other_diffs_unique <- other_diffs %>%
  distinct(var.x, values.x, values.y)
