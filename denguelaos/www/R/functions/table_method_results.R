table_method_results <- function(vec) {
  
  table(vec, useNA = "ifany") |>
    as.data.frame() |>
    mutate(order = case_when(
      vec == "Positive"  ~ 1,
      vec == "equivocal" ~ 2,
      vec == "Negative"  ~ 3,
      vec == "not done (PCR+)"  ~ 4,
      vec == "not done (NS1+)"  ~ 5,
      vec == "not done"  ~ 6,
      TRUE  ~ 7,
    )) |>
    arrange(order) |>
    select(-order) |>
    rename(` ` = vec, Patients = Freq)
  
}