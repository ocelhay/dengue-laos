table_method_results <- function(vec) {
  total <- length(vec)
  
  table(vec, useNA = "ifany") |>
    as.data.frame() |>
    mutate(Freq = glue("{Freq} ({round(100*Freq/total, 1)}%)")) |> 
    mutate(order = case_when(
      vec == "Positive"  ~ 1,
      vec == "Equivocal" ~ 2,
      vec == "Negative"  ~ 3,
      TRUE ~ 4
    )) |>
    arrange(order) |>
    select(-order) |>
    bind_rows(
      tribble(~vec, ~Freq,
              "Total", as.character(total))
    ) |> 
    rename(` ` = vec, Patients = Freq)
}