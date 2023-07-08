# Table of patients, PCR result
output$table_patients_pcr_res <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    filter(pcr_result %in% c("Negative", "Equivocal", "Positive")) %>%
    pull(pcr_result) %>%
    table_method_results()
})

# Plot of patients, PCR results
output$plot_patients_pcr_res <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  cols <- cols_nep[c("Negative", "Equivocal", "Positive") %in% dengue_dta_filt()$pcr_result]
  
  dengue_dta_filt() |>
    filter(! is.na(collection_year), ! is.na(collection_month)) |> 
    filter(pcr_result %in% c("Negative", "Equivocal", "Positive")) |> 
    mutate(pcr_result = factor(pcr_result, levels = c("Negative", "Equivocal", "Positive"))) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, pcr_result) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "pcr_result")) |> 
    hc_yAxis(title = list(text = "Results")) |>
    hc_xAxis(title = "") |> 
    hc_colors(cols) |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})


# Table of patients, PCR serotype
output$table_patients_pcr <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() |> 
    filter(stringr::str_detect(pcr_serortype_result, "DENV")) |> 
    pull(pcr_serortype_result) |> 
    table_method_results()
})

# Plot of patients, PCR serotype
output$plot_patients_pcr <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  # List provided by Audrey DP on 2023-07-05.
  display_cat <- c(
    "DENV-1",
    "DENV-2",
    "DENV-3",
    "DENV-4",
    "DENV-1/2",
    "DENV-1/3",
    "DENV-1/4",
    "DENV-2/3",
    "DENV-2/4",
    "DENV-3/4",
    "No typing",
    "Not done"
  )
  
  dengue_dta_filt() |>
    filter(
      ! is.na(collection_year), 
      ! is.na(collection_month),
      pcr_serortype_result %in% display_cat
    ) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, pcr_serortype_result) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "pcr_serortype_result")) |> 
    hc_yAxis(title = list(text = "PCR Results")) |>
    hc_xAxis(title = "") |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})