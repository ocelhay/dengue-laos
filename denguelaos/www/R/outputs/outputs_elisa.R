# Table of patients, ELISA NS1
output$table_patients_elisa_ns1 <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    filter(elisa_ns1_test_result %in% c("Negative", "Equivocal", "Positive")) %>%
    pull(elisa_ns1_test_result) %>%
    table_method_results()
})

# Plot of patients, ELISA NS1
output$plot_patients_elisa_ns1 <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() |>
    filter(! is.na(collection_year), ! is.na(collection_month)) |> 
    filter(elisa_ns1_test_result %in% c("Negative", "Equivocal", "Positive")) |> 
    mutate(elisa_ns1_test_result = factor(elisa_ns1_test_result, levels = c("Negative", "Equivocal", "Positive"))) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, elisa_ns1_test_result) |> 
    complete(elisa_ns1_test_result, collection_year_month, fill = list(n = 0)) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "elisa_ns1_test_result")) |> 
    hc_yAxis(title = list(text = "Results")) |>
    hc_xAxis(title = "") |> 
    hc_colors(cols_nep) |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})

# Table of patients, ELISA IgM
output$table_patients_elisa_igm <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    filter(elisa_ig_m_test_result %in% c("Negative", "Equivocal", "Positive")) %>%
    pull(elisa_ig_m_test_result) %>%
    table_method_results()
})

# Plot of patients, ELISA IgM
output$plot_patients_elisa_igm <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() |>
    filter(! is.na(collection_year), ! is.na(collection_month)) |> 
    filter(elisa_ig_m_test_result %in% c("Negative", "Equivocal", "Positive")) |> 
    mutate(elisa_ig_m_test_result = factor(elisa_ig_m_test_result, levels = c("Negative", "Equivocal", "Positive"))) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, elisa_ig_m_test_result) |> 
    complete(elisa_ig_m_test_result, collection_year_month, fill = list(n = 0)) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "elisa_ig_m_test_result")) |> 
    hc_yAxis(title = list(text = "Results")) |>
    hc_xAxis(title = "") |> 
    hc_colors(cols_nep) |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})