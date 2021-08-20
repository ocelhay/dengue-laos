# Table of patients, RDT NS1
output$table_patients_rdt_ns1 <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table_method_results(vec = dengue_dta_filt()$rdt_ns1_result)
})

# Plot of patients, RDT NS1
output$plot_patients_rdt_ns1 <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() |>
    filter(! is.na(collection_year), ! is.na(collection_month)) |> 
    filter(rdt_ns1_result %in% c("Positive", "equivocal", "Negative")) |> 
    mutate(rdt_ns1_result = factor(rdt_ns1_result, levels = c("Negative", "equivocal", "Positive"))) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, rdt_ns1_result) |> 
    complete(rdt_ns1_result, collection_year_month, fill = list(n = 0)) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "rdt_ns1_result")) |> 
    hc_yAxis(title = list(text = "Results")) |>
    hc_xAxis(title = "") |> 
    hc_colors(cols_nep) |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})

# Plot of patient, RDT IgM
output$plot_patients_rdt_igm <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() |>
    filter(! is.na(collection_year), ! is.na(collection_month)) |> 
    filter(rdt_ig_m_result %in% c("Positive", "equivocal", "Negative")) |> 
    mutate(rdt_ig_m_result = factor(rdt_ig_m_result, levels = c("Negative", "equivocal", "Positive"))) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, rdt_ig_m_result) |> 
    complete(rdt_ig_m_result, collection_year_month, fill = list(n = 0)) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "rdt_ig_m_result")) |> 
    hc_yAxis(title = list(text = "Results")) |>
    hc_xAxis(title = "") |> 
    hc_colors(cols_nep) |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})

# Table of patients, RDT IgM
output$table_patients_rdt_igm <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table_method_results(vec = dengue_dta_filt()$rdt_ig_m_result)
})


# Plot of patient, RDT IgG
output$plot_patients_rdt_igg <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() |>
    filter(! is.na(collection_year), ! is.na(collection_month)) |> 
    filter(rdt_ig_g_result %in% c("Positive", "equivocal", "Negative")) |> 
    mutate(rdt_ig_g_result = factor(rdt_ig_g_result, levels = c("Negative", "equivocal", "Positive"))) |> 
    mutate(collection_year_month = as_date(glue("{collection_year}-{collection_month}-01")))  |>
    count(collection_year_month, rdt_ig_g_result) |> 
    complete(rdt_ig_g_result, collection_year_month, fill = list(n = 0)) |> 
    hchart(type = "column", hcaes(x = "collection_year_month", y = "n", group = "rdt_ig_g_result")) |> 
    hc_yAxis(title = list(text = "Results")) |>
    hc_xAxis(title = "") |> 
    hc_colors(cols_nep) |> 
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})

# Table of patients, RDT IgG
output$table_patients_rdt_igg <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table_method_results(vec = dengue_dta_filt()$rdt_ig_g_result)
})