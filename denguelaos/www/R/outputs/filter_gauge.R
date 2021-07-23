output$filter_gauge <- renderGauge({
  req(dengue_dta())
  req(dengue_dta_filt())
  
  n <- dengue_dta_filt() %>% nrow()
  total <- dengue_dta() %>% nrow()
  
  gauge(n, min = 0, max = total, abbreviate = FALSE, gaugeSectors(colors = "#2c3e50"))
})

output$filter_gauge_text <- renderText({
  req(dengue_dta())
  req(dengue_dta_filt())
  
  n <- dengue_dta_filt() %>% nrow()
  total <- dengue_dta() %>% nrow()
  pct <- round(100* n / total, 1)
  
  glue("{pct}% of patients are selected")
})