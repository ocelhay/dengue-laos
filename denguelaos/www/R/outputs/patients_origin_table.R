output$patients_origin_table <- renderTable({
  req(dengue_dta_filt())
  
  if(input$selected_geo_level == "Province") {
    dta <- dengue_dta_filt() %>%
      group_by(province) %>%
      summarise(Patients = n()) %>%
      mutate(`Pct.` = 100 * Patients / sum(Patients)) %>%
      rename(` ` = province)
  }
  
  if(input$selected_geo_level == "District") {
    dta <- dengue_dta_filt() %>%
      group_by(district) %>%
      summarise(Patients = n()) %>%
      mutate(`Pct.` = 100 * Patients / sum(Patients)) %>%
      rename(` ` = district)
  }
  
  
  if(input$selected_geo_level == "Village") {
    dta <- dengue_dta_filt() %>%
      group_by(village) %>%
      summarise(Patients = n()) %>%
      mutate(`Pct.` = 100 * Patients / sum(Patients)) %>%
      rename(` ` = village)
  }
  
  return(dta)
})