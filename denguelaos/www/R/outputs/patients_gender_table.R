output$patients_gender_table <- renderTable({
  req(dengue_dta_filt())
  
  dengue_dta_filt() %>%
    group_by(gender) %>%
    summarise(Patients = n()) %>%
    mutate(`Pct.` = round(100 * Patients / sum(Patients), 1)) %>%
    rename(` ` = gender)
})