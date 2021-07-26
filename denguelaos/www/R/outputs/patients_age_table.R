output$patients_age_table <- renderTable({
  req(dengue_dta_filt())
  
  dengue_dta_filt() %>%
    group_by(age_category) %>%
    summarise(Patients = n()) %>%
    mutate(`Pct.` = round(100 * Patients / sum(Patients), 1)) %>%
    transmute(age_category = factor(age_category, levels = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown")), 
              Patients, `Pct.`) %>%
    arrange(age_category) %>%
    rename(` ` = age_category)
})