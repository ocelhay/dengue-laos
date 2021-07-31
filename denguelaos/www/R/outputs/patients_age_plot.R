output$patients_age_plot <- renderPlot({
  req(dengue_dta_filt())
  
  ggplot(dengue_dta_filt(), aes(age_in_years)) +
    geom_histogram(binwidth = 1) +
    geom_vline(xintercept = median(dengue_dta_filt()$age_in_years), na.rm = TRUE) +
    geom_label(x = median(dengue_dta_filt()$age_in_years, na.rm = TRUE), y = 0, label = "Median Age") +
    labs(x = "Age (y.o.)", y = "Patients") +
    theme_minimal(base_size = 14)
})