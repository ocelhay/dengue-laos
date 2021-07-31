# Table of patients, ELISA NS1
output$table_patients_elisa_ns1 <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table(dengue_dta_filt()$elisa_ns1_test_result) %>%
    as.data.frame() %>%
    rename(`ELISA NS1 result` = Var1, Patients = Freq)
})

# Plot of patients, ELISA NS1
output$plot_patients_elisa_ns1 <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    group_by(collection_year, collection_month, elisa_ns1_test_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, elisa_ns1_test_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = elisa_ns1_test_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "ELISA, NS1") +
    # scale_fill_manual(values = cols_elisa_ns1) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
})

# Table of patients, ELISA IgM
output$table_patients_elisa_igm <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table((dengue_dta_filt()$elisa_ig_m_test_result)) %>%
    as.data.frame() %>%
    rename(`ELISA IgM result` = Var1, Patients = Freq)
})

# Plot of patients, ELISA IgM
output$plot_patients_elisa_igm <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    group_by(collection_year, collection_month, elisa_ig_m_test_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, elisa_ig_m_test_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = elisa_ig_m_test_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "ELISA, IgM") +
    # scale_fill_manual(values = cols_elisa_igm) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
})