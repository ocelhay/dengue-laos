# Table of patients, PCR result
output$table_patients_pcr_res <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table((dengue_dta_filt()$pcr_result))%>%
    as.data.frame() %>%
    rename(`PCR Result` = Var1, Patients = Freq)
})

# Plot of patients, PCR results
output$plot_patients_pcr_res <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    group_by(collection_year, collection_month, pcr_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, pcr_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = pcr_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "PCR Results") +
    # scale_fill_manual(values = cols_pcr_result) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
  
})


# Table of patients, PCR serotype
output$table_patients_pcr <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table((dengue_dta_filt()$pcr_serortype_result))%>%
    as.data.frame() %>%
    rename(`PCR Serotype` = Var1, Patients = Freq)
})

# Plot of patients, PCR serotype
output$plot_patients_pcr <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    filter(!is.na(pcr_serortype_result)) %>%
    group_by(collection_year, collection_month, pcr_serortype_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, pcr_serortype_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = pcr_serortype_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "PCR Serotype") +
    # scale_fill_manual(values = cols_pcr_serotype) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
  
})