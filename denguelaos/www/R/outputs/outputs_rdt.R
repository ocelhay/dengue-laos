# Table of patients, RDT NS1
output$table_patients_rdt_ns1 <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table((dengue_dta_filt()$rdt_ns1_result)) %>%
    as.data.frame() %>%
    rename(`RDT NS1 result` = Var1, Patients = Freq)
})

# Plot of patients, RDT NS1
output$plot_patients_rdt_ns1 <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    group_by(collection_year, collection_month, rdt_ns1_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, rdt_ns1_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = rdt_ns1_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "RDT, NS1") +
    # scale_fill_manual(values = cols_rdt) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
})

# Plot of patient, RDT IgM
output$plot_patients_rdt_igm <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    group_by(collection_year, collection_month, rdt_ig_m_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, rdt_ig_m_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = rdt_ig_m_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "RDT, IgM") +
    # scale_fill_manual(values = cols_rdt) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
})

# Table of patients, RDT IgM
output$table_patients_rdt_igm <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table((dengue_dta_filt()$rdt_ig_m_result)) %>%
    as.data.frame() %>%
    rename(`RDT IgM result` = Var1, Patients = Freq)
})



# Plot of patient, RDT IgG
output$plot_patients_rdt_igg <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  dengue_dta_filt() %>%
    group_by(collection_year, collection_month, rdt_ig_g_result) %>% 
    count() %>%
    ungroup() %>%
    complete(nesting(collection_year, collection_month, rdt_ig_g_result), fill = list(n = 0)) %>%
    ggplot(aes(x = collection_month, y = n, fill = rdt_ig_g_result)) +
    geom_col() +
    labs(x = NULL, y = "Nb. of Tests", title = "RDT, IgG") +
    # scale_fill_manual(values = cols_rdt_igg) +
    facet_wrap(~ collection_year, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(nrow = 2))
})

# Table of patients, RDT IgG
output$table_patients_rdt_igg <- renderTable({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  table((dengue_dta_filt()$rdt_ig_g_result)) %>%
    as.data.frame() %>%
    rename(`RDT IgG result` = Var1, Patients = Freq)
})