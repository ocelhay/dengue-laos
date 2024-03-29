output$checklist_data <- renderText({
  req(dengue_dta())
  
  text_checklist(checklist_status, vec = c("read_sheet_1", "read_sheet_2", "read_sheet_3", "read_sheet_4", "read_sheet_5", 
                                           "names_sheet_1", "names_sheet_2", "names_sheet_3", "names_sheet_4", "names_sheet_5",
                                           "ward_list", "village_list", "district_list", "province_list", 
                                           "date_collection_missing",
                                           "date_onset_after_collected",
                                           "date_onset_after_received",
                                           "date_onset_after_elisa_ns1",
                                           "date_onset_after_elisa_ig_m",
                                           "date_onset_after_rdt_test",
                                           "date_onset_after_pcr_test",
                                           "date_onset_after_sero_pcr_test",
                                           "date_collected_after_received",
                                           "date_collected_after_elisa_ns1",
                                           "date_collected_after_elisa_ig_m",
                                           "date_collected_after_rdt_test",
                                           "date_collected_after_pcr_test",
                                           "date_collected_after_sero_pcr_test",
                                           "date_received_after_elisa_ns1",
                                           "date_received_after_elisa_ig_m",
                                           "date_received_after_rdt_test",
                                           "date_received_after_pcr_test",
                                           "date_received_after_sero_pcr_test",
                                           "date_elisa_ns1_after_pcr_test",
                                           "date_elisa_igm_after_ns1",
                                           "date_pcr_sero_after_pcr"))
})