output$checklist_data <- renderText(  
  text_checklist(checklist_status, vec = c("read_sheet_1", "read_sheet_2", "read_sheet_3", "read_sheet_4", "read_sheet_5", 
                                           "names_sheet_1", "names_sheet_2", "names_sheet_3", "names_sheet_4", "names_sheet_5",
                                           "ward_list", "village_list", "district_list", "province_list", "date_onset_after_collected"))
)