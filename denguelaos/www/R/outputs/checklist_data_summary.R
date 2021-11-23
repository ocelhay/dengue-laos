output$checklist_data_summary <- renderText({
  req(dengue_dta())
  
  if(data_summary$status == "ko") return(paste0("<span class='cl-fail'><i class='fa fa-times-circle'></i> ", i18n$t("Error when importing data"), "</span>"))
  if(data_summary$status == "okay") return(paste0("<span class='cl-success'><i class='fa fa-check'></i> ", i18n$t("Data successfully imported"), "</span>"))
})