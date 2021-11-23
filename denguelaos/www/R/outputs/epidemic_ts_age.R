output$epidemic_ts_age <- renderPlot({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  if(input$display_unit == "Display by year")   display <- "years"
  if(input$display_unit == "Display by month")  display <- "months"
  if(input$display_unit == "Use suggested unit")     display <- heuristic_time_unit(dengue_dta_filt()$collected_date_dd_mm_yy)
  
  if(display == "years") {
    return(
      dengue_dta_filt() %>%
        mutate(year = year(collected_date_dd_mm_yy)) %>%
        group_by(year, dengue_virus, age_category) %>%
        count() %>%
        ungroup() %>%
        complete(dengue_virus, nesting(year), fill = list(n = 0)) %>%
        filter(!is.na(age_category)) %>%
        mutate(dengue_virus = factor(dengue_virus, levels = c("No evidence of dengue infection", "Presumptive dengue infection", "Confirmed dengue infection")),
               age_category = factor(age_category, levels = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o."))) %>%
        ggplot(aes(x = year, y = n, fill = dengue_virus)) +
        geom_col() +
        scale_fill_manual(values = c("No evidence of dengue infection" = cols[1], 
                                     "Presumptive dengue infection" = cols[2], 
                                     "Confirmed dengue infection" = cols[3])) +
        facet_grid(age_category ~ .) +
        theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, vjust = 0.5))
    )
  }
  
  if(display == "months") {
    return(
      dengue_dta_filt() %>%
        mutate(month = month(collected_date_dd_mm_yy, label = FALSE)) %>%
        mutate(month = ifelse(month < 10, paste0("0", month), as.character(month))) %>%
        mutate(year_month = paste(collection_year, month, sep = "-")) %>%
        group_by(year_month, dengue_virus, age_category) %>%
        count() %>%
        ungroup() %>%
        arrange(year_month) %>%
        mutate(year_month = as.factor(year_month)) %>%
        complete(dengue_virus, nesting(year_month), fill = list(n = 0)) %>%
        filter(!is.na(age_category)) %>%
        mutate(dengue_virus = factor(dengue_virus, levels = c("No evidence of dengue infection", "Presumptive dengue infection", "Confirmed dengue infection")),
               age_category = factor(age_category, levels = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o."))) %>%
        ggplot(aes(x = year_month, y = n, fill = dengue_virus)) +
        geom_col() +
        scale_fill_manual(values = c("No evidence of dengue infection" = cols[1], 
                                     "Presumptive dengue infection" = cols[2], 
                                     "Confirmed dengue infection" = cols[3])) +
        facet_grid(age_category ~ .) +
        theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, vjust = 0.5))
    )
  }
})