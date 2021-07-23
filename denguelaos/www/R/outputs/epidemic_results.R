output$epidemic_ts <- renderHighchart({
  req(dengue_dta_filt())
  req(dengue_dta_filt() %>% nrow() >= 1)
  
  
  if(input$display_unit == "Display by year")   display <- "years"
  if(input$display_unit == "Display by month")  display <- "months"
  if(input$display_unit == "Use heuristic")     display <- heuristic_time_unit(dengue_dta_filt()$collected_date_dd_mm_yy)
  
  if(display == "years") {
    return(dengue_dta_filt() %>%
             mutate(year = year(collected_date_dd_mm_yy)) %>%
             group_by(year, dengue_virus) %>%
             count() %>%
             ungroup() %>%
             complete(dengue_virus, nesting(year), fill = list(n = 0)) %>%
             mutate(dengue_virus = factor(dengue_virus, levels = c("No evidence of dengue infection", "Presumptive dengue infection", "Confirmed dengue infection"))) %>%
             hchart("column", hcaes(x = 'year', y = 'n', group = 'dengue_virus',
                                    label = 'dengue_virus', label2 = 'year')) %>%
             hc_plotOptions(column = list(stacking = "normal", dataLabels = list())) %>%
             hc_colors(cols) %>%
             hc_title(text = "Patients per Year") %>%
             hc_xAxis(title = list(text = "Year of collection")) %>%
             hc_yAxis(title = list(text = "Number of Patients")) %>%
             hc_tooltip(useHTML = TRUE, borderWidth = 4,
                        pointFormat = "{point.label}: {point.y}") %>%
             hc_legend(
               align = "left",
               verticalAlign = "top",
               layout = "vertical",
               x = 0,
               y = 100
             ) 
    )
  }
  
  # TODO: complete to ensure there are no months without data
  if(display == "months") {
  return(
    dengue_dta_filt() %>%
      mutate(month = month(collected_date_dd_mm_yy, label = FALSE)) %>%
      mutate(month = ifelse(month < 10, paste0("0", month), as.character(month))) %>%
      mutate(year_month = paste(collection_year, month, sep = "-")) %>%
      group_by(year_month, dengue_virus) %>%
      count() %>%
      ungroup() %>%
      arrange(year_month) %>%
      mutate(year_month = as.factor(year_month)) %>%
      complete(dengue_virus, nesting(year_month), fill = list(n = 0)) %>%
      mutate(dengue_virus = factor(dengue_virus, levels = c("No evidence of dengue infection", "Presumptive dengue infection", "Confirmed dengue infection"))) %>%
      hchart("column", hcaes(x = 'year_month', y = 'n', group = 'dengue_virus',
                             label = 'dengue_virus', label2 = 'year_month')) %>%
      hc_plotOptions(column = list(stacking = "normal", dataLabels = list())) %>%
      hc_colors(cols) %>%
      hc_title(text = "Patients per Month") %>%
      hc_xAxis(title = list(text = "Month of collection")) %>%
      hc_yAxis(title = list(text = "Number of Patients")) %>%
      hc_tooltip(useHTML = TRUE, borderWidth = 4,
                 pointFormat = "{point.label}: {point.y}") %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "vertical",
        x = 0,
        y = 100
      ) 
    )
  }
})