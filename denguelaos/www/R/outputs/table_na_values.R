output$table_na_values <- DT::renderDataTable({
  req(dengue_dta())
  
  count_na <- c(names(dengue_dta()[, 1:10]), "pcr_serortype_result")
  
  dta <- tibble(
    Column = count_na,
    `Nb Missing Value` = count_na %>% map_int(function(x)  dengue_dta() |> pull(x) |> is.na() |> sum())
  ) %>%
    mutate(`% Missing Value` = round(100 * `Nb Missing Value` / dengue_dta() |> nrow(), 1))
  
  
  datatable(dta,
            rownames = FALSE,
            filter = "none",
            # style = "bootstrap",
            options = list(scrollX = TRUE, scrollY = 300, paging = FALSE, dom = "lrtip"))
})