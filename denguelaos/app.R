source("./www/R/startup.R", local = TRUE)

ui <- fluidPage(
    title = "Dashboard of LOMWRU Dengue fever data",
    theme = app_theme_en,
    includeCSS("www/styles.css"),
    usei18n(i18n),
    
    navbarPage(id = "tabs",
               title = "LOMWRU Dengue Dashboard",
               collapsible = TRUE, inverse = FALSE, 
               position = "static-top",
               
               header = conditionalPanel(
                   id = "header-filter",
                   condition = "input.tabs != 'welcome' & input.tabs != 'data_management'",
                   div(id = "data-filters",
                       h4(icon("filter"), "Filter Dataset"),
                       dateRangeInput("filter_date", "Filter by collection date"),
                       checkboxGroupInput("filter_age", "Filter by age categories", inline = TRUE, 
                                          choices = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"), 
                                          selected = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown")),
                       checkboxGroupInput("filter_status", "Filter by case status", inline = TRUE,
                                          choices = c("Confirmed dengue infection", "Presumptive dengue infection", "No evidence of dengue infection"), 
                                          selected = c("Confirmed dengue infection", "Presumptive dengue infection", "No evidence of dengue infection"))
                   )
               ),
               
               tabPanel(i18n$t("Welcome"), value = "welcome",
                        actionLink("debug", "Click to call browser()"), br(),
                        
                        img(src = "./images/LOMWRU_partners_logo.jpg", style = "height: 40px;"),
                        fluidRow(
                            column(4,
                                   br(), br(),
                                   selectInput(
                                       inputId = 'selected_language', label = span(icon('language'), i18n$t('Language:')),
                                       choices = i18n$get_languages(), selected = i18n$get_key_translation(), width = "150px"
                                   ),
                                   br(), 
                                   strong(icon("upload"), "Upload Data (.xlsx format)"),
                                   p("For more info on data format, ", 
                                     tags$a(href = "mailto:olivier.celhay@gmail.com", "contact us.")),
                                   fileInput("file_excel", label = NULL, accept = ".xlsx", buttonLabel = "Browse...")
                            ),
                            column(4,
                                   img(src = "./images/dengue_fever_symptoms.png", width = "95%", alt = "Dengue Fever Symptoms")
                            ),
                            column(4,
                                   includeMarkdown("./www/markdown/about_dengue.md")
                            )
                        )
               ),
               tabPanel(i18n$t("Data Management"), value = "data_management",
                        fluidRow(
                            column(2, htmlOutput("checklist_data_summary")),
                            column(5, br(), htmlOutput("checklist_data")),
                            column(5, strong("Missing Values:"), DT::dataTableOutput("table_na_values"))
                        )
               ),
               tabPanel(i18n$t("Epidemic Trends"), value = "epidemic_trends",
                        highchartOutput("epidemic_ts")
               )
    )
)


server <- function(input, output, session) {
    # TODO: remove on prod.
    observeEvent(input$debug, browser())
    
    # Reactive data management ----
    checklist_status <- reactiveValues()
    data_summary <- reactiveValues(status = "okay")
    dengue_dta <- reactiveVal()
    
    # Filter the dataset based on UI
    dengue_dta_filt <- reactive({
        dengue_dta() %>% filter(
            collected_date_dd_mm_yy >= input$filter_date[1],
            collected_date_dd_mm_yy <= input$filter_date[2],
            age_category %in% input$filter_age,
            dengue_virus %in% input$filter_status
        )
    })
    
    # On app launch ----
    hideTab("tabs", target = "data_management")
    hideTab("tabs", target = "epidemic_trends")
    
    # Language management ----
    observe({
        session$setCurrentTheme(
            if (isTRUE(input$selected_language == "la")) app_theme_la else app_theme_en
        )
    })
    
    observeEvent(input$selected_language, {
        update_lang(session, input$selected_language)
    })
    
    observeEvent(input$file_excel, {
        source("./www/R/read_file.R", local = TRUE)
        source("./www/R/process_data.R", local = TRUE)
        
        showTab("tabs", target = "data_management")
        
        if(data_summary$status == "okay") {
            dengue_dta(data)
            updateDateRangeInput(session = session, inputId = "filter_date", 
                                 start = min(dengue_dta()$collected_date_dd_mm_yy, na.rm = TRUE), 
                                 end = max(dengue_dta()$collected_date_dd_mm_yy, na.rm = TRUE))
            showTab("tabs", target = "epidemic_trends")
        }
    })
    
    # misc ----
    file_list <- list.files(path = "./www/R/outputs", pattern = "*.R", recursive = TRUE)
    for (file in file_list) source(paste0("./www/R/outputs/", file), local = TRUE)$value
}    

shinyApp(ui = ui, server = server)
