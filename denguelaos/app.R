source("./www/R/startup.R", local = TRUE)

ui <- page(
    title = "Dengue in Laos",
    theme = app_theme_en,
    includeCSS("www/styles.css"),
    usei18n(i18n),
    
    page_navbar(id = "tabs",
                title = "Dengue in Laos",
                collapsible = TRUE, inverse = FALSE, 
                position = "static-top",
                
                header = conditionalPanel(
                    id = "header-filter",
                    condition = "input.tabs != 'welcome' & input.tabs != 'data_management'",
                    div(id = "filters",
                        fluidRow(
                            column(4,
                                   h5(icon("filter"), "Filter Dataset"),
                                   dateRangeInput("filter_date", "by Collection Date:"),
                                   checkboxGroupInput("filter_age", "by Age Categories:", inline = TRUE, 
                                                      choices = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"), 
                                                      selected = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown"))
                            ),
                            column(5,
                                   pickerInput("filter_province", "by Province: (ordered by total number of patients)", 
                                               choices = "Placeholder", selected = "Placeholder", multiple = TRUE, width = "100%",
                                               options = list(`actions-box` = TRUE, `deselect-all-text` = "None...",
                                                              `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                   HTML("by Case Status:"),
                                   div(id = "confirmed-box", 
                                       awesomeCheckbox("filter_status_confirmed", "Confirmed dengue infection = dengue PCR and/or dengue NS1 positive (RDT or ELISA)",
                                                       value = TRUE, width = "100%")),
                                   div(id = "presumptive-box", 
                                       awesomeCheckbox("filter_status_presumptive", "Presumptive dengue infection = anti-dengue IgM detection (RDT or ELISA) alone (PCR and NS1 negative))",
                                                       value = TRUE, width = "100%")),
                                   div(id = "noevidence-box", 
                                       awesomeCheckbox("filter_status_noevidence", "No evidence of dengue infection = all other cases",
                                                       value = TRUE, width = "100%"))
                                   # ),
                            ),
                            column(3, 
                                   checkboxGroupInput("filter_gender", "by Gender:", inline = TRUE, 
                                                      choices = c("Male", "Female", "Unknown"),
                                                      selected = c("Male", "Female", "Unknown")),
                                   br(),
                                   gaugeOutput("filter_gauge", width = "100%", height = "100px"), br(),
                                   textOutput("filter_gauge_text")
                            )
                        )
                    )
                ),
                nav(i18n$t("Welcome"), value = "welcome",
                    # TODO (Prod): comment
                    # actionLink("debug", "Click to call browser()"), br(),
                    
                    img(src = "./images/LOMWRU_partners_logo.jpg", style = "height: 40px;"),
                    fluidRow(
                        column(3,
                               br(), br(),
                               pickerInput(
                                   "selected_language", label = span(icon("language"), i18n$t("Language:")),
                                   choices = lang$val,
                                   choicesOpt = list(content = lang$img)
                               ),
                               br(), 
                               strong(icon("upload"), "Upload Data (.xlsx format)"),
                               p("For more info on data format, ", 
                                 tags$a(href = "mailto:olivier.celhay@gmail.com", "contact us.")),
                               fileInput("file_excel", label = NULL, accept = ".xlsx", buttonLabel = "Browse..."),
                               conditionalPanel("output.file_uploaded",
                                                prettySwitch("show_datamanagement_tab", "(Advanced) Show Data Management")
                               )
                        ),
                        column(5,
                               includeMarkdown("./www/markdown/about_dengue.md")
                        ),
                        column(4,
                               includeMarkdown("./www/markdown/about_dashboard.md")
                               
                        )
                    )
                ),
                nav(i18n$t("Data Management"), value = "data_management",
                    fluidRow(
                        column(2, htmlOutput("checklist_data_summary")),
                        column(5, strong("Import Log:"), htmlOutput("checklist_data")),
                        column(5, strong("Missing Values:"), DT::dataTableOutput("table_na_values"))
                    )
                ),
                nav(i18n$t("Time Trends"), value = "time_trends",
                    pickerInput("display_unit", label = NULL, 
                                choices = c("Use suggested unit", "Display by month", "Display by year")),
                    highchartOutput("epidemic_ts"),
                    fluidRow(
                        column(3),
                        column(9, plotOutput("epidemic_ts_age"))
                    )
                ),
                nav(i18n$t("Patients Info"), value = "patients_info",
                    fluidRow(
                        column(3,
                               div(class = "box",
                                   h4("Patients Gender"),
                                   tableOutput("patients_gender_table")
                               )
                        ),
                        column(9,
                               div(class = "box",
                                   h4("Patients Age"),
                                   fluidRow(
                                       column(4, tableOutput("patients_age_table")),
                                       column(8, plotOutput("patients_age_plot"))
                                   )
                               )
                        )
                    ),
                    div(class = "box",
                        h4("Origin of Patients"),
                        fluidRow(
                            column(4, 
                                   selectInput("selected_geo_level", "Geo Level:",
                                               choices = c("Province", "District", "Village"), selected = "Province", width = "150px"
                                   ),
                                   tableOutput("patients_origin_table")
                            ),
                            column(8, 
                                   leafletOutput("patients_origin_map")
                            )
                        )
                    )
                ),
                nav(i18n$t("Laboratory Results"), value = "lab_results",
                    tabsetPanel(
                        nav("Summary", value = "summary",
                            grVizOutput("diagram_algo", height = "600px")
                        ),
                        nav("PCR Method", value = "pcr",
                            fluidRow(
                                column(width = 3,
                                       br(),
                                       p("Confirmatory results for dengue infection - PCR"),
                                       tableOutput("table_patients_pcr_res")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_pcr_res")
                                )),
                            fluidRow(
                                column(width = 3,
                                       p("Serotyping results for PCR positive patients"),
                                       tableOutput("table_patients_pcr")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_pcr")
                                ))
                        ),
                        nav("ELISA Method", value = "elisa",
                            fluidRow(
                                column(width = 3,
                                       br(),
                                       HTML("Confirmatory results for dengue infection - ELISA NS1"),
                                       br(),
                                       em("(only performed for PCR negative patients)"),
                                       br(), br(),
                                       tableOutput("table_patients_elisa_ns1")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_elisa_ns1")
                                )),
                            br(),
                            fluidRow(
                                column(width = 3,
                                       br(),
                                       HTML("Presumptive results for dengue infection - ELISA IgM"),
                                       br(),
                                       em("(only performed for PCR and NS1 negative patients)"),
                                       br(), br(),
                                       tableOutput("table_patients_elisa_igm")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_elisa_igm")
                                ))
                        ),
                        nav("RDT Method", value = "rdt",
                            fluidRow(
                                column(width = 3,
                                       br(),
                                       p("Confirmatory results for dengue infection - RDT NS1"),
                                       tableOutput("table_patients_rdt_ns1")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_rdt_ns1")
                                )),
                            fluidRow(
                                column(width = 3,
                                       br(),
                                       p("Presumptive results for dengue infection - RDT IgM"),
                                       tableOutput("table_patients_rdt_igm")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_rdt_igm")
                                )
                            ),
                            fluidRow(
                                column(width = 3,
                                       br(),
                                       p("Presumptive results for dengue infection - RDT IgG"),
                                       tableOutput("table_patients_rdt_igg")
                                ),
                                column(width = 9,
                                       highchartOutput("plot_patients_rdt_igg")
                                )
                            )
                        )
                    )
                )
    )
)


server <- function(input, output, session) {
    # TODO (Prod): comment
    # observeEvent(input$debug, browser())
    
    # Reactive data management ----
    checklist_status <- reactiveValues()
    data_summary <- reactiveValues(status = "okay")
    dengue_dta_with_na <- reactiveVal()
    dengue_dta <- reactiveVal()
    
    # To show the data management option only when a file is uploaded.
    output$file_uploaded <- reactive({
        return(!is.null(dengue_dta()))
    })
    
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    # Filter the dataset based on UI
    dengue_dta_filt <- reactive({
        filter_vec <- NULL
        if(input$filter_status_confirmed)   filter_vec <- c(filter_vec, "Confirmed dengue infection")
        if(input$filter_status_presumptive) filter_vec <- c(filter_vec, "Presumptive dengue infection")
        if(input$filter_status_noevidence)  filter_vec <- c(filter_vec, "No evidence of dengue infection")
        
        dengue_dta() %>% filter(
            collected_date_dd_mm_yy >= input$filter_date[1],
            collected_date_dd_mm_yy <= input$filter_date[2],
            province %in% input$filter_province,
            age_category %in% input$filter_age,
            gender %in% input$filter_gender,
            dengue_virus %in% filter_vec
        )
    })
    
    # On app launch ----
    hideTab("tabs", target = "data_management")
    hideTab("tabs", target = "time_trends")
    hideTab("tabs", target = "patients_info")
    hideTab("tabs", target = "lab_results")
    
    observeEvent(input$show_datamanagement_tab,
                 ifelse(input$show_datamanagement_tab,
                        showTab("tabs", target = "data_management"),
                        hideTab("tabs", target = "data_management")
                 )
    )
    
    # Language management ----
    observeEvent(input$selected_language, {
      update_lang(input$selected_language, session)

        if (isTRUE(input$selected_language != "la"))  session$setCurrentTheme(app_theme_en)
        if (isTRUE(input$selected_language == "la"))  session$setCurrentTheme(app_theme_la)
    })

    observeEvent(input$selected_language, {
        update_lang(input$selected_language, session)
    })
    
    observeEvent(input$file_excel, {
        source("./www/R/read_file.R", local = TRUE)
        source("./www/R/process_data.R", local = TRUE)
        
        if(data_summary$status == "okay") {
            dengue_dta(data |> filter(!is.na(collected_date_dd_mm_yy)))
            updateDateRangeInput(session = session, "filter_date", 
                                 start = min(dengue_dta()$collected_date_dd_mm_yy, na.rm = TRUE), 
                                 end = max(dengue_dta()$collected_date_dd_mm_yy, na.rm = TRUE))

            updatePickerInput(session = session, "filter_province", 
                              choices = dengue_dta() |> pull(province) |> unique(),
                              selected = dengue_dta() |> pull(province) |> unique())

            showTab("tabs", target = "time_trends")
            showTab("tabs", target = "patients_info")
            showTab("tabs", target = "lab_results")
        }
    })
    
    # misc ----
    file_list <- list.files(path = "./www/R/outputs", pattern = "*.R", recursive = TRUE)
    for (file in file_list) source(paste0("./www/R/outputs/", file), local = TRUE)$value
}    

shinyApp(ui = ui, server = server)
