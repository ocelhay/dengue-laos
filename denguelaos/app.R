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
                   condition = "input.tabs != 'welcome'",
                   div("Placeholder for filers")
               ),
               
               tabPanel(i18n$t("Welcome"), value = "welcome",
                        img(src = "./images/LOMWRU_partners_logo.jpg", style = "height: 40px;"),
                        br(),
                        selectInput(
                            inputId = 'selected_language', label = span(icon('language'), i18n$t('Language:')),
                            choices = i18n$get_languages(), selected = i18n$get_key_translation(), width = "150px"
                        ),
                        fluidRow(
                            column(4,
                                   img(src = "./images/dengue_fever_symptoms.png", width = "95%", alt = "Dengue Fever Symptoms")
                            ),
                            column(8,
                                   includeMarkdown("./www/markdown/about_dengue.md")
                            )
                        ),
                        
                        strong(icon("upload"), "Upload Data (.xlsx format)"),
                        p("For more info on data format, ", 
                          tags$a(href = "mailto:olivier.celhay@gmail.com", "contact us.")),
                        fileInput("file_excel", label = NULL, accept = ".xlsx", buttonLabel = "Browse...")
               ),
               
               tabPanel(i18n$t("Epidemic Trends"), value = "epidemic_trends",
                        div("Placeholder for ET content")
               )
    )
)


server <- function(input, output, session) {
    
    # On app launch ----
    hideTab(inputId = "tabs", target = "epidemic_trends")
    
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
        
        showModal(modalDialog(
            title = "Reading file and checking file quality.", footer = modalButton("Okay"), size = "l",
            div(
                htmlOutput("checklist_data"),
                br(),
                htmlOutput("checklist_data_summary")
            )
        ))
        
        if(data_summary$status == "okay") {
            showTab(inputId = "tabs", target = "epidemic_trends")
        }
    })
    
    
    # Reactive data management ----
    checklist_status <- reactiveValues()
    data_summary <- reactiveValues(status = "okay")
    
    # misc ----
    file_list <- list.files(path = "./www/R/outputs", pattern = "*.R", recursive = TRUE)
    for (file in file_list) source(paste0("./www/R/outputs/", file), local = TRUE)$value
}    

shinyApp(ui = ui, server = server)
