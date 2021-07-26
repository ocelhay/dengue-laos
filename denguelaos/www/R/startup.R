app_version <- "prototype.01"

library(bslib)  # bs_theme()
library(DT)
library(flexdashboard)  # gaugeOutput()
library(glue)
library(highcharter)
library(janitor)  # clean_names()
library(leaflet)
library(lubridate)
library(readxl)
library(shiny)
library(shinyWidgets)  # prettySwitch()
library(shiny.i18n)  # i18n$t()
library(tidyverse)

i18n <- Translator$new(translation_csvs_path = './www/translations/')
i18n$set_translation_language('en')

app_theme_en <- bs_theme(bootswatch = "cosmo", version = 4)
app_theme_la <- bs_theme(bootswatch = "cosmo", version = 4, base_font = "Phetsarath OT")

# Colors for Confirmed / Presumptive / No evidence:
# cols <- c('#ca0020', '#f4a582', '#92c5de')

# Colors for Confirmed / Presumptive / No evidence:
cols <- c("#92c5de", "#f4a582", "#ca0020")

for(file in list.files('./www/R/functions/'))  source(paste0('./www/R/functions/', file), local = TRUE)