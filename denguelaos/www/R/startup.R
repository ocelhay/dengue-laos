app_version <- "v0.3"

library(bslib)  # bs_theme()
library(DiagrammeR)
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


lang <- data.frame(
  val = c("en", "la"),
  img = c(
    "<img src = './images/flags/gb.png' width = 20px><div class='jhr'>English</div></img>",
    "<img src = './images/flags/la.png' width = 20px><div class='jhr'>Lao</div></img>"
  )
)

i18n <- Translator$new(translation_csvs_path = './www/translations/')
i18n$set_translation_language('en')

app_theme_en <- bslib::bs_theme(bootswatch = "flatly", version = 4)
app_theme_la <- bslib::bs_theme(bootswatch = "flatly", version = 4, base_font = "Phetsarath OT")

cols <- c('#92c5de', '#f4a582', '#ca0020')  # Confirmed / Presumptive / No evidence
cols_nep <- c("#d9d9d9", "#a1d76a", "#e9a3c9") # pos / equivocal / neg

hc_export_kind <- c("downloadJPEG", "downloadCSV")

for(file in list.files('./www/R/functions/'))  source(paste0('./www/R/functions/', file), local = TRUE)