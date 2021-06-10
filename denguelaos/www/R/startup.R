app_version <- "prototype.01"

library(bslib)  # bs_theme()
library(janitor)  # clean_names()
library(readxl)
library(shiny)
library(shiny.i18n)  # i18n$t()

i18n <- Translator$new(translation_csvs_path = './www/translations/')
i18n$set_translation_language('en')

app_theme_en <- bs_theme(bootswatch = "cosmo", version = 4)
app_theme_la <- bs_theme(bootswatch = "cosmo", version = 4, base_font = "Phetsarath OT")

for(file in list.files('./www/R/functions/'))  source(paste0('./www/R/functions/', file), local = TRUE)