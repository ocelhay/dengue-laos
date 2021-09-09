# Obtain ward description ----
ukn_ward <- unique(data$ward)[which(! unique(data$ward) %in% ward$ward)] %>%
  na.omit()

if (is_empty(ukn_ward)) {
  checklist_status$ward_list <- list(status = "okay", details = "All wards in the dataset are in the ward list")
} else { 
  checklist_status$ward_list <- list(status = "warning", details = paste0("The following wards are not in the 'ward list': ", 
                                                                          paste(ukn_ward, collapse = ", ")))}

data <- left_join(data, ward, by = "ward") %>%
  select(-ward) %>%
  rename(ward = description)

# Obtain village data ----
ukn_village <- unique(data$patient_village)[which(! unique(data$patient_village) %in% village_code$village_code)] %>%
  na.omit()

if (is_empty(ukn_village)) {
  checklist_status$village_list <- list(status = "okay", details = "All villages in the dataset are in the 'village code' sheet")
} else { 
  checklist_status$village_list <- list(status = "warning", details = paste0("The following villages are not in the 'village code' sheet': ", 
                                                                             paste(ukn_village, collapse = ", ")))}

data <- left_join(data, 
                  village_code %>% 
                    select(-id, -district_code) %>%
                    mutate(village_code = as.character(village_code)) %>%
                    rename(longitude = longtitude, latitude = lattitude), 
                  by = c("patient_village" = "village_code"))

# Obtain district data ----
ukn_district <- unique(data$patient_district)[which(! unique(data$patient_district) %in% district_code$district_code)] %>%
  na.omit()

if (is_empty(ukn_district)) {
  checklist_status$district_list <- list(status = "okay", details = "All districts in the dataset are in the 'district code' sheet")
} else { 
  checklist_status$district_list <- list(status = "warning", details = paste0("The following districts are not in the 'district code' sheet: ", 
                                                                              paste(ukn_district, collapse = ", ")))}

data <- left_join(data, 
                  district_code %>% 
                    select(-id, -province_code) %>%
                    mutate(district_code = as.character(district_code)), 
                  by = c("patient_district" = "district_code"))


# Obtain province data ----
ukn_province <- unique(data$patient_province)[which(! unique(data$patient_province) %in% province_code$code)] %>%
  na.omit()

if (is_empty(ukn_province)) {
  checklist_status$province_list <- list(status = "okay", details = "All provinces in the dataset are in the 'province code' sheet")
} else { 
  checklist_status$province_list <- list(status = "warning", details = paste0("The following provinces are not in the 'province code' sheet: ", 
                                                                              paste(ukn_province, collapse = ", ")))}

# Check that dates are consistent ----
# all dates columns in data: 
# onset_date_dd_mm_yy, 
# collected_date_dd_mm_yy, received_date_dd_mm_yy, elisa_ns1_test_date_dd_mm_yy,
# elisa_ig_m_test_date_dd_mm_yy, rdt_test_date_dd_mm_yy, pcr_test_date_dd_mm_yy, serotype_pcr_test_date_dd_mm_yy

## Toutes les dates doivent être postérieures ou égales à « Onset Date » ----
date_onset_after_collected <- which(data$onset_date_dd_mm_yy > data$collected_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_collected), 
        { checklist_status$date_onset_after_collected <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the date of data collection.") },
        { checklist_status$date_onset_after_collected <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the date of data collection for patient(s) no: ",
                                                                                                   paste(data$no[date_onset_after_collected], collapse = ", ")))})

date_onset_after_received <- which(data$onset_date_dd_mm_yy > data$received_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_received), 
        { checklist_status$date_onset_after_received <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the 'received date'.") },
        { checklist_status$date_onset_after_received <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the 'received date' for patient(s) no: ",
                                                                                                  paste(data$no[date_onset_after_received], collapse = ", ")))})

date_onset_after_elisa_ns1 <- which(data$onset_date_dd_mm_yy > data$elisa_ns1_test_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_elisa_ns1), 
        { checklist_status$date_onset_after_elisa_ns1 <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the 'Elisa NS1 date'.") },
        { checklist_status$date_onset_after_elisa_ns1 <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the 'Elisa NS1 date' for patient(s) no: ",
                                                                                                   paste(data$no[date_onset_after_elisa_ns1], collapse = ", ")))})

date_onset_after_elisa_ig_m <- which(data$onset_date_dd_mm_yy > data$elisa_ig_m_test_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_elisa_ig_m), 
        { checklist_status$date_onset_after_elisa_ig_m <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the 'Elisa IgM date'.") },
        { checklist_status$date_onset_after_elisa_ig_m <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the 'Elisa IgM date' for patient(s) no: ",
                                                                                                   paste(data$no[date_onset_after_elisa_ig_m], collapse = ", ")))})

date_onset_after_rdt_test <- which(data$onset_date_dd_mm_yy > data$rdt_test_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_rdt_test), 
        { checklist_status$date_onset_after_rdt_test <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the 'RDT test date'.") },
        { checklist_status$date_onset_after_rdt_test <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the 'RDT test date' for patient(s) no: ",
                                                                                                    paste(data$no[date_onset_after_rdt_test], collapse = ", ")))})

date_onset_after_pcr_test <- which(data$onset_date_dd_mm_yy > data$pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_pcr_test), 
        { checklist_status$date_onset_after_pcr_test <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the 'PCR test date'.") },
        { checklist_status$date_onset_after_pcr_test <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the 'PCR test date' for patient(s) no: ",
                                                                                                  paste(data$no[date_onset_after_pcr_test], collapse = ", ")))})

date_onset_after_sero_pcr_test <- which(data$onset_date_dd_mm_yy > data$serotype_pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_onset_after_sero_pcr_test), 
        { checklist_status$date_onset_after_sero_pcr_test <- list(status = "okay",    details = "All dates of symptom onsets are prior or equal to the 'Serotype PCR test date'.") },
        { checklist_status$date_onset_after_sero_pcr_test <- list(status = "warning", details = paste0("Date of symptom onset is posterior to the 'Serotype PCR test date' for patient(s) no: ",
                                                                                                  paste(data$no[date_onset_after_sero_pcr_test], collapse = ", ")))})



## La date « Received Date » et toutes les dates de test doivent être postérieures ou égales à « Collected Date » ----
date_collected_after_received <- which(data$collected_date_dd_mm_yy > data$received_date_dd_mm_yy)
ifelse (is_empty(date_collected_after_received), 
        { checklist_status$date_collected_after_received <- list(status = "okay",    details = "All 'collected date' are prior or equal to the 'received date'.") },
        { checklist_status$date_collected_after_received <- list(status = "warning", details = paste0("'collected date' is posterior to the 'received date' for patient(s) no: ",
                                                                                                  paste(data$no[date_collected_after_received], collapse = ", ")))})

date_collected_after_elisa_ns1 <- which(data$collected_date_dd_mm_yy > data$elisa_ns1_test_date_dd_mm_yy)
ifelse (is_empty(date_collected_after_elisa_ns1), 
        { checklist_status$date_collected_after_elisa_ns1 <- list(status = "okay",    details = "All 'collected date' are prior or equal to the 'Elisa NS1 date'.") },
        { checklist_status$date_collected_after_elisa_ns1 <- list(status = "warning", details = paste0("'collected date' is posterior to the 'Elisa NS1 date' for patient(s) no: ",
                                                                                                   paste(data$no[date_collected_after_elisa_ns1], collapse = ", ")))})

date_collected_after_elisa_ig_m <- which(data$collected_date_dd_mm_yy > data$elisa_ig_m_test_date_dd_mm_yy)
ifelse (is_empty(date_collected_after_elisa_ig_m), 
        { checklist_status$date_collected_after_elisa_ig_m <- list(status = "okay",    details = "All 'collected date' are prior or equal to the 'Elisa IgM date'.") },
        { checklist_status$date_collected_after_elisa_ig_m <- list(status = "warning", details = paste0("'collected date' is posterior to the 'Elisa IgM date' for patient(s) no: ",
                                                                                                    paste(data$no[date_collected_after_elisa_ig_m], collapse = ", ")))})

date_collected_after_rdt_test <- which(data$collected_date_dd_mm_yy > data$rdt_test_date_dd_mm_yy)
ifelse (is_empty(date_collected_after_rdt_test), 
        { checklist_status$date_collected_after_rdt_test <- list(status = "okay",    details = "All 'collected date' are prior or equal to the 'RDT test date'.") },
        { checklist_status$date_collected_after_rdt_test <- list(status = "warning", details = paste0("'collected date' is posterior to the 'RDT test date' for patient(s) no: ",
                                                                                                  paste(data$no[date_collected_after_rdt_test], collapse = ", ")))})

date_collected_after_pcr_test <- which(data$collected_date_dd_mm_yy > data$pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_collected_after_pcr_test), 
        { checklist_status$date_collected_after_pcr_test <- list(status = "okay",    details = "All 'collected date' are prior or equal to the 'PCR test date'.") },
        { checklist_status$date_collected_after_pcr_test <- list(status = "warning", details = paste0("'collected date' is posterior to the 'PCR test date' for patient(s) no: ",
                                                                                                  paste(data$no[date_collected_after_pcr_test], collapse = ", ")))})

date_collected_after_sero_pcr_test <- which(data$collected_date_dd_mm_yy > data$serotype_pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_collected_after_sero_pcr_test), 
        { checklist_status$date_collected_after_sero_pcr_test <- list(status = "okay",    details = "All 'collected date' are prior or equal to the 'Serotype PCR test date'.") },
        { checklist_status$date_collected_after_sero_pcr_test <- list(status = "warning", details = paste0("'collected date' is posterior to the 'Serotype PCR test date' for patient(s) no: ",
                                                                                                       paste(data$no[date_collected_after_sero_pcr_test], collapse = ", ")))})

## Toutes les dates de test doivent être postérieures ou égales à « Received Date » ----

date_received_after_elisa_ns1 <- which(data$received_date_dd_mm_yy > data$elisa_ns1_test_date_dd_mm_yy)
ifelse (is_empty(date_received_after_elisa_ns1), 
        { checklist_status$date_received_after_elisa_ns1 <- list(status = "okay",    details = "All 'received date' are prior or equal to the 'Elisa NS1 date'.") },
        { checklist_status$date_received_after_elisa_ns1 <- list(status = "warning", details = paste0("'received date' is posterior to the 'Elisa NS1 date' for patient(s) no: ",
                                                                                                       paste(data$no[date_received_after_elisa_ns1], collapse = ", ")))})

date_received_after_elisa_ig_m <- which(data$received_date_dd_mm_yy > data$elisa_ig_m_test_date_dd_mm_yy)
ifelse (is_empty(date_received_after_elisa_ig_m), 
        { checklist_status$date_received_after_elisa_ig_m <- list(status = "okay",    details = "All 'received date' are prior or equal to the 'Elisa IgM date'.") },
        { checklist_status$date_received_after_elisa_ig_m <- list(status = "warning", details = paste0("'received date' is posterior to the 'Elisa IgM date' for patient(s) no: ",
                                                                                                        paste(data$no[date_received_after_elisa_ig_m], collapse = ", ")))})

date_received_after_rdt_test <- which(data$received_date_dd_mm_yy > data$rdt_test_date_dd_mm_yy)
ifelse (is_empty(date_received_after_rdt_test), 
        { checklist_status$date_received_after_rdt_test <- list(status = "okay",    details = "All 'received date' are prior or equal to the 'RDT test date'.") },
        { checklist_status$date_received_after_rdt_test <- list(status = "warning", details = paste0("'received date' is posterior to the 'RDT test date' for patient(s) no: ",
                                                                                                      paste(data$no[date_received_after_rdt_test], collapse = ", ")))})

date_received_after_pcr_test <- which(data$received_date_dd_mm_yy > data$pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_received_after_pcr_test), 
        { checklist_status$date_received_after_pcr_test <- list(status = "okay",    details = "All 'received date' are prior or equal to the 'PCR test date'.") },
        { checklist_status$date_received_after_pcr_test <- list(status = "warning", details = paste0("'received date' is posterior to the 'PCR test date' for patient(s) no: ",
                                                                                                      paste(data$no[date_received_after_pcr_test], collapse = ", ")))})

date_received_after_sero_pcr_test <- which(data$received_date_dd_mm_yy > data$serotype_pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_received_after_sero_pcr_test), 
        { checklist_status$date_received_after_sero_pcr_test <- list(status = "okay",    details = "All 'received date' are prior or equal to the 'Serotype PCR test date'.") },
        { checklist_status$date_received_after_sero_pcr_test <- list(status = "warning", details = paste0("'received date' is posterior to the 'Serotype PCR test date' for patient(s) no: ",
                                                                                                           paste(data$no[date_received_after_sero_pcr_test], collapse = ", ")))})


## Ordre des dates de tests : PCR ≤ ELISA NS1 ≤ ELISA IgM ----
# Check that dates are consistent ----
# all dates columns in data: 
# onset_date_dd_mm_yy, 
# collected_date_dd_mm_yy, received_date_dd_mm_yy, elisa_ns1_test_date_dd_mm_yy,
# elisa_ig_m_test_date_dd_mm_yy, rdt_test_date_dd_mm_yy, pcr_test_date_dd_mm_yy, serotype_pcr_test_date_dd_mm_yy
date_elisa_ns1_after_pcr_test <- which(data$pcr_test_date_dd_mm_yy > data$elisa_ns1_test_date_dd_mm_yy)
ifelse (is_empty(date_elisa_ns1_after_pcr_test), 
        { checklist_status$date_elisa_ns1_after_pcr_test <- list(status = "okay",    details = "All 'PCR test date' are prior or equal to the 'ELISA NS1 test date'.") },
        { checklist_status$date_elisa_ns1_after_pcr_test <- list(status = "warning", details = paste0("'PCR test date' is posterior to the 'ELISA NS1 Test date' for patient(s) no: ",
                                                                                                     paste(data$no[date_elisa_ns1_after_pcr_test], collapse = ", ")))})

date_elisa_igm_after_ns1 <- which(data$elisa_ns1_test_date_dd_mm_yy > data$elisa_ig_m_test_date_dd_mm_yy)
ifelse (is_empty(date_elisa_igm_after_ns1), 
        { checklist_status$date_elisa_igm_after_ns1 <- list(status = "okay",    details = "All 'ELISA NS1 test date' are prior or equal to the 'ELISA IgM test date'.") },
        { checklist_status$date_elisa_igm_after_ns1 <- list(status = "warning", details = paste0("'ELISA NS1 test date' is posterior to the 'ELISA IgM test date' for patient(s) no: ",
                                                                                                      paste(data$no[date_elisa_igm_after_ns1], collapse = ", ")))})

## PCR Test date ≤ serotype PCR Test date ----
date_pcr_sero_after_pcr <- which(data$pcr_test_date_dd_mm_yy > data$serotype_pcr_test_date_dd_mm_yy)
ifelse (is_empty(date_pcr_sero_after_pcr), 
        { checklist_status$date_pcr_sero_after_pcr <- list(status = "okay",    details = "All 'PCR test date' are prior or equal to the 'Serotype PCR test date'.") },
        { checklist_status$date_pcr_sero_after_pcr <- list(status = "warning", details = paste0("'PCR test date' is posterior to the 'Serotype PCR test date' for patient(s) no: ",
                                                                                                 paste(data$no[date_pcr_sero_after_pcr], collapse = ", ")))})

# Province info ----
data <- left_join(data %>%
                    mutate(patient_province = as.character(patient_province)), 
                  province_code %>% 
                    mutate(code = as.character(code)), 
                  by = c("patient_province" = "code"))

# Age Category ----
data <- data %>%
  mutate(age_category = case_when(
    age_in_years < 5 ~ "Under 5 y.o.",
    (age_in_years >= 5 & age_in_years < 15) ~ "5 to 15 y.o.",
    age_in_years >= 15 ~ "Above 15 y.o."))

# Gender
data <- data %>%
  mutate(gender = case_when(
    gender == "M" ~ "Male",
    gender == "F" ~ "Female"
  ))

# Dengue results ----
data <- data %>% 
  mutate(dengue_virus = case_when(
    (pcr_result == "Positive" | elisa_ns1_test_result == "Positive" |  rdt_ns1_result == "Positive") ~ "Confirmed dengue infection",
    ((elisa_ig_m_test_result == "Positive" | rdt_ig_m_result == "Positive") & pcr_result == "Negative" &  rdt_ns1_result == "Negative") ~ "Presumptive dengue infection",
    TRUE ~ "No evidence of dengue infection")) %>% 
  mutate(dengue_virus = factor(dengue_virus, levels = c("Confirmed dengue infection", "Presumptive dengue infection", "No evidence of dengue infection")),
         collection_year = as.character(year(collected_date_dd_mm_yy)),
         collection_month = month(collected_date_dd_mm_yy, label = TRUE),
         collection_week = week(collected_date_dd_mm_yy),
         collection_day = day(collected_date_dd_mm_yy))

dengue_dta_with_na(data)

data <- data %>%
  replace_na(list(province = "Unknown", district = "Unknown", village = "Unknown", ward = "Unknown",
                  age_category = "Unknown", gender = "Unknown"))
