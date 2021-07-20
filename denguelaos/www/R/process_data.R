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
  rename(ward = description) %>%
  replace_na(list(ward = "Unknown ward"))

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
                    mutate(village_code = as.character(village_code)), 
                  by = c("patient_village" = "village_code")) %>%
  replace_na(list(village = "Unknown village"))

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
                  by = c("patient_district" = "district_code")) %>%
  replace_na(list(district = "Unknown district"))


# Obtain province data ----
ukn_province <- unique(data$patient_province)[which(! unique(data$patient_province) %in% province_code$code)] %>%
  na.omit()

if (is_empty(ukn_province)) {
  checklist_status$province_list <- list(status = "okay", details = "All provinces in the dataset are in the 'province code' sheet")
} else { 
  checklist_status$province_list <- list(status = "warning", details = paste0("The following provinces are not in the 'province code' sheet: ", 
                                                                              paste(ukn_province, collapse = ", ")))}

data <- left_join(data %>%
                    mutate(patient_province = as.character(patient_province)), 
                  province_code %>% 
                    mutate(code = as.character(code)), 
                  by = c("patient_province" = "code")) %>%
  replace_na(list(province = "Unknown province"))

# Age Category ----
data <- data %>%
  mutate(age_category = case_when(
    age_in_years < 5 ~ "Under 5 y.o.",
    (age_in_years >= 5 & age_in_years < 15) ~ "5 to 15 y.o.",
    age_in_years >= 15 ~ "Above 15 y.o.",
    TRUE ~ "Unknown")) %>%
  mutate(age_category = factor(age_category, levels = c("Under 5 y.o.", "5 to 15 y.o.", "Above 15 y.o.", "Unknown")))

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
