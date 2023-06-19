# Import data ----
excel_files <- input$files_excel$datapath

data <- try(
  purrr::map(
    excel_files,
    read_xlsx,
    sheet = "Data",
    na = c("Missing data", "missing data")
  ) |> 
    purrr::list_rbind()
)

ward <- try(
  purrr::map(
    excel_files,
    read_xlsx,
    sheet = "ward list"
  ) |> 
    purrr::list_rbind() |> 
    dplyr::distinct(WARD, .keep_all = TRUE)
)

village_code <- try(
  purrr::map(
    excel_files,
    read_xlsx,
    sheet = "village code",
  ) |> 
    purrr::list_rbind() |> 
    dplyr::distinct(VillageCode, .keep_all = TRUE)
)

district_code <- try(
  purrr::map(
    excel_files,
    read_xlsx,
    sheet = "disctrict code",
  ) |> 
    purrr::list_rbind() |> 
    dplyr::distinct(DistrictCode, .keep_all = TRUE)
)

province_code <- try(
  purrr::map(
    excel_files,
    read_xlsx,
    sheet = "province code",
  ) |> 
    purrr::list_rbind() |> 
    dplyr::distinct(Code, .keep_all = TRUE)
)

if (inherits(data, 'try-error')) {
  checklist_status$read_sheet_1 <- list(status = "ko", details = "Can't read the sheet 'Data'")
  data_summary$status <- "ko"
  return() } else { checklist_status$read_sheet_1 <- list(status = "okay", details = "Sheet 'Data' read") }

if (inherits(ward, 'try-error')) {
  checklist_status$read_sheet_2 <- list(status = "ko", details = "Can't read the sheet 'ward list'")
  data_summary$status <- "ko"
  return() } else { checklist_status$read_sheet_2 <- list(status = "okay", details = "Sheet 'ward list' read") }

if (inherits(village_code, 'try-error')) {
  checklist_status$read_sheet_3 <- list(status = "ko", details = "Can't read the sheet 'village code'")
  data_summary$status <- "ko"
  return() } else { checklist_status$read_sheet_3 <- list(status = "okay", details = "Sheet 'village code' read") }

if (inherits(district_code, 'try-error')) {
  checklist_status$read_sheet_4 <- list(status = "ko", details = "Can't read the sheet 'disctrict code'")
  data_summary$status <- "ko"
  return() } else { checklist_status$read_sheet_4 <- list(status = "okay", details = "Sheet 'disctrict code' read") }

if (inherits(province_code, 'try-error')) {
  checklist_status$read_sheet_5 <- list(status = "ko", details = "Can't read the sheet 'province code'")
  data_summary$status <- "ko"
  return() } else { checklist_status$read_sheet_5 <- list(status = "okay", details = "Sheet 'province code' read") }


# Rename and check data names ----
data <- clean_names(data)
if (all(names(data) == c("no", "ward", "institute_lab_id", "gender", "age_in_years", 
                 "patient_village", "patient_district", "patient_province", "onset_date_dd_mm_yy", 
                 "collected_date_dd_mm_yy", "received_date_dd_mm_yy", "elisa_ns1_test_date_dd_mm_yy", 
                 "elisa_ns1_method", "elisa_ns1_test_result", "elisa_ig_m_test_date_dd_mm_yy", 
                 "elisa_ig_m_method", "elisa_ig_m_test_result", "rdt_test_date_dd_mm_yy", 
                 "rdt_method", "rdt_ns1_result", "rdt_ig_g_result", "rdt_ig_m_result", 
                 "pcr_test_date_dd_mm_yy", "pcr_method", "pcr_reference", "pcr_result", 
                 "serotype_pcr_test_date_dd_mm_yy", "serotype_pcr_method", "serotype_pcr_reference", 
                 "pcr_serortype_result", "remark"))) {
  checklist_status$names_sheet_1 <- list(status = "okay", details = "Column names of the sheet 'Data' are as expected.")
} else { 
  checklist_status$names_sheet_1 <- list(status = "ko", details = "Column names of the sheet 'Data' are different from expected.") 
  data_summary$status <- "ko" }

ward <- clean_names(ward)
if (all(names(ward) == c("ward", "description"))) {
  checklist_status$names_sheet_2 <- list(status = "okay", details = "Column names of the sheet 'ward list' are as expected.")
} else { 
  checklist_status$names_sheet_2 <- list(status = "ko", details = "Column names of the sheet 'ward list' are different from expected.") 
  data_summary$status <- "ko" }

village_code <- clean_names(village_code)
if (all(names(village_code) == c("id", "village_code", "village", "district_code", "lattitude", "longtitude"))) {
  checklist_status$names_sheet_3 <- list(status = "okay", details = "Column names of the sheet 'village code' are as expected.")
} else { 
  checklist_status$names_sheet_3 <- list(status = "ko", details = "Column names of the sheet 'village code' are different from expected.") 
  data_summary$status <- "ko" }

district_code <- clean_names(district_code)
if (all(names(district_code) == c("id", "district_code", "district", "province_code"))) {
  checklist_status$names_sheet_4 <- list(status = "okay", details = "Column names of the sheet 'district code' are as expected.")
} else { 
  checklist_status$names_sheet_4 <- list(status = "ko", details = "Column names of the sheet 'district code' are different from expected.") 
  data_summary$status <- "ko" }

province_code <- clean_names(province_code)
if (all(names(province_code) == c("province", "code"))) {
  checklist_status$names_sheet_5 <- list(status = "okay", details = "Column names of the sheet 'province code' are as expected.")
} else { 
  checklist_status$names_sheet_5 <- list(status = "ko", details = "Column names of the sheet 'province code' are different from expected.") 
  data_summary$status <- "ko" }