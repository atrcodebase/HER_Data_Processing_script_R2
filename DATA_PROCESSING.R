##### Data Processing Script #####
# Install/load required packages -------------------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(janitor)) install.packages("janitor")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Read data ----------------------------------------------------------------------------------------
# file.edit("R/read_data.R")
source("R/read_data.R") # read data

# Rename problematic column
hf_t1_data <- hf_t1_data %>% 
  select(-Who_Provides_Referrals_For_The_Receipt_Of_Nutrition_Services_In_This_Health_Facility_6...357,
         -Do_You_Face_Issues_That_Would_Prevent_You_From_Working_Effectively_At_This_Health_Facility_Other) %>% # Removed in tool but was still in dataset
  rename(Who_Provides_Referrals_For_The_Receipt_Of_Nutrition_Services_In_This_Health_Facility_6=Who_Provides_Referrals_For_The_Receipt_Of_Nutrition_Services_In_This_Health_Facility_6...358)
t2_data <- t2_data %>% 
  rename(ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4.1=
           ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1...252, # 0s/1s column
         ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1=
           ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1...313 # Main question
  )

# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSa9Kqouso7Oq0CfebhpP7fgO3WxRoOdjyOcniLVoCMDTmwv9rK3GICDzsAvrH4iitc_tdJxS-yF4F4/pub?" 
url2 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS_lU37EeYithWp_cTKsjAcFCXm8w8MTB7gPolFASyF8-MhvRNh-0BHu6VUiUL-YSJiM9gsPeVrXHgg/pub?" 
qa_log <- readr::read_csv(paste0(url, "gid=473078450&single=true&output=csv"), col_types = "c")
correction_log <- readr::read_csv(paste0(url, "gid=758688462&single=true&output=csv"), col_types = "c")
rejection_log <- readr::read_csv(paste0(url, "gid=1224153730&single=true&output=csv"), col_types = "c")
translation_log <- readr::read_csv(paste0(url2, "gid=0&single=true&output=csv"), col_types = "c")

# Join QA Status -----------------------------------------------------------------------------------
count(qa_log, Tool, `Final QA Status`)
qa_log_sub <- qa_log %>% 
  select(KEY, qa_status=`Final QA Status`, Tool) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status
  )) %>% unique()

## Tool 1.1
hf_t1_data <- hf_t1_data %>%
  left_join(filter(qa_log_sub, Tool=="Tool 1.1"), by="KEY") %>% select(-Tool)
## Tool 1.2
hf_t2_data <- hf_t2_data %>%
  left_join(filter(qa_log_sub, Tool=="Tool 1.2"), by="KEY") %>% select(-Tool)
## Tool 1.3
hf_t3_data <- hf_t3_data %>% 
  left_join(filter(qa_log_sub, Tool=="Tool 1.3"), by="KEY") %>% select(-Tool)
## Tool 2
t2_data <- t2_data %>% 
  left_join(filter(qa_log_sub, Tool=="Tool 2"), by="KEY") %>% select(-Tool)
## Tool 3
t3_data <- t3_data %>% 
  left_join(filter(qa_log_sub, Tool=="Tool 3"), by="KEY") %>% select(-Tool)

# apply correction log -----------------------------------------------------------------------------
correction_log %>% count(Tools, Sheet_Name)
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R") # Check unique key
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Remove Rejected data ----------------------------------------------------------------------------
# # file.edit("R/remove_rejected_data.R")
source("R/remove_rejected_data.R")

# Relevancy check ----------------------------------------------------------------------------------
# file.edit("R/check_relevancy_rules.R")
source("R/check_relevancy_rules.R")

## Attach labels -----------------------------------------------------------------------------------
# file.edit("R/attach_labels.R")
source("R/attach_labels.R")

# apply Translation log ----------------------------------------------------------------------------
translation_log %>% count(Tool, `Tab Name`)
# file.edit("R/apply_translation_log.R")
source("R/apply_translation_log.R")
if(nrow(translation_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Recode ------------------------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R") 

# produce qa-backlog -------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(Tool, qa_status=`Final QA Status`, KEY)
## Filter
QA_backlog_keys <- rbind(
  left_join(
    hf_t1_data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="1.1"),
  left_join(
    hf_t2_data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="1.2"),
  left_join(
    hf_t3_data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="1.3"),
  left_join(
    t2_data %>% select(SubmissionDate, KEY), 
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="2"),
  left_join(
    t3_data %>% select(SubmissionDate, KEY), 
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="3")) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>% 
  filter(qa_status %notin% c("Approved", "Excel Check Approved", "Rejected")) # Filter Keys not yet QAed
# Count
QA_backlog <- QA_backlog_keys %>% 
  group_by(SubmissionDate, Tool) %>% count(qa_status, name = "freq") %>% 
  # mutate(percentage = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% arrange(SubmissionDate) %>% 
  pivot_wider(names_from = "Tool", values_from = "freq")
# Print
print(knitr::kable(QA_backlog, format = "simple"))

## Merge sheets ------------------------------------------------------------------------------------
# file.edit("R/convert_to_wide_format.R")
source("R/convert_to_wide_format.R")

## Filter Approved data ----------------------------------------------------------------------------
approved_qa_status <- c("Approved", "Excel Check Approved")
# file.edit("R/filter_approved_data.R")
source("R/filter_approved_data.R")

## Red Flags ---------------------------------------------------------------------------------------
# file.edit("R/Red_Flags.R")
# source("R/Red_Flags.R") # Check with Daniel

## Logic check -------------------------------------------------------------------------------------
# file.edit("R/logic_check.R")
source("R/logic_check.R")

## Compare dataset responses with the Tools --------------------------------------------------------
# file.edit("R/dataset_responses_check.R")
source("R/dataset_responses_check.R")

## Remove Extra columns ----------------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/remove_extra_columns.R")

# generate data with missing translations ----------------------------------------------------------
# file.edit("R/check_missing_translation.R")
source("R/check_missing_translation.R") # Temorary filter for QA at the end

# Export -------------------------------------------------------------------------------------------
## Tool 1.1
hf_t1_list <- list(
  data=hf_t1_data,
  Injuries_Details=hf_injuries,
  Fatalities_Details=hf_fatalities,
  Incidents=hf_incidents
)
hf_t1_list_filtered <- list(
  data=hf_t1_data_filtered,
  Injuries_Details=hf_injuries_filtered,
  Fatalities_Details=hf_fatalities_filtered,
  Incidents=hf_incidents_filtered
)
## Tool 1.2
hf_t2_list <- list(
  data=hf_t2_data,
  Photos_Of_Handwashing_Stations=hf_t2_photos
)
hf_t2_list_filtered <- list(
  data=hf_t2_data_filtered,
  Photos_Of_Handwashing_Stations=hf_t2_photos_filtered
)
## Tool 1.3
hf_t3_list <- list(
  data=hf_t3_data
)
hf_t3_list_filtered <- list(
  data=hf_t3_data_filtered
)
## Tool 2
t2_list <- list(
  data=t2_data,
  Income_Earning_Members_Details=t2_income,
  Illness_Details=t2_illness,
  Injuries_Details=t2_injuries,
  Immunization_Details=t2_immunization,
  Section_B6_Other_Group=t2_other
)
t2_list_filtered <- list(
  data=t2_data_filtered,
  Income_Earning_Members_Details=t2_income_filtered,
  Illness_Details=t2_illness_filtered,
  Injuries_Details=t2_injuries_filtered,
  Immunization_Details=t2_immunization_filtered,
  Section_B6_Other_Group=t2_other_filtered
)
## Tool 3
t3_list <- list(
  data=t3_data
)
t3_list_filtered <- list(
  data=t3_data_filtered
)

## QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)

## create the output path
check_path("output/cleaned_data")
## export cleaned datasets
writexl::write_xlsx(hf_t1_list, "output/cleaned_data/HER_ESS_Tool_1.1_Health_Facility_Level_cleaned.xlsx", format_headers = F)
writexl::write_xlsx(hf_t2_list, "output/cleaned_data/HER_ESS_Tool_1.2_Health_Facility_Level_cleaned.xlsx", format_headers = F)
writexl::write_xlsx(hf_t3_list, "output/cleaned_data/HER_ESS_Tool_1.3_NC_Interview_cleaned.xlsx", format_headers = F)
writexl::write_xlsx(t2_list, "output/cleaned_data/HER_ESS_Tool_2_Household_Level_Surveys_cleaned.xlsx", format_headers = F)
writexl::write_xlsx(t3_list, "output/cleaned_data/HER_ESS_Tool_3_Community_Level_Surveys_cleaned.xlsx", format_headers = F)

## export client datasets
check_path("output/client_data")
header_color <- "#91CBD9"
export_datasets(hf_t1_list_filtered, "output/client_data/HER_ESS_Tool_1.1_Health_Facility_Level_cleaned_approved.xlsx", header_color = header_color)
export_datasets(hf_t2_list_filtered, "output/client_data/HER_ESS_Tool_1.2_Health_Facility_Level_cleaned_approved.xlsx", header_color = header_color)
export_datasets(hf_t3_list_filtered, "output/client_data/HER_ESS_Tool_1.3_NC_Interview_cleaned_approved.xlsx", header_color = header_color)
export_datasets(t2_list_filtered, "output/client_data/HER_ESS_Tool_2_Household_Level_Surveys_cleaned_approved.xlsx", header_color = header_color)
export_datasets(t3_list_filtered, "output/client_data/HER_ESS_Tool_3_Community_Level_Surveys_cleaned_approved.xlsx", header_color = header_color)

## export additional files
writexl::write_xlsx(correction_log, "output/correction_log.xlsx", format_headers = F) # correction
writexl::write_xlsx(correction_log_issues, "output/correction_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(translation_log_issues, "output/translation_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)
writexl::write_xlsx(relevancy_issues, "output/relevancy_issues.xlsx", format_headers = F)
writexl::write_xlsx(SM_issues, "output/Select_multiple_issues.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_QA_log, "output/Missing_audio_translation_&_image_QA.xlsx", format_headers = F)
writexl::write_xlsx(qa_backlog_list, "output/QA_backlog.xlsx", format_headers = F)
writexl::write_xlsx(response_log_list, "output/dataset_response_mismatch_with_tool.xlsx", format_headers = F)
writexl::write_xlsx(logical_issues_list, "output/Logical_issues.xlsx", format_headers = F)
writexl::write_xlsx(tool3_interview_issue, "output/Tool3_interview_issue.xlsx", format_headers = F)

