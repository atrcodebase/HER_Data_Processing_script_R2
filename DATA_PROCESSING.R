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
# install.packages("plyr") # Don't load, will affect rename functions
# if(!require(cleaninginspectoR)) remotes::install_github("ellieallien/cleaninginspectoR")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Read data ----------------------------------------------------------------------------------------
# file.edit("R/read_data.R")
source("R/read_data.R") # read data

# Rename problematic column
t2_data <- t2_data %>% 
  rename(ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4.1=
           ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1...375, # 0s/1s column
         ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1=
           ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1...437 # Main question
  )

# read qa-log, correction log, and translation log -------------------------------------------
url1 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTod-F0fcKUp28i0e0mM_Wm5FiwiyUjtnBw-BfMnDIabgfAVeCiPpYdvaNFc1TurMRXxguXJZ8L6Pbd/pub?" # Tools 1.1, 1.2, 1.3, 3
url2 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQsMoH1gFF-hH_X5A-0ZmPTrsH5GRwskbmsCqF6FjvsaaZKfenHaka1wcHaxMCCnbQCJHXWWryBlWAy/pub?" # Tool 2
qa_log1 <- readr::read_csv(paste0(url1, "gid=473078450&single=true&output=csv"), col_types = "c")
qa_log2 <- readr::read_csv(paste0(url2, "gid=473078450&single=true&output=csv"), col_types = "c")
correction_log1.1 <- readr::read_csv(paste0(url1, "gid=758688462&single=true&output=csv"), col_types = "c")
correction_log1.2 <- readr::read_csv(paste0(url1, "gid=395941858&single=true&output=csv"), col_types = "c")
correction_log1.3 <- readr::read_csv(paste0(url1, "gid=280794209&single=true&output=csv"), col_types = "c")
correction_log3 <- readr::read_csv(paste0(url1, "gid=797781638&single=true&output=csv"), col_types = "c")
correction_log2 <- readr::read_csv(paste0(url2, "gid=758688462&single=true&output=csv"), col_types = "c")
rejection_log1 <- readr::read_csv(paste0(url1, "gid=1224153730&single=true&output=csv"), col_types = "c")
rejection_log2 <- readr::read_csv(paste0(url2, "gid=1722425176&single=true&output=csv"), col_types = "c")
translation_log <- readr::read_csv(paste0(url1, "gid=187815952&single=true&output=csv"), col_types = "c")

# Merge logs
qa_log <- plyr::rbind.fill(qa_log1, qa_log2 %>% mutate(Tool="Tool 2"))
correction_log <- plyr::rbind.fill(
  correction_log1.1 %>% mutate(Tool="Tool 1.1") %>% rename(KEY=`Full_ KEY`),
  correction_log1.2 %>% mutate(Tool="Tool 1.2") %>% rename(KEY=`Full_KEY`),
  correction_log1.3 %>% mutate(Tool="Tool 1.3"),
  correction_log3 %>% mutate(Tool="Tool 3") %>% rename(KEY=`Unique KEY`),
  correction_log2 %>% mutate(Tool="Tool 2") %>% rename(KEY=UNIQUE_KEY)
)
rejection_log <- plyr::rbind.fill(
  rejection_log1,
  rejection_log2
)

# rm(url1, url2, url3, correction_log1.1, correction_log1.2, correction_log1.3, correction_log3, 
#    correction_log2, qa_log1, qa_log2)
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
correction_log %>% count(Tool, Sheet_Name)
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R") 
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
translation_log %>% count(Tool)
# file.edit("R/apply_translation_log.R")
source("R/apply_translation_log.R")
if(nrow(translation_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Recode ------------------------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R") # Check Village_Enlglish in Tool 2, 3

# produce qa-backlog -------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(Tool, qa_status=`Final QA Status`, KEY)
## Filter
QA_backlog_keys <- rbind(
  left_join(
    hf_t1_data %>% select(SubmissionDate, review_status, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="1.1"),
  left_join(
    hf_t2_data %>% select(SubmissionDate, review_status, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="1.2"),
  left_join(
    hf_t3_data %>% select(SubmissionDate, review_status, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="1.3"),
  left_join(
    t2_data %>% select(SubmissionDate, review_status, KEY), 
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="2"),
  left_join(
    t3_data %>% select(SubmissionDate, review_status, KEY), 
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="3")) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>% 
  filter(qa_status %notin% c("Approved", "Rejected") & review_status != "REJECTED") # Filter Keys not yet QAed
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
approved_qa_status <- "Approved"
# file.edit("R/filter_approved_data.R")
source("R/filter_approved_data.R")

# ## Join missing photos  ----------------------------------------------------------------------------
# # file.edit("R/join_infra_photos.R")
# source("R/join_infra_photos.R")

## Red Flags ---------------------------------------------------------------------------------------
# file.edit("R/Red_Flags.R")
# source("R/Red_Flags.R")

## Logic check -------------------------------------------------------------------------------------
# file.edit("R/logic_check.R")
source("R/logic_check.R") 

# # Split Infra dataset ----------------------------------------------------------------------------
# # file.edit("R/split_infra_data.R")
# source("R/split_infra_data.R")

## Compare dataset responses with the Tools --------------------------------------------------------
# file.edit("R/dataset_responses_check.R")
source("R/dataset_responses_check.R")

## Remove Extra columns ----------------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/remove_extra_columns.R")

# generate data with missing translations ----------------------------------------------------------
# file.edit("R/check_missing_translation.R")
source("R/check_missing_translation.R")

# Export -------------------------------------------------------------------------------------------
## Tool 1.1
hf_t1_list <- list(
  data=hf_t1_data,
  Injuries_Details=hf_injuries,
  Fatalities_Details=hf_fatalities,
  Incidents=hf_incidents
)
## Tool 1.2
hf_t2_list <- list(
  data=hf_t2_data,
  Photos_Of_Handwashing_Stations=hf_t2_photos
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
# ## Tool 4
# t4_list <- list(
#   data=infra_data,
#   Representatives=infra_rep,
#   Documents=infra_doc,
#   Environmental_And_Social_Sta...=infra_env,
#   Features=infra_feat,
#   Elements=infra_elem
# )
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
# write.xlsx(t4_list, "output/cleaned_data/HER_HF_INFRASTRUCTURE_cleaned.xlsx")
# writexl::write_xlsx(t4_list, "output/cleaned_data/HER_HF_INFRASTRUCTURE_Approved.xlsx", format_headers = F)
# writexl::write_xlsx(infra_checklist, "output/cleaned_data/HER_Health_Facility_Infrastructure_Checklist.xlsx", format_headers = F)

## export client datasets
check_path("output/client_data")
export_datasets(list(data=hf_t1_data_wide), "output/client_data/HER_ESS_Tool_1.1_Health_Facility_Level_cleaned_approved.xlsx")
export_datasets(list(data=hf_t2_data_wide), "output/client_data/HER_ESS_Tool_1.2_Health_Facility_Level_cleaned_approved.xlsx")
export_datasets(hf_t3_list_filtered, "output/client_data/HER_ESS_Tool_1.3_NC_Interview_cleaned_approved.xlsx", header_color = "#91CBD9", text_dec="")
export_datasets(t2_list_filtered, "output/client_data/HER_ESS_Tool_2_Household_Level_Surveys_cleaned_approved.xlsx", header_color = "#91CBD9")
export_datasets(t3_list_filtered, "output/client_data/HER_ESS_Tool_3_Community_Level_Surveys_cleaned_approved.xlsx", header_color = "#91CBD9")

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

