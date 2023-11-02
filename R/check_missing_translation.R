# Log missing audio translation and missing image QA -----------------------------------------------
## Tool 1.1
t1.1_tool <- read_excel("input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level.xlsx", "survey", guess_max = 100000)
t1.1_audio_cols <- t1.1_tool %>% filter(type %in% c("audio") & name %in% names(hf_t1_data)) %>% pull(name)
t1.1_image_cols <- t1.1_tool %>% filter(type %in% c("image") & name %in% names(hf_t1_data)) %>% pull(name)

t1.1_missing_log <-  rbind(
  # Translation
  log_questions(data=hf_t1_data, columns=t1.1_audio_cols, suffix="Translation", sheet="data"),
  log_questions(data=hf_injuries, columns="What_Caused_The_Injury", suffix="Translation", sheet="Injuries"),
  log_questions(data=hf_fatalities, columns="What_Caused_The_Fatality", suffix="Translation", sheet="Fatalities"),
  log_questions(data=hf_incidents, columns="Can_You_Please_Elaborate_On_The_Safety_Security_Incident_S",
                suffix="Translation", sheet="Incidents"),
  # Image QA
  log_questions(data=hf_t1_data, columns=t1.1_image_cols, suffix="QA", sheet="data")
)

## Tool 1.2
t1.2_tool <- read_excel("input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level.xlsx", "survey", guess_max = 100000)
t1.2_image_cols <- t1.2_tool %>% filter(type %in% c("image") & name %in% names(hf_t2_data)) %>% pull(name)
# Image QA cols with different spelling
t1.2_image_cols_diff <- list(
  "Please_Take_Photo_Of_The_Toilet"="Are_The_Latrines_Gender_Segregated_For_Men_And_Women_QA", # Diff
  "Please_Take_Photo_Of_The_Informations_Provided_Photo_1"="Please_Take_Photo_Of_The_Informations_Provided_Photo_QA", # Diff
  "Please_Take_Photo_Of_The_Informations_Provided_Photo_2"="Please_Take_Photo_Of_The_Informations_Provided_Photo2_QA", # Diff
  "Please_Take_Photo_Of_The_Informations_Provided_Photo_3"="Please_Take_Photo_Of_The_Informations_Provided_Photo3_QA", # Diff
  "Please_Take_Photo_Of_Collection_Receptacle_Non_Contaminated"="Please_Take_Photo_Of_Collection_Receptacle_Non_Contaminated_Caption_QA", # Diff
  "Please_Take_Photo_Of_Pathological_Disposed_In_This_Receptacle"="Please_Take_Photo_Of_Pathological_Materials_Disposed_In_This_Receptacle_QA", # Diff
  "Please_Take_Photo_Of_Collection_Receptacle_Pathological"="Please_Take_Photo_Of_Collection_Receptacle_Pathological_Caption_QA" # Diff
)

t1.2_missing_log <-  rbind(
  # Translation
  log_questions(data=hf_t2_data, columns="If_There_Is_No_Disinfection_Procedure_For_The_Above_Materials_Why_Not",
                suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=hf_t2_data, columns=t1.2_image_cols, columns_different = t1.2_image_cols_diff, suffix="QA", sheet="data"),
  log_questions(data=hf_t2_photos, columns="Please_Take_Photo_Of_The_Handwashing_Area", suffix="QA", sheet="Photos")
)

## Tool 1.3
t1.3_audio_cols <- c("Explanation_Note", "Can_You_Please_Explain_What_You_Do_Besides_Your_Job_As_A_Nutrition_Counsellor_At_This_Health_Facility", 
                     "Can_You_Explain_What_Trainings_Have_You_Received_From_Which_Entity", 
                     "Thank_You_Very_Much_For_Your_Time_And_Responding_To_My_Questions_Please_Let_Me_Know_If_You_Would_Like_To_Provide_Any_Comments_On_How_To_Improve_Nutrition_Services_And_Counselling_In_The_Health_Facility")
# Log
t1.3_missing_log <-  rbind(
  # Translation
  log_questions(data=hf_t3_data_filtered, columns=t1.3_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=hf_t3_data_filtered, columns=c("Selfie_Photo", "Explanation_Paper_Photo"), suffix="QA", sheet="data"))


## Tool 2
t2_tool <- read_excel("input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx", "survey", guess_max = 100000)
t2_audio_cols <- t2_tool %>% filter(type %in% c("audio") & name %notin% "Please_Provide_Explanation") %>% pull(name)
# Audio Translation cols with different spelling 
t2_audio_cols_diff <- list(
  "ANC_PNC_Why_Did_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit"="ANC_PNC_Why_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit_Translation", 
  "ANC_PNC_Why_Not_Satisfied_B4"="ANC_PNC_Why_Not_Satisfied__B4_Translation", 
  "ANC_PNC_Why_Chose_A_Different_Health_Facility_B4_1"="ANC_PNC_Why_Chose_A_Different_Health_Facility__B4_1_Translation", 
  "Nature_of_Complaint_Please_Elaborate_On_Your_Response"="Nature_of_Complaint_Please_Elaborate_On_Your_Response_Description", 
  "Please_Explain_Why_You_Were_Not_Satisfied_With_The_Resolution"="Please_Explain_Why_You_Were_Not_Satisfied_With_The_Resolution_Description",
  "Please_Elaborate_audio2_SectionE"="Can_You_Please_Tell_Me_About_The_Pamphlet_Sms_Or_Any_Other_Resources_You_Have_Heard_For_Women_Or_Girls_To_Seek_Help_Or_Get_Information_On_Womens_Family_Health_And_Well_Being_Translation")
# Log
t2_missing_log <-  rbind(
  # Translation
  log_questions(data=t2_data_filtered, columns=t2_audio_cols, columns_different=t2_audio_cols_diff, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=t2_data_filtered, 
                columns=c("Please_Take_Photo_Of_The_Vaccination_Card_1", 
                          "Please_Take_Photo_Of_The_Vaccination_Card_2",
                          "Please_Take_A_Photo_Of_Door_Tag_From_The_Selected_Household"), suffix="QA", sheet="data"))

## Tool 3
t3_tool <- read_excel("input/tools/HER+ESS+Tool+3_+Community+Actors+Survey+Tool.xlsx", "survey", guess_max = 100000)
t3_audio_cols <- t3_tool %>% filter(type %in% c("audio")) %>% pull(name)
# Audio Translation cols with different spelling 
t3_audio_cols_diff <- list(
  "Please_Describe_How_You_Work_Engage_With_Community_Health_Supervisor_Chs_CHWs"="Please_Describe_How_You_Work_Engage_With_Community_Health_Supervisor_Chs__CHWs_Translation",
  "Please_Describe_How_You_Work_Engage_With_Community_Health_Workers_Chws_CHWs"="Please_Describe_How_You_Work_Engage_With_Community_Health_Workers_Chws__CHWs_Translation"
  )
# Log
t3_missing_log <- log_questions(data=t3_data_filtered, columns=t3_audio_cols, columns_different = t3_audio_cols_diff, suffix="Translation", sheet="data")



## Log Missing Translation -------------------------------------------------------------------------
excluded_cols <- c("Province_DariPashto", "District_DariPashto", "HF_Name_based_on_Sample_DariPashto", 
            "Resp_Name", "Surveyor_Name", "Province_DariPashto", "District_DariPashto", 
            "HF_Name_based_on_Sample_DariPashto", "Resp_Name", "Surveyor_Name", 
            "Province_DariPashto", "District_DariPashto", "HF_Name_based_on_Sample_DariPashto", 
            "Province_DariPashto", "District_DariPashto", "HF_Name_based_on_Sample_DariPashto", 
            "Village", "Province_DariPashto", "District_DariPashto", "HF_Name_based_on_Sample_DariPashto", 
            "Please_Take_Photo_Of_The_Vaccination_Card_2_QA", "Please_Take_A_Photo_Of_Door_Tag_From_The_Selected_Household_QA", 
            "review_comments", "B6_Value_Other")

missing_translation_log <- rbind(
  missing_translation(data = hf_t1_data_wide, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 1.1"),
  missing_translation(data = hf_t2_data_wide, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 1.2"),
  ## Tool 1.3
  missing_translation(data = hf_t3_data_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 1.3"),
  ## Tool 2
  missing_translation(data = t2_data_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2"),
  missing_translation(data = t2_income_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2"),
  missing_translation(data = t2_illness_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2"),
  missing_translation(data = t2_injuries_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2"),
  missing_translation(data = t2_immunization_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2"),
  missing_translation(data = t2_other_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2"),
  ## Tool 3
  missing_translation(data = t3_data_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 3")
)
# t4_missing_translation_log <- rbind(
#   missing_translation(data = infra_data, KEY = "KEY", excluded_cols) %>% mutate(Sheet = "Data"),
#   missing_translation(data = infra_rep, KEY = "KEY", excluded_cols) %>% mutate(Sheet = "infra_rep"),
#   missing_translation(data = infra_doc, KEY = "KEY", excluded_cols) %>% mutate(Sheet = "infra_doc"),
#   missing_translation(data = infra_env, KEY = "KEY", excluded_cols) %>% mutate(Sheet = "infra_env"),
#   missing_translation(data = infra_feat, KEY = "KEY", excluded_cols) %>% mutate(Sheet = "infra_feat"),
#   missing_translation(data = infra_elem, KEY = "KEY", excluded_cols) %>% mutate(Sheet = "infra_elem")
# )

## Export List -------------------------------------------------------------------------------------
## Missing Translation and QA status
# missing_translation_QA_log <- list(
#   # t1.1_missing_log=t1.1_missing_log,
#   # t1.2_missing_log=t1.2_missing_log,
#   t1.3_missing_log=t1.3_missing_log,
#   t2_missing_log=t2_missing_log,
#   t3_missing_log=t3_missing_log
# )
missing_translation_QA_log <- rbind(
  t1.1_missing_log %>% mutate(Tool = 1.1),
  t1.2_missing_log %>% mutate(Tool = 1.2),
  t1.3_missing_log %>% mutate(Tool = 1.3),
  t2_missing_log %>% mutate(Tool = 2),
  t3_missing_log %>% mutate(Tool = 3)
) %>% 
  arrange(Tool, KEY)

missing_translation_QA_log <- missing_translation_QA_log %>% 
  left_join(hf_t1_data_wide %>% select(Interview_Type_Tool, KEY),by="KEY")
# missing_translation_QA_log <- missing_translation_QA_log %>% 
#   mutate(Question = str_remove(question, "_Translation")) %>%
#   # Filter issues added in Translation log but marked as other languages
#   left_join(translation_log2 %>% 
#               select(KEY, Language, Question, 'Translation Status',	Translation), by=c("KEY", "Question")) %>%
#   # Filter Tool 3 data marked as other languages
#   anti_join(
#     different_language,
#     by=c("KEY", "question")
#   )

# remove extra objects -----------------------------------------------------------------------------
rm(t1.1_audio_cols, t1.1_image_cols, t1.2_image_cols, t1.2_image_cols_diff, t1.1_tool,
  excluded_cols, t1.3_audio_cols, t2_tool, t2_audio_cols, t2_audio_cols_diff, t3_tool, t3_audio_cols, t3_audio_cols_diff)
