# Log missing audio translation and missing image QA -----------------------------------------------
## Tool 1.1
t1.1_tool <- read_excel("input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level+-+R3.xlsx", "survey", guess_max = 100000)
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
t1.2_tool <- read_excel("input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level+-+R3.xlsx", "survey", guess_max = 100000)
t1.2_audio_cols <- t1.2_tool %>% filter(type %in% c("audio") & name %in% names(hf_t2_data)) %>% pull(name)
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
  log_questions(data=hf_t2_data, columns=t1.2_audio_cols,
                suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=hf_t2_data, columns=t1.2_image_cols, columns_different = t1.2_image_cols_diff, suffix="QA", sheet="data"),
  log_questions(data=hf_t2_photos, columns="Please_Take_Photo_Of_The_Handwashing_Area", suffix="QA", sheet="Photos")
)

## Tool 1.3
t1.3_audio_cols <- c("Explanation_Note", "Can_You_Please_Explain_What_You_Do_Besides_Your_Job_As_A_Nutrition_Counsellor_At_This_Health_Facility_Other", 
                     "Can_You_Explain_What_Trainings_Have_You_Received_From_Which_Entity", 
                     "Thank_You_Very_Much_For_Your_Time_And_Responding_To_My_Questions_Please_Let_Me_Know_If_You_Would_Like_To_Provide_Any_Comments_On_How_To_Improve_Nutrition_Services_And_Counselling_In_The_Health_Facility")
# Log
t1.3_missing_log <-  rbind(
  # Translation
  log_questions(data=hf_t3_data, columns=t1.3_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=hf_t3_data, columns=c("Selfie_Photo", "Explanation_Paper_Photo"), suffix="QA", sheet="data"))


## Tool 2
t2_tool <- read_excel("input/tools/HER+ESS+Tool+2_+Household+Level+Surveys+-++R3.xlsx", "survey", guess_max = 100000)
t2_audio_cols <- t2_tool %>% filter(type %in% c("audio") & name %notin% "Please_Provide_Explanation") %>% pull(name) # Please_Provide_Explanation removed from R2
# Audio Translation cols with different spelling 
t2_audio_cols_diff <- list(
  "ANC_PNC_Why_Did_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit"="ANC_PNC_Why_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit_Translation", 
  "ANC_PNC_Why_Not_Satisfied_B4"="ANC_PNC_Why_Not_Satisfied__B4_Translation", 
  "ANC_PNC_Why_Chose_A_Different_Health_Facility_B4_1"="ANC_PNC_Why_Chose_A_Different_Health_Facility__B4_1_Translation", 
  "Nature_of_Complaint_Please_Elaborate_On_Your_Response"="Nature_of_Complaint_Please_Elaborate_On_Your_Response_Description", 
  "Please_Explain_Why_You_Were_Not_Satisfied_With_The_Resolution"="Please_Explain_Why_You_Were_Not_Satisfied_With_The_Resolution_Description")
# Log
t2_missing_log <-  rbind(
  # Translation
  log_questions(data=t2_data, columns=t2_audio_cols, columns_different=t2_audio_cols_diff, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=t2_data, 
                columns=c("Please_Take_A_Photo_Of_Door_Tag_From_The_Selected_Household"), suffix="QA", sheet="data"))

## Tool 3
t3_tool <- read_excel("input/tools/HER+ESS+R3+Tool+3_+Community+Actors+Survey+Tool.xlsx", "survey", guess_max = 100000)
t3_audio_cols <- t3_tool %>% filter(type %in% c("audio")) %>% pull(name)
# Audio Translation cols with different spelling 
t3_audio_cols_diff <- list(
  "Please_Describe_How_You_Work_Engage_With_Community_Health_Supervisor_Chs_CHWs"="Please_Describe_How_You_Work_Engage_With_Community_Health_Supervisor_Chs__CHWs_Translation",
  "Please_Describe_How_You_Work_Engage_With_Community_Health_Workers_Chws_CHWs"="Please_Describe_How_You_Work_Engage_With_Community_Health_Workers_Chws__CHWs_Translation",
  "Community_Elders_Confirmation_On_Why_Lhc_Or_Chw__Does_Not_Exist_In_The_Community"="Community_Elders_Confirmation_On_Why_Lhc_Or_Chw_Does_Not_Exist_In_The_Community_Translation")
# Log
t3_missing_log <- log_questions(data=t3_data_filtered, columns=t3_audio_cols, columns_different = t3_audio_cols_diff, suffix="Translation", sheet="data")
# No need to translate the confirmation audio if at least one of those questions does not have No
t3_missing_log <- t3_missing_log %>% 
  left_join(
    t3_data_filtered %>% select(Does_The_Community_Have_Local_Health_Committee_Lhc_Members, Does_The_Community_Have_Community_Health_Workers_Chws, KEY),
    by="KEY") %>% 
  filter(!(question %in% "Community_Elders_Confirmation_On_Why_Lhc_Or_Chw_Does_Not_Exist_In_The_Community_Translation" &
         (Does_The_Community_Have_Local_Health_Committee_Lhc_Members %notin% "No" & Does_The_Community_Have_Community_Health_Workers_Chws %notin% "No"))) %>% 
  select(-Does_The_Community_Have_Local_Health_Committee_Lhc_Members, -Does_The_Community_Have_Community_Health_Workers_Chws)

## Log Missing Translation -------------------------------------------------------------------------
excluded_cols <- c("Province_DariPashto", "HF_Name_based_on_Sample_DariPashto", 
                   "Resp_Name", "Surveyor_Name", "District_DariPashto", "Village", 
                   "review_comments", "Selected_HH_Member_Label", "PC_Selected_HH_Member_Label",
                   "B6_Label")

missing_translation_log <- rbind(
  missing_translation(data = hf_t1_data_wide, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 1.1", tab_name="data"),
  missing_translation(data = hf_t2_data_wide, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 1.2", tab_name="data"),
  ## Tool 1.3
  missing_translation(data = hf_t3_data_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 1.3", tab_name="data"),
  ## Tool 2
  missing_translation(data = t2_data_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2", tab_name="data"),
  missing_translation(data = t2_income_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2", tab_name="Income_Earning_Members_Details"),
  missing_translation(data = t2_illness_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2", tab_name="Illness_Details"),
  missing_translation(data = t2_injuries_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2", tab_name="Injuries_Details"),
  missing_translation(data = t2_immunization_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2", tab_name="Immunization_Details"),
  missing_translation(data = t2_other_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 2", tab_name="Section_B6_Other_Group"),
  ## Tool 3
  missing_translation(data = t3_data_filtered, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Tool 3", tab_name="data")
)

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

## Separate translation and image logs
missing_translation_QA_log_sub <- missing_translation_QA_log %>% 
  filter(question_type == "Translation" &
           !(Tool %in% c("1.1", "2") & question %in% c("Explanation_Note_Translation", "No_Consent_Reason_Translation"))) %>% # QA: translation not required
  mutate(key=str_split_fixed(KEY, "/", 2)[,1]) %>%
  left_join(qa_log %>% select(key=KEY, Survey_Language), by="key") %>% 
  select(KEY, Tool, Survey_Language, Question=question, Audio_link=download_link, value, sheet, issue) 
# Remove audios already added in translation log
missing_translation_QA_log_sub <- missing_translation_QA_log_sub %>% 
  left_join(hf_t1_data_wide %>% select(Interview_Type_Tool, KEY),by="KEY") %>% 
  anti_join(
    translation_log %>% select(KEY, Question),
    by=c("KEY", "Question")
  )

# Export list
missing_translation_QA_log <- list(
  Image_log=filter(missing_translation_QA_log, question_type=="QA"),
  Audio_log=missing_translation_QA_log_sub
)

# remove extra objects -----------------------------------------------------------------------------
rm(t1.1_audio_cols, t1.1_image_cols, t1.2_image_cols, t1.2_image_cols_diff, t1.1_tool,
  excluded_cols, t1.3_audio_cols, t2_tool, t2_audio_cols, t2_audio_cols_diff, t3_tool, t3_audio_cols, 
  t3_audio_cols_diff, missing_translation_QA_log_sub)
