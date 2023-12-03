### Remove extra columns
## Extra Columns -----------------------------------------------------------------------------------
# Tool 1.1
t1.1_caption_cols <- names(hf_t1_data_wide)[grepl("caption$", names(hf_t1_data_wide), ignore.case = TRUE)] # All question ending with Caption
t1.1_extra_cols <- c(t1.1_caption_cols, "Passcode", "Deviceid", "Subscriberid", "Simid", "Devicephonenum", 
                     "Username", "TA", "Geopoint1-Altitude", "Geopoint1-Accuracy", 
                     "Province_filter", "Surveyor_Name", "Line_Ministry_Name", "Line_Ministry_Project_Id", 
                     "Line_Ministry_SubProject_Id", "Line_Ministry_Sub_Project_Name_And_Description", 
                     "Type_Of_Implementing_Partner", "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID", 
                     "Reporting_Period", "Sample_info_correct", "AA1", "Resp_Name", "comments",
                     "Contact_number1_of_Respondent", "Contact_number2_of_Respondent", 
                     "SET-OF-Injuries_Details", "SET-OF-Fatalities_Details", "SET-OF-Incidents", 
                     "Contact_Number_1_Of_The_Staff", "Contact_Number_2_Of_The_Staff", 
                     "AA2", "Contact_number1_of_GBV_Respondent", "Contact_number2_of_GBV_Respondent", 
                     "AA3", "Contact_number1_of_HWCHS_Respondent", "Contact_number2_of_HWCHS_Respondent", 
                     "instanceID", "formdef_version", "review_status", "review_quality", 
                     "review_comments", "review_corrections", "qa_status", "Instances_Count", "Passcode_correct[1]/Location_Type",
                     "Province_DariPashto", "District_DariPashto", "HF_Name_based_on_Sample_DariPashto",
                     "Survey_Language", "Geopoint1-Latitude",	"Geopoint1-Longitude")
# Tool 1.2
t1.2_caption_cols <- names(hf_t2_data_wide)[grepl("caption$", names(hf_t2_data_wide), ignore.case = TRUE)] # All question ending with Caption
t1.2_extra_cols <- c(t1.2_caption_cols, "Passcode", "Deviceid", "Subscriberid", "Simid", "Devicephonenum",
                     "Username", "TA", "Geopoint1-Altitude", "Geopoint1-Accuracy",
                     "Province_filter", "Surveyor_Name", "Line_Ministry_Name", "Line_Ministry_Project_Id",
                     "Line_Ministry_SubProject_Id", "Line_Ministry_Sub_Project_Name_And_Description",
                     "Type_Of_Implementing_Partner", "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID",
                     "Reporting_Period", "AA1", "Resp_Name", "Contact_number1_of_Respondent",
                     "Contact_number2_of_Respondent", "SET-OF-Photos_Of_Handwashing_Stations",
                     "How_Is_Mixed_Waste_Disposed_7777", "instanceID", "formdef_version", "comments",
                     "review_status", "review_quality", "review_comments", "review_corrections",
                     "SET-OF-Photos_Of_Handwashing_Stations", "qa_status", "Province_DariPashto", 
                     "District_DariPashto", "HF_Name_based_on_Sample_DariPashto", "Survey_Language",
                     "Passcode_correct[1]/Location_Type", "Geopoint1-Latitude",	"Geopoint1-Longitude")
# Tool 1.3
t1.3_extra_cols <- c("Passcode", "Deviceid", "Subscriberid", "Simid", "Devicephonenum", 
                     "Username", "Audio_Audit_Full", "TA", "Geopoint1-Altitude", "Geopoint1-Accuracy", 
                     "Province_filter", "Surveyor_Name", "Province_DariPashto", "District_DariPashto", 
                     "HF_Type_based_on_sample_Value", "HF_Name_based_on_Sample_DariPashto", 
                     "Line_Ministry_Project_Id", "Line_Ministry_SubProject_Id", "Type_Of_Implementing_Partner", 
                     "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID", "Reporting_Period", 
                     "Selfie_Photo_Caption", "Explanation_Paper_Photo_Caption", "Contact_num1", 
                     "Contact_num2", "instanceID", "formdef_version", "review_status", "comments",
                     "review_quality", "review_comments", "review_corrections", "qa_status", "Survey_Language", 
                     "Geopoint1-Latitude",	"Geopoint1-Longitude")

# Tool 2 
t2_extra_cols <- c("Passcode", "Deviceid", "Subscriberid", "Simid", "Devicephonenum", 
                   "Username", "TA", "AA1", "Geopoint1-Altitude", "Geopoint1-Accuracy", 
                   "Province_filter", "Surveyor_Name", "Province_DariPashto", "District_DariPashto", 
                   "HF_Type_based_on_sample_Value", "HF_Name_based_on_Sample_DariPashto", 
                   "Line_Ministry_Name", "Line_Ministry_Project_Id", "Line_Ministry_SubProject_Id", 
                   "Line_Ministry_Sub_Project_Name_And_Description", "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID", 
                   "Reporting_Period", "AA2", "Contact_num1", "Contact_num2", "Income_Earning_Members_Details_count", 
                   "SET-OF-Income_Earning_Members_Details", "Count_Income_Earning_Members_Details", 
                   "AA3", "Illness_Details_count", "SET-OF-Illness_Details", "Count_Illness_Details", 
                   "Selected_HH_Member", "Selected_HH_Member_Label", "AA4", "Injuries_Details_count", 
                   "SET-OF-Injuries_Details", "Count_Injuries_Details", "Selected_Member_Inj", 
                   "AA5", "AA6", "AA7", "Immunization_Details_count", "SET-OF-Immunization_Details", 
                   "Count_Immunization_Details", "PC_Selected_HH_Member", "PC_Selected_HH_Member_Label", 
                   "Please_Take_Photo_Of_The_Vaccination_Card_1_Caption", "Please_Take_Photo_Of_The_Vaccination_Card_2_Caption", 
                   "AA8", "Section_B6_Other_Group_count", "SET-OF-Section_B6_Other_Group", 
                   "Count_Section_B6_Other_Group", "AA9", "AA10", "AA11", "AA12", 
                   "id", "Doortag_ID", "Please_Take_A_Photo_Of_Door_Tag_From_The_Selected_Household_Caption", 
                   "Survey_Language", "HH_Illness_Member1", "HH_Illness_Member2", 
                   "HH_Illness_Member3", "HH_Illness_Member4", "HH_Illness_Member5", 
                   "HH_Illness_Member6", "HH_Illness_Member7", "HH_Illness_Member8", 
                   "HH_Illness_Member9", "HH_Illness_Member10", "HH_Inj_Member1", "comments",
                   "HH_Inj_Member2", "HH_Inj_Member3", "HH_Inj_Member4", "HH_Inj_Member5", 
                   "HH_Inj_Member6", "HH_Inj_Member7", "HH_Inj_Member8", "HH_Inj_Member9", 
                   "HH_Inj_Member10", "HH_Immunization_Member1", "HH_Immunization_Member2", 
                   "HH_Immunization_Member3", "HH_Immunization_Member4", "HH_Immunization_Member5", 
                   "HH_Immunization_Member6", "HH_Immunization_Member7", "HH_Immunization_Member8", 
                   "HH_Immunization_Member9", "HH_Immunization_Member10", "instanceID", 
                   "formdef_version", "review_status", "review_quality", "review_comments", 
                   "review_corrections", "qa_status", "Village_English", "End_Comments",	
                   "End_Comments_Translation", "Geopoint1-Latitude",	"Geopoint1-Longitude")
# Tool 3
t3_extra_cols <- c("Passcode", "Deviceid", "Subscriberid", "Simid", "Devicephonenum", 
                   "Username", "TA", "AA1", "Geopoint1-Altitude", "Geopoint1-Accuracy", 
                   "Province_filter", "Surveyor_Name", "Province_DariPashto", "District_DariPashto", 
                   "HF_Type_based_on_sample_Value", "HF_Name_based_on_Sample_DariPashto", 
                   "Line_Ministry_Project_Id", "Line_Ministry_SubProject_Id", "Type_Of_Implementing_Partner", 
                   "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID", "Reporting_Period", 
                   "AA2", "Contact_num1", "Contact_num2", "AA3", "AA4", "instanceID", 
                   "formdef_version", "review_status", "review_quality", "review_comments", 
                   "review_corrections", "qa_status", "Village_English", "Survey_Language", 
                   "Geopoint1-Latitude",	"Geopoint1-Longitude", "comments")
# Tool 4
t4_extra_cols <- c("instanceID", "formdef_version", "review_status", "review_quality", 
  "review_comments", "review_corrections", "SET-OF-Representatives", "SET-OF-Documents", 
  "SET-OF-Environmental_And_Social_Standards", "SET-OF-Features")

## Remove Extra columns ----------------------------------------------------------------------------
hf_t1_data_wide <- hf_t1_data_wide %>% select(-all_of(t1.1_extra_cols))
hf_t2_data_wide <- hf_t2_data_wide %>% select(-all_of(t1.2_extra_cols))

# Tool 1.3
hf_t3_data_filtered <- hf_t3_data_filtered %>% select(-all_of(t1.3_extra_cols))

# Tool 2
t2_data_filtered <- t2_data_filtered %>%
  select(-all_of(t2_extra_cols)) %>% 
  mutate(KEY_Unique = KEY)
t2_income_filtered <- t2_income_filtered %>% 
  select(-all_of(c("SET-OF-Income_Earning_Members_Details"))) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_illness_filtered <- t2_illness_filtered %>% 
  select(-all_of(c("Illness_Name","SET-OF-Illness_Details"))) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_injuries_filtered <- t2_injuries_filtered %>% 
  select(-all_of(c("SET-OF-Injuries_Details"))) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_immunization_filtered <- t2_immunization_filtered %>% 
  select(-all_of(c("Immu_Name", "SET-OF-Immunization_Details"))) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_other_filtered <- t2_other_filtered %>% 
  select(-all_of(c("B6_Label", "SET-OF-Section_B6_Other_Group"))) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
  
# Tool 3
t3_data_filtered <- t3_data_filtered %>% select(-all_of(t3_extra_cols))

# Tool4
# infra_data <- infra_data %>% select(-all_of(t4_extra_cols))
# infra_rep <- infra_rep %>% select(-"SET-OF-Representatives")
# infra_doc <- infra_doc %>% select(-"SET-OF-Documents")
# infra_env <- infra_env %>% select(-"SET-OF-Environmental_And_Social_Standards")
# infra_feat <- infra_feat %>% select(-all_of(c("SET-OF-Features","SET-OF-Elements")))
# infra_elem <- infra_elem %>% select(-"SET-OF-Elements")


# remove extra objects -----------------------------------------------------------------------------
rm(
  # t1.1_extra_cols, t1.2_extra_cols, t1.1_caption_cols, t1.2_caption_cols, t4_extra_cols, 
  t1.3_extra_cols, t2_extra_cols, t3_extra_cols)

