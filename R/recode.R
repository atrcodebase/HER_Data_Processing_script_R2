# Change/Recode variables
remove_98_99 <- function(x) {
  x = case_when(
    x %in% c(998, "998") ~ "I don't know",
    TRUE ~ x
  )}
t2_numeric_cols <- c("Survey_Number", "Respondent_Age", "How_Many_Members_Are_In_Your_Household_Including_Yourself", 
  "How_Many_Members_Are_Currently_Living_In_Your_Household", "How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities", 
  "Pregnancy_Related_Care_How_Many_Household_Members_Have_Given_Birth_In_The_Past_6_Months_More_Than_One", 
  "Pregnancy_Related_Care_How_Old_Is_The_Household_Member_Who_Has_Given_Birth_Recently", 
  "Pregnancy_Related_Care_How_Long_Did_It_Take_The_Household_Member_Who_Has_Given_Birth_Recently_To_Get_To_The_Health_Facility_For_Treatment_Approximately", 
  "Pregnancy_Related_Care_How_Long_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately", 
  "ANC_PNC_How_Many_Of_Your_Household_Members_Including_You_Who_Were_Pregnant_Went_To_A_Health_Facility_For_At_Least_One_Antenatal_Visit_In_The_Last_6_Months", 
  "ANC_PNC_How_Long_Did_It_Take_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_To_Get_To_The_Health_Facility_For_An_Antenatal_Visit_Approximately", 
  "ANC_PNC_How_Long_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately", 
  "ANC_PNC_How_Many_Of_Your_Household_Members_Including_You_Who_Gave_Birth_In_The_Last_6_Months_Went_To_A_Health_For_At_Least_One_Postnatal_Visit", 
  "ANC_PNC_How_Long_Did_It_Take_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_To_Get_To_The_Health_Facility_For_A_Postnatal_Visit_Approximately", 
  "ANC_PNC_How_Long_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately", 
  "How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months_More_Than_One", 
  "Age_Of_Member", "How_Long_Did_It_Take_The_Household_Member_Who_Was_Diagnosed_With_Illness_To_Get_To_The_Health_Facility_For_Treatment_Approximately", 
  "How_Long_Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Selected_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately", 
  "How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One", 
  "Age_of_HH_Who_got_injured", "Injury_Related_Care_How_Long_Did_It_Take_The_Household_Member_Who_Was_Diagnosed_With_Illness_Selected_To_Get_To_The_Health_Facility_For_Treatment_Approximately", 
  "How_Long_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately", 
  "How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months_More_Than_One", 
  "Age_Of_The_Member_Who_Received_In_Years", "Age_Of_The_Member_Who_Received_In_Months", 
  "Preventative_Care_How_Long_Did_It_Take_The_Household_Member_Who_Have_Received_The_Vaccine_Selected_To_Get_To_The_Health_Facility_To_Received_Immunization_Vaccination_Approximately", 
  "How_Long_Did_It_Take_To_Resolve_Your_Complaint_Days", "How_Long_Did_It_Take_To_Resolve_Your_Complaint_Months"
)

## Tool 1.1 ----------------------------------------------------------------------------------------
t1.1_tool <- "input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level+-+R3.xlsx"
hf_t1_data <- hf_t1_data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
  Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"),
  KEY_Unique=KEY)
hf_t1_data <- update_media_links(data=hf_t1_data, tool_path = t1.1_tool)
hf_injuries <- update_media_links(data=hf_injuries, tool_path = t1.1_tool, key_col = "PARENT_KEY")
hf_fatalities <- update_media_links(data=hf_fatalities, tool_path = t1.1_tool, key_col = "PARENT_KEY")
hf_incidents <- update_media_links(data=hf_incidents, tool_path = t1.1_tool, key_col = "PARENT_KEY")

# Join main Sheet cols
hf_t1_sub <- hf_t1_data %>% 
  select(Site_Visit_ID, Province, District, HF_Name_based_on_Sample, KEY)

hf_injuries <- hf_injuries %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(hf_t1_sub, by="KEY") %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
hf_fatalities <- hf_fatalities %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(hf_t1_sub, by="KEY") %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
hf_incidents <- hf_incidents %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(hf_t1_sub, by="KEY") %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)

## Tool 1.2 ----------------------------------------------------------------------------------------
t1.2_tool <- "input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level+-+R3.xlsx"
hf_t2_data <- hf_t2_data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")
         # SubmissionDate= openxlsx::convertToDateTime(SubmissionDate),
         )

hf_t2_data <- update_media_links(data=hf_t2_data, tool_path = t1.2_tool)
hf_t2_photos <- update_media_links(data=hf_t2_photos, tool_path = t1.2_tool, key_col = "PARENT_KEY")

# Join main Sheet cols
hf_t2_photos <- hf_t2_photos %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(hf_t2_data %>% 
              select(Site_Visit_ID, Province, District, HF_Name_based_on_Sample, KEY), 
            by="KEY") %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)

## Tool 1.3 ----------------------------------------------------------------------------------------
t1.3_tool <- "input/tools/HER+ESS+Tool+1.3_+Nutrition+Counsellor+Interview+Tool+-+R3.xlsx"
hf_t3_data <- hf_t3_data %>% 
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))
hf_t3_data <- update_media_links(data=hf_t3_data, tool_path = t1.3_tool)

## Tool 2 ------------------------------------------------------------------------------------------
t2_tool <- "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys+-++R3.xlsx"
t2_data <- t2_data %>% 
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"),
         Survey_Number = case_when(
           Interviewee_Respondent_Type %in% "Female member of HH" ~ paste0("HH", Survey_Number, "-Female"),
           Interviewee_Respondent_Type %in% "Male member of HH" ~ paste0("HH", Survey_Number, "-Male"),
         ),
         Village = case_when(
           !is.na(Village_English) ~ Village_English,
           TRUE ~ Village
         )) %>% 
  mutate(across(any_of(t2_numeric_cols), as.character)) %>% 
  mutate(across(any_of(t2_numeric_cols), remove_98_99))
t2_other <- t2_other %>% 
  mutate(Please_Tell_Me_What_Type_Of_Health_Facility_Medical_Professional_You_Your_Household_Member_Sought_For_Service_Name = 
           str_replace(Please_Tell_Me_What_Type_Of_Health_Facility_Medical_Professional_You_Your_Household_Member_Sought_For_Service_Name, 
                       "HFNamebasedonSample", "HF_Name_based_on_Sample"),
         B6_Value = case_when(
           B6_Value %in% "1" ~ "Nutrition services (screening, counselling, treatment)",
           B6_Value %in% "2" ~ "Health education including WASH awareness session",
           B6_Value %in% "3" ~ "Counselling on mental health/psychological support",
           B6_Value %in% "4" ~ "Family planning/contraceptives",
           B6_Value %in% "77" ~ "Other",
           B6_Value %in% "0" ~ "None",
           TRUE ~ as.character(B6_Value)
         )) 

# Fix Download links
t2_data <- update_media_links(data=t2_data, tool_path = t2_tool)
t2_income <- update_media_links(data=t2_income, tool_path = t2_tool)
t2_illness <- update_media_links(data=t2_illness, tool_path = t2_tool)
t2_injuries <- update_media_links(data=t2_injuries, tool_path = t2_tool)
t2_immunization <- update_media_links(data=t2_immunization, tool_path = t2_tool)
t2_other <- update_media_links(data=t2_other, tool_path = t2_tool)

# Join repeat sheet columns
t2_data <- t2_data %>% 
  mutate(Selected_HH_Member = str_remove_all(Selected_HH_Member, "\\$\\{HHIllnessMember|\\}") %>% as.numeric(),
         Selected_Member_Inj = str_remove_all(Selected_Member_Inj, "Injury / Age: \\$\\{HHInjMember|\\}") %>% as.numeric(),
         PC_Selected_HH_Member = str_remove_all(PC_Selected_HH_Member, "\\$\\{HHImmunizationMember|\\}") %>% as.numeric()) %>% 
  # Illness Sheet
  left_join(t2_illness %>% 
              select(Illness_index, 
                     Selected_HH_Member_Age_Illness=Age_Of_Member,
                     Selected_HH_Member_Gender_Illness=Gender_Of_Member,
                     KEY=PARENT_KEY) %>% unique(), by=c("Selected_HH_Member"="Illness_index", "KEY")) %>% 
  relocate(Selected_HH_Member_Age_Illness:Selected_HH_Member_Gender_Illness, 
           .after = How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months) %>%
  # Injuries sheet
  left_join(t2_injuries %>% 
              select(Injury_index,
                     Selected_HH_Member_Gender_Injured=Gender_of_HH_Who_got_injured, 
                     KEY=PARENT_KEY) %>% unique(), by=c("Selected_Member_Inj"="Injury_index", "KEY")) %>% 
  relocate(Selected_HH_Member_Gender_Injured, .after = Age_Member_Injured) %>% 
  # Immunization sheet
  left_join(t2_immunization %>% 
              select(Immu_Index, 
                     Selected_HH_Member_Age_Immun_Years=Age_Of_The_Member_Who_Received_In_Years,
                     Selected_HH_Member_Age_Immun_Months=Age_Of_The_Member_Who_Received_In_Months,
                     Selected_HH_Member_Gender_Immun=Gender_Of_The_Member_Who_Received,
                     KEY=PARENT_KEY) %>% unique(), by=c("PC_Selected_HH_Member"="Immu_Index", "KEY")) %>% 
  relocate(Selected_HH_Member_Age_Immun_Years:Selected_HH_Member_Gender_Immun, 
           .after=How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months) %>% 
  rename(Selected_HH_Member_Age_Injured=Age_Member_Injured)

# Subset & Join main sheet columns
t2_data_sub <- t2_data %>% 
  select(Site_Visit_ID,	Province,	District,	Village, HF_Name_based_on_Sample, KEY)

t2_income <- t2_income %>%
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
t2_illness <- t2_illness %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
t2_injuries <- t2_injuries %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
t2_immunization <- t2_immunization %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
t2_other <- t2_other %>% 
  mutate(KEY_Unique=KEY, KEY=PARENT_KEY) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:HF_Name_based_on_Sample, .before = 1)
t2_other <- t2_other %>% 
  left_join(t2_data %>% 
              select(B6_Value_Other=Did_You_Your_Household_Member_Receive_The_Following_Health_Services_In_The_Past_6_Months_Other, KEY), 
            by=c("PARENT_KEY"="KEY")) %>% 
  relocate(B6_Value_Other, .after=B6_Value) %>% 
  mutate(B6_Value_Other = case_when(
    B6_Value %notin% "Other" & !is.na(B6_Value_Other) ~ NA_character_,
    TRUE ~ B6_Value_Other
  ))

## Tool 3 ------------------------------------------------------------------------------------------
t3_tool <- "input/tools/HER+ESS+R3+Tool+3_+Community+Actors+Survey+Tool.xlsx"
t3_data <- t3_data %>% 
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"),
         Village = case_when(
           !is.na(Village_English) ~ Village_English,
           TRUE ~ Village
         ), 
         Which_Health_Facility_Are_You_An_Lhc_Health_Shura_Member_In = str_replace(Which_Health_Facility_Are_You_An_Lhc_Health_Shura_Member_In, "HFNamebasedonSample", "HF_Name_based_on_Sample"),
         Which_Health_Facility_Are_You_Related_To_As_A_Chw= str_replace(Which_Health_Facility_Are_You_Related_To_As_A_Chw, "HFNamebasedonSample", "HF_Name_based_on_Sample"),
         KEY_Unique=KEY)

t3_data <- update_media_links(data=t3_data, tool_path = t3_tool)

# remove extra objects -----------------------------------------------------------------------------
rm(t1.1_tool, t1.2_tool, remove_98_99,t1.3_tool, t2_tool, t3_tool, t2_data_sub, hf_t1_sub
   # t4_checklist_tool,infra_doc_link_cols,infra_env_link_cols, infra_feat_link_cols, t4_tool,
   # infra_elem_link_cols, infra_link_cols, checklist_link_cols,
   )

