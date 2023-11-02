# Change/Recode variables
remove_98_99 <- function(x) {
  x = case_when(
    x %in% c(998, "998") ~ "I don't know",
    TRUE ~ x
  )}
t2_numeric_cols <- c(
  "How_Long_Did_It_Take_The_Household_Member_Who_Was_Diagnosed_With_Illness_To_Get_To_The_Health_Facility_For_Treatment_Approximately",
  "How_Long_Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Selected_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately",
  "Injury_Related_Care_How_Long_Did_It_Take_The_Household_Member_Who_Was_Diagnosed_With_Illness_Selected_To_Get_To_The_Health_Facility_For_Treatment_Approximately",
  "How_Long_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately",
  "Pregnancy_Related_Care_How_Long_Did_It_Take_The_Household_Member_Who_Has_Given_Birth_Recently_To_Get_To_The_Health_Facility_For_Treatment_Approximately",
  "Pregnancy_Related_Care_How_Long_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately",
  "ANC_PNC_How_Long_Did_It_Take_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_To_Get_To_The_Health_Facility_For_An_Antenatal_Visit_Approximately",
  "ANC_PNC_How_Long_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately",
  "ANC_PNC_How_Long_Did_It_Take_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_To_Get_To_The_Health_Facility_For_A_Postnatal_Visit_Approximately",
  "ANC_PNC_How_Long_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Have_To_Wait_To_Be_Seen_Examined_By_Health_Facility_Staff_Approximately",
  "Preventative_Care_How_Long_Did_It_Take_The_Household_Member_Who_Have_Received_The_Vaccine_Selected_To_Get_To_The_Health_Facility_To_Received_Immunization_Vaccination_Approximately",
  "Preventative_Care_How_Long_Did_The_Household_Member_Who_Have_Received_The_Vaccine_Selected_Wait_To_Be_Seen_To_Receive_Vaccination_Service_By_Health_Facility_Staff_Approximately",
  "How_Long_Did_It_Take_To_Resolve_Your_Complaint_Days"
)
## Tool 1.1 ----------------------------------------------------------------------------------------
t1.1_tool <- "input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level.xlsx"
hf_t1_data <- hf_t1_data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
  Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"),
  # Adding back the underscore removed by the labeler
  What_Other_Complaint_Channels_Do_You_Use = str_replace(What_Other_Complaint_Channels_Do_You_Use, "AudioResponse", "Audio_Response"))
hf_t1_data <- update_links(data=hf_t1_data, tool_path = t1.1_tool)
hf_injuries <- update_links(data=hf_injuries, tool_path = t1.1_tool, key_col = "PARENT_KEY")
hf_fatalities <- update_links(data=hf_fatalities, tool_path = t1.1_tool, key_col = "PARENT_KEY")
hf_incidents <- update_links(data=hf_incidents, tool_path = t1.1_tool, key_col = "PARENT_KEY")

## Tool 1.2 ----------------------------------------------------------------------------------------
t1.2_tool <- "input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level.xlsx"
hf_t2_data <- hf_t2_data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))
hf_t2_data <- update_links(data=hf_t2_data, tool_path = t1.2_tool)
hf_t2_photos <- update_links(data=hf_t2_photos, tool_path = t1.2_tool, key_col = "PARENT_KEY")

## Tool 1.3 ----------------------------------------------------------------------------------------
t1.3_tool <- "input/tools/+HER+ESS+Tool+1.3_+Nutrition+Counsellor+Interview+Tool.xlsx"
hf_t3_data <- hf_t3_data %>% 
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))
hf_t3_data <- update_links(data=hf_t3_data, tool_path = t1.3_tool)

## Tool 2 ------------------------------------------------------------------------------------------
t2_tool <- "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx"
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
  mutate(across(all_of(t2_numeric_cols), as.character)) %>% 
  mutate(across(all_of(t2_numeric_cols), remove_98_99))
t2_other <- t2_other %>% 
  mutate(Please_Tell_Me_What_Type_Of_Health_Facility_Medical_Professional_You_Your_Household_Member_Sought_For_Service_Name = 
           str_replace(Please_Tell_Me_What_Type_Of_Health_Facility_Medical_Professional_You_Your_Household_Member_Sought_For_Service_Name, 
                       "HFNamebasedonSample", "HF_Name_based_on_Sample"))
# Fix Download links
t2_data <- update_links(data=t2_data, tool_path = t2_tool)
t2_income <- update_links(data=t2_income, tool_path = t2_tool)
t2_illness <- update_links(data=t2_illness, tool_path = t2_tool)
t2_injuries <- update_links(data=t2_injuries, tool_path = t2_tool)
t2_immunization <- update_links(data=t2_immunization, tool_path = t2_tool)
t2_other <- update_links(data=t2_other, tool_path = t2_tool)

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
  select(Site_Visit_ID,	Province,	District,	Village,	
         Interviewee_Respondent_Type,	`Survey Number`=Survey_Number, KEY)

t2_income <- t2_income %>%
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:`Survey Number`, .before = 1)
t2_illness <- t2_illness %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:`Survey Number`, .before = 1)
t2_injuries <- t2_injuries %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:`Survey Number`, .before = 1)
t2_immunization <- t2_immunization %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:`Survey Number`, .before = 1)
t2_other <- t2_other %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:`Survey Number`, .before = 1)
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
t3_tool <- "input/tools/HER+ESS+Tool+3_+Community+Actors+Survey+Tool.xlsx"
t3_data <- t3_data %>% 
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"),
         Village = case_when(
           !is.na(Village_English) ~ Village_English,
           TRUE ~ Village
         ))
t3_data <- update_links(data=t3_data, tool_path = t3_tool)

# ## Tool 4 ------------------------------------------------------------------------------------------
# t4_tool <- "input/tools/HER_HF_Combined_Infra_V01+(1)+(6).xlsx"
# infra_data <- infra_data %>% 
#   mutate(Start = as.POSIXct(Start, format="%a %b %d %Y %H:%M:%S"),
#          End = as.POSIXct(End, format="%a %b %d %Y %H:%M:%S"),
#          HF_Type = case_when(
#            is.na(Type_Of_Essential_Package_Of_Hospital_Services_Ephs) ~ Type_Of_Basic_Package_Of_Health_Services_Bphs,
#            is.na(Type_Of_Basic_Package_Of_Health_Services_Bphs) ~ Type_Of_Essential_Package_Of_Hospital_Services_Ephs
#          ))
# infra_elem["KEY_Main"] <- str_split_fixed(infra_elem$PARENT_KEY, "/", 2)[,1]
# # Recode links
# infra_data <- update_links(data=infra_data, tool_path = t4_tool)
# infra_doc <- update_links(data=infra_doc, key_col = "PARENT_KEY", tool_path = t4_tool)
# infra_env <- update_links(data=infra_env, key_col = "PARENT_KEY", tool_path = t4_tool)
# infra_feat <- update_links(data=infra_feat, key_col = "PARENT_KEY", tool_path = t4_tool)
# infra_elem <- update_links(data=infra_elem, key_col = "KEY_Main", tool_path = t4_tool)

# 
# ## Infra Checklist Verification --------------------------------------------------------------------
# t4_checklist_tool <- "input/tools/HER_+Health+Facility+Infrastructure+Checklist+(1).xlsx"
# infra_checklist <- update_links(data=infra_checklist, tool_path = t4_checklist_tool)

# remove extra objects -----------------------------------------------------------------------------
rm(t1.1_tool, t1.2_tool, remove_98_99,t1.3_tool, t2_tool, t3_tool, t2_data_sub
   # t4_checklist_tool,infra_doc_link_cols,infra_env_link_cols, infra_feat_link_cols, t4_tool,
   # infra_elem_link_cols, infra_link_cols, checklist_link_cols,
   )

