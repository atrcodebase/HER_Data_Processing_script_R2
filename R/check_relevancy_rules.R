# Check Relevancy Rules ----------------------------------------------------------------------------
## Read
t1.1_tool_relevancy <- read_excel("input/tool_relevancy_rules/Too1.1_relevancies.xlsx")
t1.2_tool_relevancy <- read_excel("input/tool_relevancy_rules/Too1.2_relevancies.xlsx")
t1.3_tool_relevancy <- read_excel("input/tool_relevancy_rules/Tool_1.3_relevancies.xlsx")
t2_tool_relevancy <- read_excel("input/tool_relevancy_rules/Tool_2_relevancies.xlsx")
t3_tool_relevancy <- read_excel("input/tool_relevancy_rules/Tool_3_relevancies.xlsx")
# t4_tool_relevancy <- read_excel("input/tool_relevancy_rules/Too4_relevancies.xlsx")

### Tool 1.1
## Join main columns with repeat sheets
hf_t1_data_sub <- hf_t1_data %>%
  filter(qa_status %notin% "Rejected") %>%
  # filter(KEY %notin% rejection_log$KEY_Unique) %>%
  select(ESS_Resp_Consent, Interview_Type_Tool, How_Many_Instances_Of_Injuries_Have_Occurred,
         Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months,
         How_Many_Instances_Of_Fatalities_Have_Occurred, KEY)
hf_injuries_joined <- hf_injuries %>%
  filter(PARENT_KEY %in% hf_t1_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(hf_t1_data_sub, by=c("PARENT_KEY"="KEY"))
hf_fatalities_joined <- hf_fatalities %>%
  filter(PARENT_KEY %in% hf_t1_data_sub$KEY) %>% # Filter rejected data
  left_join(hf_t1_data_sub, by=c("PARENT_KEY"="KEY"))
hf_incidents_joined <- hf_incidents %>%
  filter(PARENT_KEY %in% hf_t1_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(hf_t1_data_sub, by=c("PARENT_KEY"="KEY"))

# Check Relevancy Rules
t1.1_relevancy_issues <- rbind(
  check_relevancy_rules(filter(hf_t1_data, qa_status %notin% "Rejected"), # Filter rejected data
                        t1.1_tool_relevancy, sheet_name="data"),
  check_relevancy_rules(hf_injuries_joined, t1.1_tool_relevancy, sheet_name = "injuries"),
  check_relevancy_rules(hf_fatalities_joined, t1.1_tool_relevancy, sheet_name = "fatalities"),
  check_relevancy_rules(hf_incidents_joined, t1.1_tool_relevancy, sheet_name = "incidents")
)

### Tool 1.2
## Join main columns with repeat sheets
hf_t2_data_sub <- hf_t2_data %>%
  filter(qa_status %notin% "Rejected") %>% # Filter rejected data
  select(ESS_Resp_Consent, How_Many_Handwashing_Stations_Are_There_In_This_Facility, KEY)
hf_t2_photos_joined <- hf_t2_photos %>%
  filter(PARENT_KEY %in% hf_t2_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(hf_t2_data_sub, by=c("PARENT_KEY"="KEY"))

# Check Relevancy Rules
t1.2_relevancy_issues <- rbind(
  check_relevancy_rules(filter(hf_t2_data, qa_status %notin% "Rejected"), # Filter rejected data
                        t1.2_tool_relevancy, sheet_name="data"),
  check_relevancy_rules(hf_t2_photos_joined, t1.2_tool_relevancy, sheet_name="photos")
)

# Check Relevancy Rules
### Tool 1.3
t1.3_relevancy_issues <- check_relevancy_rules(filter(hf_t3_data, qa_status %notin% "Rejected"), # Filter rejected data
                        t1.3_tool_relevancy, sheet_name="data")

### Tool 2
t2_data_sub <- t2_data %>%
  filter(qa_status %notin% "Rejected") %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  select(Consent, Gender_Of_Interviewee, Respondent_Age, 
         Do_You_Agree_If_My_Female_Colleague_Interview_A_Female_Member_Of_Your_Household,
         Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care,
         How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months,
         Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months,
         Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months,
         Did_You_Your_Household_Member_Receive_The_Following_Health_Services_In_The_Past_6_Months,
         How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities, KEY)
## Join main columns with repeat sheets
t2_income_joined <- t2_income %>%
  filter(PARENT_KEY %in% t2_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY"))
t2_illness_joined <- t2_illness %>%
  filter(PARENT_KEY %in% t2_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY"))
t2_injuries_joined <- t2_injuries %>%
  filter(PARENT_KEY %in% t2_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY"))
t2_immunization_joined <- t2_immunization %>%
  filter(PARENT_KEY %in% t2_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY"))
t2_other_joined <- t2_other %>%
  filter(PARENT_KEY %in% t2_data_sub$KEY) %>% # Filter rejected data
  filter(KEY %notin% rejection_log$KEY_Unique) %>%
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY"))

# Check Relevancy Rules
t2_relevancy_issues <- rbind(
  check_relevancy_rules(filter(t2_data, qa_status %notin% "Rejected" & KEY %notin% rejection_log$KEY_Unique), # Filter rejected data
                        t2_tool_relevancy, sheet_name="data"),
  check_relevancy_rules(t2_income_joined, t2_tool_relevancy, sheet_name = "Income_Earning_Members_Details"),
  check_relevancy_rules(t2_illness_joined, t2_tool_relevancy, sheet_name = "Illness_Details"),
  check_relevancy_rules(t2_injuries_joined, t2_tool_relevancy, sheet_name = "Injuries_Details"),
  check_relevancy_rules(t2_immunization_joined, t2_tool_relevancy, sheet_name = "Immunization_Details"),
  check_relevancy_rules(t2_other_joined, t2_tool_relevancy, sheet_name = "Section_B6_Other_Group"))

### Tool 3
t3_relevancy_issues <- check_relevancy_rules(filter(t3_data, qa_status %notin% "Rejected"), # Filter rejected data
                                               t3_tool_relevancy, sheet_name="data")



# Update Select_multiple series columns ------------------------------------------------------------
### Tool 1.1
hf_t1_data <- update_series_cols(data=hf_t1_data,
                                 tool_path = "input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level.xlsx",
                                 question_separator="_")
# Check if updated correctly
t1.1_SM_issues <- check_select_multiple(data=hf_t1_data,
                                        tool_path = "input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level.xlsx",
                                        question_separator="_")

### Tool 1.2
hf_t2_data <- update_series_cols(data=hf_t2_data,
                                 tool_path = "input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level.xlsx",
                                 question_separator="_")
# Check if updated correctly
t1.2_SM_issues <- check_select_multiple(data=hf_t2_data,
                                        tool_path = "input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level.xlsx",
                                        question_separator="_")

### Tool 1.3
hf_t3_data <- update_series_cols(data=hf_t3_data,
                                 tool_path = "input/tools/+HER+ESS+Tool+1.3_+Nutrition+Counsellor+Interview+Tool.xlsx",
                                 question_separator="_")
# Check if updated correctly
t1.3_SM_issues <- check_select_multiple(data=hf_t3_data,
                                        tool_path = "input/tools/+HER+ESS+Tool+1.3_+Nutrition+Counsellor+Interview+Tool.xlsx",
                                        question_separator="_")

### Tool 2
t2_data <- update_series_cols(data=t2_data,
                                 tool_path = "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx",
                                 question_separator="_")
t2_other <- update_series_cols(data=t2_other,
                              tool_path = "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx",
                              question_separator="_")
# Manually update this column
t2_data <- t2_data %>% 
  mutate(ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4.1 = case_when(
           grepl("\\b1\\b", ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4) ~ 1,
           !grepl("\\b1\\b", ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4) &
             !is.na(ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4) ~ 0,
           TRUE ~ as.numeric(NA)
  ))

# Check if updated correctly
t2_SM_issues <- check_select_multiple(data=t2_data,
                                      tool_path = "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx",
                                      question_separator="_")
t2_other_SM_issues <- check_select_multiple(data=t2_other,
                                      tool_path = "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx",
                                      question_separator="_")
t2_SM_issues <- rbind(
  t2_SM_issues,
  t2_other_SM_issues
)

### Tool 3
t3_data <- update_series_cols(data=t3_data,
                                 tool_path = "input/tools/HER+ESS+Tool+3_+Community+Actors+Survey+Tool.xlsx",
                                 question_separator="_")
# Check if updated correctly
t3_SM_issues <- check_select_multiple(data=t3_data,
                                      tool_path = "input/tools/HER+ESS+Tool+3_+Community+Actors+Survey+Tool.xlsx",
                                      question_separator="_")

## Export List -------------------------------------------------------------------------------------
# Relevancy
# relevancy_issues <- list(
#   T1.1_relevancy_issues=t1.1_relevancy_issues,
#   T1.2_relevancy_issues=t1.2_relevancy_issues,
#   T1.3_relevancy_issues=t1.3_relevancy_issues,
#   T2_relevancy_issues=t2_relevancy_issues,
#   T2_relevancy_issues_sub=t2_relevancy_issues_sub,
#   T3_relevancy_issues=t3_relevancy_issues
# )
relevancy_issues <- plyr::rbind.fill(
  t1.1_relevancy_issues %>% mutate(Tool=1.1),
  t1.2_relevancy_issues %>% mutate(Tool=1.2),
  t1.3_relevancy_issues %>% mutate(Tool=1.3),
  t2_relevancy_issues %>% mutate(Tool=2),
  t3_relevancy_issues %>% mutate(Tool=3)
) %>% 
  mutate(key = str_split_fixed(KEY, "/", 2)[,1], .after = KEY) %>% 
  arrange(Tool, KEY)

## Select Multiple issues
SM_issues <- list(
  T1.1_SM_issue=t1.1_SM_issues,
  T1.2_SM_issue=t1.2_SM_issues,
  T1.3_SM_issue=t1.3_SM_issues,
  T2_SM_issue=t2_SM_issues,
  T3_SM_issue=t3_SM_issues
)

# remove extra objects -----------------------------------------------------------------------------
rm(t1.3_tool_relevancy, t2_tool_relevancy, t3_tool_relevancy, t2_data_sub, t2_income_joined, 
   t2_illness_joined, t2_injuries_joined, t2_immunization_joined, t2_other_joined,
   t1.1_tool_relevancy, t1.2_tool_relevancy, hf_t1_data_sub, hf_injuries_joined, hf_fatalities_joined,
   hf_incidents_joined, hf_t2_data_sub, hf_t2_photos_joined 
   # t4_tool_relevancy, infra_data_sub, infra_rep_joined, infra_doc_joined, infra_env_joined, infra_feat_joined, infra_elem_joined
   )


# ### Tool 4
# ## Join main columns with repeat sheets
# infra_data_sub <- infra_data %>%
#   filter(review_status %in% "APPROVED") %>%
#   select(Subproject_Overall_Status_Established_Condition_Sa, KEY)
# infra_rep_joined <- infra_rep %>%
#   filter(PARENT_KEY %in% infra_data_sub$KEY) %>% # Filter Approved data
#   left_join(infra_data_sub, by=c("PARENT_KEY"="KEY"))
# infra_doc_joined <- infra_doc %>%
#   filter(PARENT_KEY %in% infra_data_sub$KEY) %>% # Filter Approved data
#   left_join(infra_data_sub, by=c("PARENT_KEY"="KEY"))
# infra_env_joined <- infra_env %>%
#   filter(PARENT_KEY %in% infra_data_sub$KEY) %>% # Filter Approved data
#   left_join(infra_data_sub, by=c("PARENT_KEY"="KEY"))
# infra_feat_joined <- infra_feat %>%
#   filter(PARENT_KEY %in% infra_data_sub$KEY) %>% # Filter Approved data
#   left_join(infra_data_sub, by=c("PARENT_KEY"="KEY"))
# infra_elem_joined <- infra_elem %>%
#   filter(PARENT_KEY %in% infra_feat_joined$KEY) %>% # Parent sheet is Features
#   left_join(infra_data_sub, by=c("PARENT_KEY"="KEY"))
# 
# # Check Relevancy Rules
# t4_relevancy_issues <- rbind(
#   check_relevancy_rules(filter(infra_data, review_status %in% "APPROVED"), # Filter rejected data
#                         t4_tool_relevancy, sheet_name="data"),
#   check_relevancy_rules(infra_rep_joined, t4_tool_relevancy, sheet_name="Representatives"),
#   check_relevancy_rules(infra_doc_joined, t4_tool_relevancy, sheet_name="Documents"),
#   check_relevancy_rules(infra_env_joined, t4_tool_relevancy, sheet_name="Environmental_And_Social_Standards"),
#   check_relevancy_rules(infra_feat_joined,t4_tool_relevancy, sheet_name="Features"),
#   check_relevancy_rules(infra_elem_joined,t4_tool_relevancy, sheet_name="Elements")
# )