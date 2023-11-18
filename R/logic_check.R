### Logic Checks
## Tool 1.1 ----------------------------------------------------------------------------
# hf_t1_data %>%
#   rowwise() %>%
#   filter(How_Many_Instances_Of_Injuries_Have_Occurred %notin% Injuries_Details_count) %>%
#   select(How_Many_Instances_Of_Injuries_Have_Occurred, Injuries_Details_count)
# hf_t1_data %>%
#   rowwise() %>%
#   filter(How_Many_Instances_Of_Fatalities_Have_Occurred %notin% Fatalities_Details_count) %>%
#   select(How_Many_Instances_Of_Fatalities_Have_Occurred, Fatalities_Details_count)
# # Ask about these 3 questions
# # How_Many_Instances	Instances_Count	Incidents_count
# hf_t1_data %>%
#   rowwise() %>%
#   filter(Incidents_count %notin% How_Many_Instances & Incidents_count != 0) %>%
#   select(Incidents_count, How_Many_Instances)

t1.1_count_mismatch <- rbind(
  # Tool 1.1
  hf_injuries %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Injuries_Details") %>%
    full_join(hf_t1_data %>% select(main_sheet_count=How_Many_Instances_Of_Injuries_Have_Occurred, KEY) %>% 
                mutate(Sheet="Injuries_Details", Question="How_Many_Instances_Of_Injuries_Have_Occurred"), by=c("KEY", "Sheet")),
  hf_fatalities %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Fatlities_Details") %>%
    full_join(hf_t1_data %>% select(main_sheet_count=How_Many_Instances_Of_Fatalities_Have_Occurred, KEY) %>% 
                mutate(Sheet="Fatlities_Details", Question="How_Many_Instances_Of_Fatalities_Have_Occurred"), by=c("KEY", "Sheet")),
  hf_incidents %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Incidents") %>%
    full_join(hf_t1_data %>% select(main_sheet_count=Instances_Count, KEY) %>% 
                mutate(Sheet="Incidents", Question="Instances_Count"), by=c("KEY", "Sheet")) %>% 
    filter(!(main_sheet_count %in% 0 & is.na(repeat_sheet_count)))
) %>% rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & !(is.na(repeat_sheet_count) & is.na(main_sheet_count))) %>% mutate(Tool="Tool 1.1")


HF_Type_issue <- hf_t1_data_wide %>%
  select(Province, District, HF_Code_based_on_sample, HF_Name_based_on_Sample, HF_Type_based_on_sample, HF_Type_Based_on_SV) %>% unique() %>%
  janitor::get_dupes(HF_Code_based_on_sample) %>% 
  mutate(issue="Same HF has two differnet HF_Type_Based_on_SV")
write.xlsx(HF_Type_issue, "output/Multiple_HF_Type_T1.xlsx")

t1.1_logical_issues <- rbind(
  hf_t1_data %>%
    filter((How_Many_Male_Chws_Are_Active_Under_This_Health_Facility+How_Many_Female_Chws_Are_Active_Under_This_Health_Facility) >
             How_Many_Health_Posts_Are_Active_Under_This_Health_Facility*2) %>%
    mutate(issue="Sum of Active Male/Female workers shouldn't be more than Active Posts in the HF",
           Questions = "How_Many_Male_Chws_Are_Active_Under_This_Health_Facility - How_Many_Female_Chws_Are_Active_Under_This_Health_Facility - How_Many_Health_Posts_Are_Active_Under_This_Health_Facility",
           Values = paste0(How_Many_Male_Chws_Are_Active_Under_This_Health_Facility, " - ", How_Many_Female_Chws_Are_Active_Under_This_Health_Facility, " - ", How_Many_Health_Posts_Are_Active_Under_This_Health_Facility)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # More than one interview in the same HF with the same Interview type
  hf_t1_data_wide %>% 
    janitor::get_dupes(HF_Code_based_on_sample, Interview_Type_Tool) %>% 
    mutate(issue="More than one interview in the same HF with the same Interview type",
           Questions = "HF_Code_based_on_sample - Interview_Type_Tool",
           Values = paste0(HF_Code_based_on_sample, " - ", Interview_Type_Tool)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Month shouldnt be more than 11
  hf_t1_data_wide %>%
    filter(How_long_have_you_been_working_at_the_facility_Months > 11) %>% 
mutate(issue="Month shouldnt be more than 12",
           Questions = "How_long_have_you_been_working_at_the_facility_Months",
           Values = How_long_have_you_been_working_at_the_facility_Months) %>% 
    select(Questions, Values, issue, KEY, qa_status)
) %>% mutate(Tool="Tool 1.1")

## Tool 1.2  ---------------------------------------------------------------------------------------
# hf_t2_data_wide %>%
#   rowwise() %>%
#   filter(How_Many_Handwashing_Stations_Are_There_In_This_Facility %notin% Photos_Of_Handwashing_Stations_count) %>%
#   select(How_Many_Handwashing_Stations_Are_There_In_This_Facility, Photos_Of_Handwashing_Stations_count)
# 

t1.2_count_mismatch <- hf_t2_photos %>%
  count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Photos") %>%
  full_join(hf_t2_data %>% 
              select(main_sheet_count=Photos_Of_Handwashing_Stations_count, KEY) %>% 
              mutate(Sheet="Photos", Question="Photos_Of_Handwashing_Stations_count"), by=c("KEY", "Sheet")) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & !(main_sheet_count %in% 0 & is.na(repeat_sheet_count))) %>% mutate(Tool = "Tool 1.2")

t1.2_logical_issues <- rbind(
  hf_t2_data %>%
    filter(How_Many_Handwashing_Stations_Have_Functional_Water > Photos_Of_Handwashing_Stations_count) %>%
    mutate(issue="The number of Hand Washing stations does not match with number of photos",
           Questions = "How_Many_Handwashing_Stations_Have_Functional_Water - Photos_Of_Handwashing_Stations_count",
           Values = paste0(How_Many_Handwashing_Stations_Have_Functional_Water, " - ", Photos_Of_Handwashing_Stations_count)) %>% 
    select(Questions, Values, issue, KEY, qa_status)
) %>% mutate(Tool="Tool 1.1")

## Tool 1.3  ---------------------------------------------------------------------------------------
t1.3_logical_issues <- rbind(
  hf_t3_data_filtered %>%
    filter(For_How_Long_Have_You_Been_Working_At_This_Hf_Years > For_How_Long_Have_You_Been_A_Nutrition_Counsellor_Years) %>%
    mutate(issue="The NC worked more years in the HF than the number of years she says she was a NC, plz double-check",
           Questions = "For_How_Long_Have_You_Been_Working_At_This_Hf_Years - For_How_Long_Have_You_Been_A_Nutrition_Counsellor_Years",
           Values = paste0(For_How_Long_Have_You_Been_Working_At_This_Hf_Years, " - ", For_How_Long_Have_You_Been_A_Nutrition_Counsellor_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_t3_data_filtered %>%
    rowwise() %>% 
    filter(Where_Did_You_Work_Before_As_NC %in% "I have been always working at this health facility as a nutrition counsellor" & 
             For_How_Long_Have_You_Been_Working_At_This_Hf_Years != For_How_Long_Have_You_Been_A_Nutrition_Counsellor_Years) %>%
    mutate(issue="She always worked in this HF as NC but her number of years as NC doesn't match number of years as NC",
           Questions = "Where_Did_You_Work_Before_As_NC - For_How_Long_Have_You_Been_Working_At_This_Hf_Years - For_How_Long_Have_You_Been_A_Nutrition_Counsellor_Years",
           Values = paste0(Where_Did_You_Work_Before_As_NC, " - ", For_How_Long_Have_You_Been_Working_At_This_Hf_Years, " - ", For_How_Long_Have_You_Been_A_Nutrition_Counsellor_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status)
) %>% mutate(Tool="Tool 1.3")

## Tool 2 ------------------------------------------------------------------------------------------
t2_logical_issues <- rbind(
  # Male HH head but also Female Headed HH
  t2_data_filtered %>% 
    filter(Gender_Of_Interviewee %in% "Male" & Is_This_A_Female_Headed_Household %in% "Yes" & 
             What_Is_The_Relationship_Of_Household_Head_With_You %in% "Household head / myself") %>% 
    mutate(issue="Male HH head but also Female Headed HH",
           Questions = "Gender_Of_Interviewee - Is_This_A_Female_Headed_Household - What_Is_The_Relationship_Of_Household_Head_With_You",
           Values = "Male - Yes - Household head / myself") %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Female Headed HH is no but respondent is Female and HH head
  t2_data_filtered %>% 
    filter(Gender_Of_Interviewee %in% "Female" & Is_This_A_Female_Headed_Household %in% "No" & 
             What_Is_The_Relationship_Of_Household_Head_With_You %in% "Household head / myself") %>% 
    mutate(issue="Female Headed HH is no but respondent is Female and HH head",
           Questions = "Gender_Of_Interviewee - Is_This_A_Female_Headed_Household - What_Is_The_Relationship_Of_Household_Head_With_You",
           Values = "Female - No - Household head / myself") %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Highest education for both questions shouldn't be the same
  t2_data_filtered %>% 
    filter(What_Is_Your_Household_Heads_Highest_Level_Of_Education == What_Is_The_Highest_Education_Level_In_Your_Household) %>% 
    rowwise() %>% 
    mutate(issue = "Highest education for both questions shouldn't be the same",
           Questions = "What_Is_Your_Household_Heads_Highest_Level_Of_Education - What_Is_The_Highest_Education_Level_In_Your_Household",
           Values = paste0(What_Is_Your_Household_Heads_Highest_Level_Of_Education, " - ", What_Is_The_Highest_Education_Level_In_Your_Household)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # HH headed by Elderly person but respondent is less than 65 years old
  t2_data_filtered %>% 
    filter(Is_Your_Household_Headed_By_An_Elderly_Person %in% "Yes" & What_Is_The_Relationship_Of_Household_Head_With_You %in% "Household head / myself" & 
             Respondent_Age < 65) %>% 
    rowwise() %>% 
    mutate(issue = "HH headed by Elderly person but respondent is less than 65 years old",
           Questions = "Respondent_Age - What_Is_The_Relationship_Of_Household_Head_With_You - Is_Your_Household_Headed_By_An_Elderly_Person",
           Values = paste0(Respondent_Age, " - ",What_Is_The_Relationship_Of_Household_Head_With_You," - ", Is_Your_Household_Headed_By_An_Elderly_Person)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # HH is headed by person who is 65 or older but HH headed by elderly is No
  t2_data_filtered %>% 
    filter(Is_Your_Household_Headed_By_An_Elderly_Person %in% "No" & What_Is_The_Relationship_Of_Household_Head_With_You %in% "Household head / myself" & 
             Respondent_Age >= 65) %>% 
    rowwise() %>% 
    mutate(issue = "HH is headed by person who is 65 or older but HH headed by elderly is No",
           Questions = "Respondent_Age - What_Is_The_Relationship_Of_Household_Head_With_You - Is_Your_Household_Headed_By_An_Elderly_Person",
           Values = paste0(Respondent_Age, " - ",What_Is_The_Relationship_Of_Household_Head_With_You," - ", Is_Your_Household_Headed_By_An_Elderly_Person)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Illness: Selected responses and count of HH members does not match
  t2_data_filtered %>% 
    filter(grepl("Yes, I have fallen seriously ill|Yes, a household member has fallen seriously ill", 
                 Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) & 
             str_count(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, ";") == 0 &
             How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months != 1) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care - How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months",
           Values = paste0(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, " - ",How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(grepl("Yes, I have fallen seriously ill", 
                 Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) &
             grepl("Yes, a household member has fallen seriously ill", 
                   Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) &
             str_count(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, ";") == 1 &
             How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months != 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care - How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months",
           Values = paste0(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, " - ",How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(grepl("Yes, I have fallen seriously ill", 
                 Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) &
             grepl("Yes, more than one household member fell seriously ill – excluding myself", 
                   Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) &
             str_count(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, ";") == 1 &
             How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months <= 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care - How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months",
           Values = paste0(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, " - ",How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care %in% 
             "Yes, more than one household member fell seriously ill – excluding myself" &
             How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months_More_Than_One < 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care - How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months_More_Than_One",
           Values = paste0(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, " - ",How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months_More_Than_One)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Respondent's Age/Gender is different in the main sheet compared to Illness sheet
  t2_data_filtered %>% 
    filter(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care %in% "Yes, I have fallen seriously ill" & 
             (Selected_HH_Member_Age_Illness != Respondent_Age | Gender_Of_Interviewee != Selected_HH_Member_Gender_Illness)) %>% 
    mutate(issue = "Respondent's Age/Gender is different in the main sheet compared to Illness sheet",
           Questions = "Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care - Respondent_Age - Selected_HH_Member_Age_Illness - Gender_Of_Interviewee - Selected_HH_Member_Gender_Illness",
           Values = paste0(Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care, " - ",
                           Respondent_Age, " - ", Selected_HH_Member_Age_Illness, " - ", Gender_Of_Interviewee, " - ", Selected_HH_Member_Gender_Illness)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Interview respondent type and Gender of Interviewee are inconsistent
  t2_data_filtered %>% 
    filter((Interviewee_Respondent_Type %in% "Male member of HH" & Gender_Of_Interviewee %in% "Female") |
             (Interviewee_Respondent_Type %in% "Female member of HH" & Gender_Of_Interviewee %in% "Male")) %>% 
    mutate(issue = "Interview respondent type and Gender of Interviewee are inconsistent",
           Questions = "Interviewee_Respondent_Type - Gender_Of_Interviewee",
           Values = paste0(Interviewee_Respondent_Type, " - ",Gender_Of_Interviewee)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Number of HH members fell ill are more than household members
  t2_data_filtered %>% 
    filter(How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months > 
             How_Many_Members_Are_In_Your_Household_Including_Yourself) %>% 
    mutate(issue = "Number of HH members fell ill are more than household members",
           Questions = "How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months - How_Many_Members_Are_In_Your_Household_Including_Yourself",
           Values = paste0(How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months, " - ",How_Many_Members_Are_In_Your_Household_Including_Yourself)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Number of HH members injured are more than household members
  t2_data_filtered %>% 
    filter(How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One > 
             How_Many_Members_Are_In_Your_Household_Including_Yourself) %>% 
    mutate(issue = "Number of HH members injured are more than household members",
           Questions = "How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One - How_Many_Members_Are_In_Your_Household_Including_Yourself",
           Values = paste0(How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One, " - ",How_Many_Members_Are_In_Your_Household_Including_Yourself)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Number of HH members currenlty living in household are more than household members
  t2_data_filtered %>% 
    filter(How_Many_Members_Are_Currently_Living_In_Your_Household > How_Many_Members_Are_In_Your_Household_Including_Yourself) %>% 
    mutate(issue = "Number of HH members currenlty living in household are more than household members",
           Questions = "How_Many_Members_Are_Currently_Living_In_Your_Household - How_Many_Members_Are_In_Your_Household_Including_Yourself",
           Values = paste0(How_Many_Members_Are_Currently_Living_In_Your_Household, " - ",How_Many_Members_Are_In_Your_Household_Including_Yourself)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Number of Incoming earning members are more than household members
  t2_data_filtered %>% 
    filter(How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities > How_Many_Members_Are_In_Your_Household_Including_Yourself) %>% 
    mutate(issue = "Number of Incoming earning members are more than household members",
           Questions = "How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities - How_Many_Members_Are_In_Your_Household_Including_Yourself",
           Values = paste0(How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities, " - ",How_Many_Members_Are_In_Your_Household_Including_Yourself)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Injuries: Selected responses and count of HH members does not match
  t2_data_filtered %>% 
    filter(grepl("Yes, I was injured|Yes, a household member was injured", 
                 Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months) & 
             str_count(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, ";") == 0 &
             How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months != 1) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months - How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, " - ",How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(grepl("Yes, I was injured", 
                 Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months) &
             grepl("Yes, a household member was injured", 
                   Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months) &
             str_count(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, ";") == 1 &
             How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months != 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months - How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, " - ",How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(grepl("Yes, I was injured", 
                 Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months) &
             grepl("Yes, more than one household member was injured – excluding myself", 
                   Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months) &
             str_count(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, ";") == 1 &
             How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months <= 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months - How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, " - ",How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months %in% 
             "Yes, more than one household member was injured – excluding myself" &
             How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One < 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months - How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One",
           Values = paste0(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, " - ",How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_More_Than_One)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Respondent's Age/Gender is different in the main sheet compared to Injuries sheet
  t2_data_filtered %>% 
    filter(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months %in% "Yes, I was injured" & 
             (Selected_HH_Member_Age_Injured != Respondent_Age | Gender_Of_Interviewee != Selected_HH_Member_Gender_Injured)) %>% 
    mutate(issue = "Respondent's Age/Gender is different in the main sheet compared to Injuries sheet",
           Questions = "Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months - Respondent_Age - Selected_HH_Member_Age_Injured - Gender_Of_Interviewee - Selected_HH_Member_Gender_Injured",
           Values = paste0(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, " - ",
                           Respondent_Age, " - ", Selected_HH_Member_Age_Injured, " - ", Gender_Of_Interviewee, " - ", Selected_HH_Member_Gender_Injured)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Respondent provided different ages for herself in the two questions
  t2_data_filtered %>% 
    filter(Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months %in% "Yes, I gave birth" & 
             Pregnancy_Related_Care_How_Old_Is_The_Household_Member_Who_Has_Given_Birth_Recently != Respondent_Age) %>% 
    mutate(issue = "Respondent provided different ages for herself in the two questions",
           Questions = "Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months - Respondent_Age - Pregnancy_Related_Care_How_Old_Is_The_Household_Member_Who_Has_Given_Birth_Recently",
           Values = paste0(Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months, " - ",
                           Respondent_Age, " - ", Pregnancy_Related_Care_How_Old_Is_The_Household_Member_Who_Has_Given_Birth_Recently)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Immunization: Selected responses and count of HH members does not match
  t2_data_filtered %>% 
    filter(grepl("Yes, I have received immunizations/vaccinations|Yes, a household member has received immunizations/vaccinations", 
                 Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months) & 
             str_count(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, ";") == 0 &
             How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months != 1) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months - How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, " - ",How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(grepl("Yes, I have received immunizations", 
                 Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months) &
             grepl("Yes, a household member has received immunizations/vaccinations", 
                   Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months) &
             str_count(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, ";") == 1 &
             How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months != 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months - How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, " - ",How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(grepl("Yes, I have received immunizations", 
                 Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months) &
             grepl("Yes, more than one household member has received immunizations/vaccinations - excluding myself", 
                   Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months) &
             str_count(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, ";") == 1 &
             How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months <= 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months - How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, " - ",How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  t2_data_filtered %>% 
    filter(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months %in% 
             "Yes, more than one household member has received immunizations/vaccinations - excluding myself" &
             How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months < 2) %>% 
    mutate(issue = "Selected responses and count of HH members does not match",
           Questions = "Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months - How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months",
           Values = paste0(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months, " - ",How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Respondent's Age/Gender is different in the main sheet compared to Immunization sheet
  t2_data_filtered %>% 
    filter(Have_You_Someone_In_Your_Household_Received_Immunizations_Vaccinations_In_The_Past_6_Months %in% "Yes, I have received immunizations" & 
             (Selected_HH_Member_Age_Immun_Years != Respondent_Age | Gender_Of_Interviewee != Selected_HH_Member_Gender_Immun)) %>% 
    mutate(issue = "Respondent's Age/Gender is different in the main sheet compared to Injuries sheet",
           Questions = "Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months - Respondent_Age - Selected_HH_Member_Age_Immun - Gender_Of_Interviewee - Selected_HH_Member_Gender_Immun",
           Values = paste0(Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, " - ",
                           Respondent_Age, " - ", Selected_HH_Member_Age_Immun_Years, " - ", Gender_Of_Interviewee, " - ", Selected_HH_Member_Gender_Immun)) %>% 
    select(Questions, Values, issue, KEY, qa_status)
)

# Check all question related to medicine and patient dissatisfaction
sufficient_med_cols <- c(
  "Did_You_Your_Household_Member_Find_The_Facility_To_Have_Sufficient_Medication",
  "Injury_Related_Care_Did_You_Your_Household_Member_Find_The_Facility_To_Have_Sufficient_Medication",
  "Pregnancy_Related_Care_Did_You_Your_Household_Member_Find_The_Facility_To_Have_Sufficient_Medication",
  "ANC_PNC_Did_You_Your_Household_Member_Find_The_Facility_To_Have_Sufficient_Medication_B4",
  "ANC_PNC_Did_You_Your_Household_Member_Find_The_Facility_To_Have_Sufficient_Medication_B4_1",
  "Preventative_Care_Did_You_Your_Household_Member_Find_The_Facility_To_Have_Sufficient_Medication")
not_satisfied <- c(
  "Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility",
  "Injury_Related_Care_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility",
  "Pregnancy_Related_Care_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility",
  "ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4",
  "ANC_PNC_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility_B4_1",
  "Preventative_Care_Why_Werent_You_Your_Household_Member_Satisfied_With_Your_Experience_Service_In_The_Health_Facility")

for(col_i in 1:6){
  main_question <- sufficient_med_cols[col_i]
  relev_question <- not_satisfied[col_i]
  
  t2_logical_issues <- rbind(
    t2_logical_issues,
    t2_data_filtered %>% 
      filter(get(main_question) %in% "Yes, we were provided with all the medicines from the health facility" & 
               grepl("Insufficient medicine/lack of necessary medication in the health facility", get(relev_question))) %>% 
      mutate(issue = "Respondent says were were provided with all medicines but also says the reason for disatisfaction was Insufficient medication",
             Questions = paste0(main_question, " - ", relev_question),
             Values = paste0(get(main_question), " - ", get(relev_question))) %>% 
      select(Questions, Values, issue, KEY, qa_status)
  )
}
t2_logical_issues <- t2_logical_issues %>% 
  mutate(Tool="Tool 2") %>% 
  arrange(KEY)

## Separate calculation and structure
# Respondent says I have fallen ill but none of the ages in the repeat sheet match the respondent age 
logical_issues2 <- t2_data_filtered %>%
  filter(grepl("Yes, I have fallen seriously ill", Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) & 
           How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months > 1) %>% 
  select(KEY, Respondent_Age, Gender_Of_Interviewee, 
         Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_That_Needed_Medical_Care) %>% 
  left_join(t2_illness_filtered %>% select(Age_Of_Member, Gender_Of_Member, KEY=PARENT_KEY)) %>% 
  group_by(KEY) %>% 
  mutate(age_matches = case_when(
    Respondent_Age %in% Age_Of_Member ~ "YES",
    TRUE ~ "No"
  ), issue= "Respondent says I have fallen ill but none of the ages in the repeat sheet match the respondent age") %>% 
  filter(age_matches %in% "No") %>% 
  arrange(KEY)
logical_issues2


# Repeat Sheet count does not match main sheet
t2_sub <- t2_data %>% 
  rowwise() %>% 
  mutate(main_sheet_count= case_when(
    !is.na(Did_You_Your_Household_Member_Receive_The_Following_Health_Services_In_The_Past_6_Months) & 
      Did_You_Your_Household_Member_Receive_The_Following_Health_Services_In_The_Past_6_Months %notin% "None" ~ 
      str_count(Did_You_Your_Household_Member_Receive_The_Following_Health_Services_In_The_Past_6_Months, ";")+1
  ), Sheet="Section_B6_Other_Group", Question="Did_You_Your_Household_Member_Receive_The_Following_Health_Services_In_The_Past_6_Months") %>% 
  select(main_sheet_count, Question, KEY, Sheet)

t2_count_mismatch <- rbind(
  t2_income %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Income_Earning_Members_Details") %>%
    full_join(t2_data %>% 
                select(main_sheet_count=How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities, KEY) %>% 
                mutate(Sheet="Income_Earning_Members_Details", Question="How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities"), by=c("KEY", "Sheet")),
  t2_illness %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Illness_Details") %>%
    full_join(t2_data %>% 
                select(main_sheet_count=How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months, KEY) %>% 
                mutate(Sheet="Illness_Details", Question="How_Many_Household_Members_Fell_Seriously_Ill_In_The_Past_6_Months"), by=c("KEY", "Sheet")),
  t2_injuries %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Injuries_Details") %>%
    full_join(t2_data %>% 
                select(main_sheet_count=How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months, KEY) %>% 
                mutate(Sheet="Injuries_Details", Question="How_Many_Household_Members_Have_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months"), by=c("KEY", "Sheet")),
  t2_immunization %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Immunization_Details") %>%
    full_join(t2_data %>% 
                select(main_sheet_count=How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months, KEY) %>% 
                mutate(Sheet="Immunization_Details", Question="How_Many_Household_Members_Received_Immunization_Vaccinations_In_The_Past_6_Months"), by=c("KEY", "Sheet")),
  t2_other %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Section_B6_Other_Group") %>%
    full_join(t2_sub, by=c("KEY", "Sheet"))
) %>% rowwise() %>% filter(repeat_sheet_count %notin% main_sheet_count & !(Sheet=="Income_Earning_Members_Details" & main_sheet_count == 0)) %>% mutate(Tool="Tool 2")

## Check duplicate survey numbers
duplicate_survey_numbers <- t2_data_filtered %>% 
  mutate(Survey_Number = str_remove_all(Survey_Number, "HH|-Male|-Female")) %>%
  group_by(Province, District, Village, HF_Code_based_on_sample, HF_Name_based_on_Sample, Survey_Number) %>% 
  summarize(Total_People_interviewed=length(Survey_Number)) %>% filter(Total_People_interviewed>2)
         
## The male respondent agreed for the interview with the female member but there is not data for the female member
response_crossmatch <- full_join(
  t2_data_filtered %>%
    filter(Consent != "No" & Interviewee_Respondent_Type %in% "Male member of HH" &
             Do_You_Agree_If_My_Female_Colleague_Interview_A_Female_Member_Of_Your_Household %in% "Yes") %>%
    select(KEY, Province, District, Village, HF_Code_based_on_sample, HF_Name_based_on_Sample, Survey_Number,
           Do_You_Agree_If_My_Female_Colleague_Interview_A_Female_Member_Of_Your_Household,
           Reason_for_noconsent_of_Female_Member_Interview) %>% 
    mutate(Survey_Number = str_remove_all(Survey_Number, "HH|-Male|-Female")),
    # anti_join(duplicate_survey_numbers),
  t2_data_filtered %>%
    filter(Consent != "No" & Interviewee_Respondent_Type %in% "Female member of HH") %>%
    select(Key_female=KEY, Province, District, Village, HF_Code_based_on_sample, HF_Name_based_on_Sample, Survey_Number) %>%
    mutate(Survey_Number = str_remove_all(Survey_Number, "HH|-Male|-Female")),
    # anti_join(duplicate_survey_numbers),
  by=c("Province", "District", "Village", "HF_Code_based_on_sample", "HF_Name_based_on_Sample", "Survey_Number")) %>%
  relocate(Key_female, .after = KEY)

# response_crossmatch %>% janitor::get_dupes(KEY) %>% View

missing_male_female_data <- rbind(
  response_crossmatch %>% 
    filter(is.na(Key_female) & Reason_for_noconsent_of_Female_Member_Interview %notin% "IEA did not allow female interviews to be conducted.") %>%
    select(-Key_female) %>% 
    mutate(issue="The male respondent agreed for the interview with the female member but there is not data for the female member"),
  response_crossmatch %>% 
    filter(is.na(KEY)) %>%
    mutate(KEY=Key_female, Key_female=NULL, issue="The Male respondent's interview is missing") 
)

## Responses cross match between male and female household members ---------------------------------
overall_questions <- c("How_Many_Members_Are_In_Your_Household_Including_Yourself",	"How_Many_Members_Are_Currently_Living_In_Your_Household",
                       "How_Many_Of_These_Members_Are_Engaged_In_Income_Earning_Activities", "Is_This_A_Female_Headed_Household", "Is_Your_Household_Headed_By_An_Elderly_Person",
                       "Is_Your_Household_Headed_By_A_Child_Too_Young_To_Work")
response_crossmatch <- left_join(
  t2_data_filtered %>%
    filter(Consent != "No" & Interviewee_Respondent_Type %in% "Male member of HH" &
             Do_You_Agree_If_My_Female_Colleague_Interview_A_Female_Member_Of_Your_Household %in% "Yes") %>%
    select(Key_male=KEY, Province, District, Village, HF_Code_based_on_sample, HF_Name_based_on_Sample, Survey_Number,
           all_of(overall_questions)) %>%
    mutate(Survey_Number = str_remove_all(Survey_Number, "HH|-Male|-Female")) %>%
    mutate(across(all_of(overall_questions), as.character)) %>% 
    pivot_longer(all_of(overall_questions), names_to = "questions",  values_to = "Response_male"),
  t2_data_filtered %>%
    filter(Consent != "No" & Interviewee_Respondent_Type %in% "Female member of HH") %>%
    select(Key_female=KEY, Province, District, Village, HF_Code_based_on_sample, HF_Name_based_on_Sample, Survey_Number,
           all_of(overall_questions)) %>%
    mutate(Survey_Number = str_remove_all(Survey_Number, "HH|-Male|-Female")) %>%
    mutate(across(all_of(overall_questions), as.character)) %>% 
    pivot_longer(all_of(overall_questions), names_to = "questions",  values_to = "Response_female")) %>%
  relocate(Key_female, .after = Key_male)

# Shabar confirmed that the responses don't have to be the same
mismatch_male_fem_resp <- response_crossmatch %>% filter(Response_male %notin% Response_female)

# Rejected and approved data -----------------------------------------------------------------------
rejec_approved <- rbind(
  hf_t1_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "1.2"),
  hf_t2_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "1.2"),
  hf_t3_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "1.3"),
  t2_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "2"),
  t3_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "3")
)

## Check against Tools 1.1 and 1.2 -----------------------------------------------------------------
hf_t1_data_wide %>% filter(HF_Code_based_on_sample %notin% hf_t2_data_wide$HF_Code_based_on_sample)
hf_t2_data_wide %>% filter(HF_Code_based_on_sample %notin% hf_t1_data_wide$HF_Code_based_on_sample)

# Tool 2 checks
t1_missing_HFs <- rbind(
  hf_t1_data_wide %>%
    filter(HF_Code_based_on_sample %notin% hf_t3_data_filtered$HF_Code_based_on_sample) %>%
    select(Province, District, HF_Type_based_on_sample, HF_Type_Based_on_SV, HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample) %>% unique() %>%
    mutate(not_found_in = "Tool 1.3"),
  hf_t1_data_wide %>%
    filter(HF_Code_based_on_sample %notin% t2_data_filtered$HF_Code_based_on_sample) %>%
    select(Province, District, HF_Type_based_on_sample, HF_Type_Based_on_SV, HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample) %>% unique() %>%
    mutate(not_found_in = "Tool 2"),
  hf_t1_data_wide %>%
    filter(HF_Code_based_on_sample %notin% t3_data_filtered$HF_Code_based_on_sample) %>%
    select(Province, District, HF_Type_based_on_sample, HF_Type_Based_on_SV, HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample) %>% unique() %>%
    mutate(not_found_in = "Tool 3")
)

t1_missing_HFs <- t1_missing_HFs %>%
  group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample) %>%
  mutate(not_found_in = paste0(not_found_in, collapse = " & ")) %>% unique()
write.xlsx(t1_missing_HFs, "output/Tool1.1_HF_missing_in_other_tools.xlsx")

hf_t3_data_filtered %>% filter(HF_Code_based_on_sample %notin% hf_t1_data_wide$HF_Code_based_on_sample)
t2_data_filtered %>% filter(HF_Code_based_on_sample %notin% hf_t1_data_wide$HF_Code_based_on_sample)
t3_data_filtered %>% filter(HF_Code_based_on_sample %notin% hf_t1_data_wide$HF_Code_based_on_sample)

# Check sample data
hf_t1_data_wide %>%
  select(Province, District, HF_Code_based_on_sample, HF_Name_based_on_Sample) %>% unique() %>%
  janitor::get_dupes(HF_Code_based_on_sample)
hf_t2_data_wide %>%
  select(Province, District, HF_Code_based_on_sample, HF_Name_based_on_Sample) %>% unique() %>%
  janitor::get_dupes(HF_Code_based_on_sample)
hf_t3_data_filtered %>%
  select(Province, District, HF_Code_based_on_sample, HF_Name_based_on_Sample) %>% unique() %>%
  janitor::get_dupes(HF_Code_based_on_sample)
t2_data %>%
  select(Province, District, HF_Code_based_on_sample, HF_Name_based_on_Sample) %>% unique() %>%
  janitor::get_dupes(HF_Code_based_on_sample)
t3_data_filtered %>%
  select(Province, District, HF_Code_based_on_sample, HF_Name_based_on_Sample) %>% unique() %>%
  janitor::get_dupes(HF_Code_based_on_sample)

## Check keys 
if(any(duplicated(hf_t1_data_wide$KEY)) | any(duplicated(hf_t2_data_wide$KEY)) | 
   any(duplicated(hf_t3_data_filtered$KEY)) | any(duplicated(t2_data_filtered$KEY)) | 
   any(duplicated(t3_data_filtered$KEY))){
  print("Duplicates found!")
}

# Export list --------------------------------------------------------------------------------------
logical_issues <- plyr::rbind.fill(
  t1.1_logical_issues,
  t1.2_logical_issues,
  t1.3_logical_issues,
  t2_logical_issues
)
count_mismatch <- plyr::rbind.fill(
  t1.1_count_mismatch,
  t1.2_count_mismatch,
  t2_count_mismatch
)

logical_issues_list <- list(
  logical_issues=logical_issues,
  logical_issues2=logical_issues2,
  duplicate_survey_numbers=duplicate_survey_numbers,
  missing_male_female_data=missing_male_female_data,
  repeat_sheet_issues=count_mismatch,
  mismatch_male_fem_resp=mismatch_male_fem_resp,
  rejec_approved=rejec_approved
)

# Remove extra objects -----------------------------------------------------------------------------
rm()


