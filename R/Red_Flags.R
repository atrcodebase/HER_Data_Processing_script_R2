### Red Flags
HF_cols <- c("HF_Code_based_on_sample", "HF_Name_based_on_Sample", "SP_Name_based_on_sample")

## Tool 1 Health Facility Level --------------------------------------------------------------------
## Percentages
red_flag_perc <- rbind(
  # HF Head reports GRM has not been established
  hf_t1_data %>%
    group_by(
      Red_Flag = "Lack of functioning reporting channel",
      Indicator = "% of HFs reporting GRMs have not been established"
    ) %>%
    count(response = Has_A_Grievance_Redress_Mechanism_GRM_Been_Established_At_The_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # HF Head reports GRM logbook is not maintained at the facility
  hf_t2_data %>%
    group_by(
      Red_Flag = "Lack of functioning reporting channel",
      Indicator = "% of HFs reporting GRM logbook is not maintained at the facility"
    ) %>%
    count(response = Is_A_Grm_Logbook_Maintained_At_The_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # HF does not have grievance reporting information visible at the facility
  hf_t2_data %>%
    group_by(
      Red_Flag = "Lack of functioning reporting channel",
      Indicator = "% of HFs with no grievance reporting information visible at facility"
    ) %>%
    count(resp_1=Is_Information_About_Grievance_Reporting_Mechanism_Visible_And_Accessible_Within_The_Facility,
          resp_2=Is_There_A_Complaint_Box_On_Site,
          resp_3=Is_There_A_Phone_Number_For_Grievance_Reporting_On_Display_At_The_Facility,
          resp_4=Enumerator_To_Call_The_Phone_Number_And_Confim_If_Someone_Pick_Up,
          resp_5=Please_Ask_If_This_Is_The_Number_To_Report_Complaints_Through_The_Grm_Is_It_The_Correct_Number_For_Grm_Complaints,
          resp_6=Is_There_An_Email_Address_For_Grievance_Reporting_On_Display_At_The_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>%
    # Filtering through 3 conditions
    filter((resp_1=="No" & resp_2=="No" & resp_3=="No" & resp_6=="No") |
             (resp_1=="No" & resp_2=="No" & resp_3=="Yes" & resp_4=="No" & resp_6=="No") |
             (resp_1=="No" & resp_2=="No" & resp_3=="Yes" & resp_4=="Yes" & resp_5=="No" & resp_6=="No")) %>%
    select(-starts_with("resp_"), Count=n),
  # HF Head reports receipt of complaints related to GBV, SH, or SEA
  hf_t1_data %>%
    group_by(
      Red_Flag = "GBV/SH instances",
      Indicator = "% of HFs reporting receipt of complaints related to gender-based violence (GBV), sexual harassment, or abuse/explotiation"
    ) %>%
    count(response = Have_Any_Complaints_Related_To_Gender_Based_Violence_Gbv_Sexual_Harassment_Or_Abuse_Exploitation_Been_Received_In_This_Hf) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="Yes") %>% select(-response, Count=n),
  # HF Heads reports there is no Occupational Health and Safety incident/accident reporting system at the facility
  hf_t1_data %>%
    group_by(
      Red_Flag = "Absence of OHS incident/accident reporting",
      Indicator = "% of HFs reporting no OHS incident/accident reporting system at the facility"
    ) %>%
    count(response = Is_There_An_Occupational_Health_And_Safety_Incident_Accident_Reporting_System_Established_At_The_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # HF reports OHS incident logbook is not maintained at facility
  hf_t1_data %>%
    group_by(
      Red_Flag = "Absence of OHS incident/accident reporting",
      Indicator = "Tool 1.1: % of HFs reporting OHS incident logbook is not maintained at the facility"
    ) %>%
    count(response = Is_An_Occupational_Health_And_Safety_Ohs_Incident_Logbook_Maintained_At_The_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # Tool 1.2
  hf_t2_data %>%
    group_by(
      Red_Flag = "Absence of OHS incident/accident reporting",
      Indicator = "Tool 1.2: % of HFs reporting OHS incident logbook is not maintained at the facility"
    ) %>%
    count(response = Is_An_OHS_Incident_Logbook_Maintained_At_The_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # HF Head reports a serious injury at the facility in the last 6 months
  hf_t1_data %>%
    group_by(
      Red_Flag = "Serious or fatal incident in the last 6 months",
      Indicator = "% of HFs reporting a serious injury occurred at the facility in the last 6 months"
    ) %>%
    count(response = In_The_Past_6_Months_Since_Jadi_1401_Has_There_Been_A_Serious_Injury_Of_Hf_Staff_Requiring_Hospitalisation_Medical_Care_During_Working_Hours_At_This_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="Yes") %>% select(-response, Count=n),
  # HF Head reports a fatal incident at the facility in the last 6 months
  hf_t1_data %>%
    group_by(
      Red_Flag = "Serious or fatal incident in the last 6 months",
      Indicator = "% of HFs reporting a fatal indicent occurred at the facility in the last 6 months"
    ) %>%
    count(response = In_The_Past_6_Months_Since_Jadi_1401_Has_There_Been_A_Fatality_Of_Hf_Staff_During_Working_Hours_At_This_Facility) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="Yes") %>% select(-response, Count=n),
  # HF Head reports a security incident occurred in the past 6 months (by type)
  hf_t1_data %>%
    mutate(Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months_Since_Jadi_1401 = case_when(
      Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months_Since_Jadi_1401 %in%
        c("Yes, there has been only one incident", "Yes, there have been more than one incidents") ~ "Yes",
      TRUE ~ Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months_Since_Jadi_1401
    )) %>%
    group_by(
      Red_Flag = "Security incident(s) occurred in the last 6 months",
      Indicator = "% of HFs reporting a security incident occurred in the last 6 months"
    ) %>%
    count(response = Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months_Since_Jadi_1401) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response == "Yes") %>%
    select(-response, Count=n),
  # HF Head reports sharps not disinfected before disposal
  hf_t2_data %>%
    group_by(
      Red_Flag = "Lack of implementation of the Healthcare waste management plans",
      Indicator = "% of HFs reporting sharps are not disinfected before disposal"
    ) %>%
    count(response = Are_Sharps_Such_As_Hypodermic_Needles_Saws_Pipettes_Scalpels_Broken_Glass_And_Blades_Disinfected_Hypochlorite_Or_An_Equivalent_Chemical_Reagent_Before_Disposal) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # HF Head reports infectious and non-infectious waste is not segregated at collection points
  hf_t2_data %>%
    group_by(
      Red_Flag = "Lack of implementation of the Healthcare waste management plans",
      Indicator = "% of HFs reporting infectious and non-infectious waste is not segregated at collection points"
    ) %>%
    count(response = Are_Infectious_And_Non_Infectious_Waste_Materials_Segregated_Immediately_After_Use_At_Collection_Points) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # Waste collector not wearing PPE while collecting or disposing of medical waste AND
  hf_t2_data %>%
    group_by(
      Red_Flag = "Lack of implementation of the Healthcare waste management plans",
      Indicator = "% of HFs in which waste collector is not wearing PPE while collecting or disposing of waste"
    ) %>%
    count(response = Is_The_Waste_Collector_Guard_Wearing_Any_Of_PPE) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n),
  # Vaccinator not wearing PPE when administering a vaccine/injection
  hf_t2_data %>%
    group_by(
      Red_Flag = "Lack of implementation of the Healthcare waste management plans",
      Indicator = "% of HFs in which vaccinator is not wearing PPE when administering a vaccine or injection"
    ) %>%
    count(response = Is_The_Vaccinator_Wearing_Any_Of_PPE_While_Administering_An_Injection) %>%
    mutate(Percentage = round(n/sum(n)*100, 2)) %>% filter(response=="No") %>% select(-response, Count=n)
)

## Report
red_flag_rep <- plyr::rbind.fill(
  # HF Head reports GRM has not been established
  hf_t1_data %>%
    filter(Has_A_Grievance_Redress_Mechanism_GRM_Been_Established_At_The_Facility == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Has_A_Grievance_Redress_Mechanism_GRM_Been_Established_At_The_Facility",
           Value="No",
           Red_Flag = "Lack of functioning reporting channel",
           Indicator="HFs reporting GRMs have not been established"),
  # HF Head reports GRM logbook is not maintained at the facility
  hf_t2_data %>%
    filter(Is_A_Grm_Logbook_Maintained_At_The_Facility == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Is_A_Grm_Logbook_Maintained_At_The_Facility",
           Value="No",
           Red_Flag = "Lack of functioning reporting channel",
           Indicator="HFs reporting GRM logbook is not maintained at the facility"),
  # HF does not have grievance reporting information visible at the facility
  hf_t2_data %>%
    filter((Is_Information_About_Grievance_Reporting_Mechanism_Visible_And_Accessible_Within_The_Facility=="No" &
              Is_There_A_Complaint_Box_On_Site=="No" &
              Is_There_A_Phone_Number_For_Grievance_Reporting_On_Display_At_The_Facility=="No" &
              Is_There_An_Email_Address_For_Grievance_Reporting_On_Display_At_The_Facility=="No") |
             (Is_Information_About_Grievance_Reporting_Mechanism_Visible_And_Accessible_Within_The_Facility=="No" &
                Is_There_A_Complaint_Box_On_Site=="No" &
                Is_There_A_Phone_Number_For_Grievance_Reporting_On_Display_At_The_Facility=="Yes" &
                Enumerator_To_Call_The_Phone_Number_And_Confim_If_Someone_Pick_Up=="No" &
                Is_There_An_Email_Address_For_Grievance_Reporting_On_Display_At_The_Facility=="No") |
             (Is_Information_About_Grievance_Reporting_Mechanism_Visible_And_Accessible_Within_The_Facility=="No" &
                Is_There_A_Complaint_Box_On_Site=="No" &
                Is_There_A_Phone_Number_For_Grievance_Reporting_On_Display_At_The_Facility=="Yes" &
                Enumerator_To_Call_The_Phone_Number_And_Confim_If_Someone_Pick_Up=="Yes" &
                Please_Ask_If_This_Is_The_Number_To_Report_Complaints_Through_The_Grm_Is_It_The_Correct_Number_For_Grm_Complaints=="No" &
                Is_There_An_Email_Address_For_Grievance_Reporting_On_Display_At_The_Facility=="No")) %>%
    mutate(Question="Is_Information_About_Grievance_Reporting_Mechanism_Visible_And_Accessible_Within_The_Facility",
           Value=Is_Information_About_Grievance_Reporting_Mechanism_Visible_And_Accessible_Within_The_Facility,
           Question_2="Is_There_A_Complaint_Box_On_Site",
           Value_2=Is_There_A_Complaint_Box_On_Site,
           Question_3="Is_There_A_Phone_Number_For_Grievance_Reporting_On_Display_At_The_Facility",
           Value_3=Is_There_A_Phone_Number_For_Grievance_Reporting_On_Display_At_The_Facility,
           Question_4="Enumerator_To_Call_The_Phone_Number_And_Confim_If_Someone_Pick_Up",
           Value_4=Enumerator_To_Call_The_Phone_Number_And_Confim_If_Someone_Pick_Up,
           Question_5="Please_Ask_If_This_Is_The_Number_To_Report_Complaints_Through_The_Grm_Is_It_The_Correct_Number_For_Grm_Complaints",
           Value_5=Please_Ask_If_This_Is_The_Number_To_Report_Complaints_Through_The_Grm_Is_It_The_Correct_Number_For_Grm_Complaints,
           Question_6="Is_There_An_Email_Address_For_Grievance_Reporting_On_Display_At_The_Facility",
           Value_6=Is_There_An_Email_Address_For_Grievance_Reporting_On_Display_At_The_Facility,
           Red_Flag = "Lack of functioning reporting channel",
           Indicator="HFs with no grievance reporting information visible at facility") %>%
    select(all_of(HF_cols), starts_with("Question"), starts_with("Value"), Red_Flag, Indicator),
  # HF Head reports receipt of complaints related to GBV, SH, or SEA
  hf_t1_data %>%
    filter(Have_Any_Complaints_Related_To_Gender_Based_Violence_Gbv_Sexual_Harassment_Or_Abuse_Exploitation_Been_Received_In_This_Hf == "Yes") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Have_Any_Complaints_Related_To_Gender_Based_Violence_Gbv_Sexual_Harassment_Or_Abuse_Exploitation_Been_Received_In_This_Hf",
           Value="Yes",
           Red_Flag = "GBV/SH instances",
           Indicator="HFs reporting receipt of complaints related to gender-based violence (GBV), sexual harassment, or abuse/explotiation"),
  # HF Heads reports there is no Occupational Health and Safety incident/accident reporting system at the facility
  hf_t1_data %>%
    filter(Is_There_An_Occupational_Health_And_Safety_Incident_Accident_Reporting_System_Established_At_The_Facility == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Is_There_An_Occupational_Health_And_Safety_Incident_Accident_Reporting_System_Established_At_The_Facility",
           Value="No",
           Red_Flag = "Absence of OHS incident/accident reporting",
           Indicator="HFs reporting no OHS incident/accident reporting system at the facility"),
  # HF reports OHS incident logbook is not maintained at facility
  hf_t1_data %>%
    filter(Is_An_Occupational_Health_And_Safety_Ohs_Incident_Logbook_Maintained_At_The_Facility == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Is_An_Occupational_Health_And_Safety_Ohs_Incident_Logbook_Maintained_At_The_Facility",
           Value="No",
           Red_Flag = "Absence of OHS incident/accident reporting",
           Indicator="HFs reporting OHS incident logbook is not maintained at the facility"),
  # Tool 1.2
  hf_t2_data %>%
    filter(Is_An_OHS_Incident_Logbook_Maintained_At_The_Facility == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Is_An_OHS_Incident_Logbook_Maintained_At_The_Facility",
           Value="No",
           Red_Flag = "Absence of OHS incident/accident reporting",
           Indicator="HFs reporting OHS incident logbook is not maintained at the facility"),
  # HF Head reports a serious injury at the facility in the last 6 months
  hf_t1_data %>%
    filter(In_The_Past_6_Months_Since_Jadi_1401_Has_There_Been_A_Serious_Injury_Of_Hf_Staff_Requiring_Hospitalisation_Medical_Care_During_Working_Hours_At_This_Facility == "Yes") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="In_The_Past_6_Months_Since_Jadi_1401_Has_There_Been_A_Serious_Injury_Of_Hf_Staff_Requiring_Hospitalisation_Medical_Care_During_Working_Hours_At_This_Facility",
           Value="Yes",
           Red_Flag = "Serious or fatal incident in the last 6 months",
           Indicator="HFs reporting a serious injury occurred at the facility in the last 6 months"),
  # HF Head reports a fatal incident at the facility in the last 6 months
  hf_t1_data %>%
    filter(In_The_Past_6_Months_Since_Jadi_1401_Has_There_Been_A_Fatality_Of_Hf_Staff_During_Working_Hours_At_This_Facility == "Yes") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="In_The_Past_6_Months_Since_Jadi_1401_Has_There_Been_A_Fatality_Of_Hf_Staff_During_Working_Hours_At_This_Facility",
           Value="Yes",
           Red_Flag = "Serious or fatal incident in the last 6 months",
           Indicator="HFs reporting a fatal indicent occurred at the facility in the last 6 months"),
  # HF Head reports a security incident occurred in the past 6 months (by type)
  hf_t1_data %>%
    filter(Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months_Since_Jadi_1401 %in%
             c("Yes, there has been only one incident", "Yes, there have been more than one incidents")) %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Have_There_Been_Any_Safety_Or_Security_Incidents_At_This_Facility_In_The_Last_6_Months_Since_Jadi_1401",
           Value="Yes, there has been only one incident / Yes, there have been more than one incidents",
           Red_Flag = "Security incident(s) occurred in the last 6 months",
           Indicator="HFs reporting a security incident occurred in the last 6 months"),
  # HF Head reports sharps not disinfected before disposal
  hf_t2_data %>%
    filter(Are_Sharps_Such_As_Hypodermic_Needles_Saws_Pipettes_Scalpels_Broken_Glass_And_Blades_Disinfected_Hypochlorite_Or_An_Equivalent_Chemical_Reagent_Before_Disposal == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Are_Sharps_Such_As_Hypodermic_Needles_Saws_Pipettes_Scalpels_Broken_Glass_And_Blades_Disinfected_Hypochlorite_Or_An_Equivalent_Chemical_Reagent_Before_Disposal",
           Value="No",
           Red_Flag = "Lack of implementation of the Healthcare waste management plans",
           Indicator="HFs reporting sharps are not disinfected before disposal"),
  # HF Head reports infectious and non-infectious waste is not segregated at collection points
  hf_t2_data %>%
    filter(Are_Infectious_And_Non_Infectious_Waste_Materials_Segregated_Immediately_After_Use_At_Collection_Points == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Are_Infectious_And_Non_Infectious_Waste_Materials_Segregated_Immediately_After_Use_At_Collection_Points",
           Value="No",
           Red_Flag = "Lack of implementation of the Healthcare waste management plans",
           Indicator="HFs reporting infectious and non-infectious waste is not segregated at collection points"),
  # Waste collector not wearing PPE while collecting or disposing of medical waste AND
  hf_t2_data %>%
    filter(Is_The_Waste_Collector_Guard_Wearing_Any_Of_PPE == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Is_The_Waste_Collector_Guard_Wearing_Any_Of_PPE",
           Value="No",
           Red_Flag = "Lack of implementation of the Healthcare waste management plans",
           Indicator="HFs in which waste collector is not wearing PPE while collecting or disposing of waste"),
  # Vaccinator not wearing PPE when administering a vaccine/injection
  hf_t2_data %>%
    filter(Is_The_Vaccinator_Wearing_Any_Of_PPE_While_Administering_An_Injection == "No") %>%
    select(all_of(HF_cols)) %>%
    mutate(Question="Is_The_Vaccinator_Wearing_Any_Of_PPE_While_Administering_An_Injection",
           Value="No",
           Red_Flag = "Lack of implementation of the Healthcare waste management plans",
           Indicator="HFs in which vaccinator is not wearing PPE when administering a vaccine or injection")
)

red_flag_rep <- red_flag_rep %>%
  select(all_of(HF_cols), Red_Flag, Indicator, Question, Value, Question_2, Value_2, Question_3, Value_3,
         Question_4, Value_4, Question_5, Value_5, Question_6, Value_6)


## Tool 2 Household Level --------------------------------------------------------------------------
fallen_ill <- c(
  "Yes, I have fallen seriously ill",
  "Yes, a household member has fallen seriously ill",
  "Yes, more than one household member fell seriously ill – excluding myself")
injured <- c(
  "Yes, I was injured",
  "Yes, a household member was injured",
  "Yes, more than one household member was injured – excluding myself")
visited_HF <- c(
  "No, but went to a community health worker", 
  "No, but went to a pharmacist",
  "No, but received traditional (Hakeem/Yunanee/bone setter) medical care",
  "No, but went to a mullah/religious figure for spiritual treatment") # Select one question: no need for escape characters
not_visited_HF <- c(
  "The health facility is too far from our community",
  "We cannot afford transportation cost to get to the health facility",
  "The health facility does not provide good services, and we therefore do not believe they are efficient in treatment of illnesses",
  # "We believe in religious treatment than going to a health facility", # Not in the RF responses
  "We are not allowed by our household head to visit a health facility",
  "It is not secure to commute to the health facility",
  "My household member could not travel to the health facility without a mahram \\[N/A for males\\]",
  "We were prevented from visiting the health facility by a local authority",
  "The facility staff would not see me/my household member because of my/their gender",
  "The facility staff would not see me/my household member because of my/their ethnicity",
  "The facility staff would not see me/my household member because of my/their social standing",
  "My household member was too ill to seek medical attention outside of the home")
prc_visited_hf <- c(
  "No, but went to a community health worker",
  "No, but delivered with the help of a traditional birth attendant at home",
  "No, but delivered with the help of a skilled birth attendant/local midwife at home",
  "No, didn’t go anywhere and delivered at home")
prc_delivery <- c(
  "Yes, I was pregnant \\(but did not give birth\\)",
  "Yes, a household member was pregnant \\(but did not give birth\\)",
  "Yes, more than one household member was pregnant – excluding myself", # different from last one 
  "Yes, I gave birth", 
  "Yes, a household member gave birth",
  "Yes, more than one household member gave birth – excluding myself")
prc_delivery_birth <- c(
  "Yes, I gave birth", 
  "Yes, a household member gave birth",
  "Yes, more than one household member gave birth – excluding myself")
anc_visited_hf <- c(
  "No, but went to a community health worker", 
  "No, she didn’t go to any health facility")

# HHs in which women were seriously ill but did not visit health facility
indicator1 <- t2_data %>% 
  right_join(t2_illness %>% select(Gender_Of_Member, Key_Unique=KEY, PARENT_KEY), by=c("KEY"="PARENT_KEY")) %>% 
  mutate(HH_females_seriously_ill = case_when(
    Gender_Of_Member %notin% "Female" ~ NA, # Excluding male HH members and all HHs with No Illnesses from percentages 
    grepl(paste0(fallen_ill, collapse = "|"), Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_Since_Dalwa_1401_That_Needed_Medical_Care) & 
    Gender_Of_Member %in% "Female" & 
      Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Go_To_A_Health_Facility_To_Treat_The_Illness %in% visited_HF & 
      grepl(paste0(not_visited_HF, collapse = "|"), 
            Why_Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Not_Go_To_A_Health_Facility_To_Treat_His_Her_Illness) ~ "Yes",
    TRUE ~ "No"),
    Indicator="HHs in which women were serisouly ill but did not visit health facility")
# HHs in which women were hurt or physically injured but did not visit the health facility
indicator2 <- t2_data %>% 
  right_join(t2_injuries %>% select(Gender_of_HH_Who_got_injured, Key_Unique=KEY, PARENT_KEY), by=c("KEY"="PARENT_KEY")) %>% 
  mutate(HH_females_injured = case_when(
    Gender_of_HH_Who_got_injured %notin% "Female" ~ NA, # Excluding male HH members and all HH with No injuries from percentages
    grepl(paste0(injured, collapse = "|"), Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_Since_Dalwa_1401) &
    Gender_of_HH_Who_got_injured %in% "Female" & 
      Injury_Related_Care_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Go_To_A_Health_Facility_To_Treat_The_Injury %in% visited_HF &
      grepl(paste0(not_visited_HF, collapse = "|"), 
            Injury_Related_Care_Why_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Not_Go_To_A_Health_Facility_To_Treat_His_Her_Injury) ~ "Yes",
    TRUE ~ "No"),
  Indicator="HHs in which women were hurt or physically injured but did not visit the health facility")

# Pregnancy related Indicators
indicator3 <- t2_data %>% 
  mutate(
    # HHs in which women gave birth but did not visit the HF for the delivery 
    HH_women_gave_birth = case_when(
      # Gender_Of_Interviewee %notin% "Female" ~ NA, # Excluding male HH members from percentages
      !grepl(paste0(prc_delivery_birth, collapse = "|"), 
            Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401) ~ NA,
      grepl(paste0(prc_delivery_birth, collapse = "|"), Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401) &
        Pregnancy_Related_Care_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Go_To_A_Health_Facility_For_Delivery %in% prc_visited_hf & 
        grepl(paste0(not_visited_HF, collapse = "|"), Pregnancy_Related_Care_Why_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Not_Go_To_A_Health_Facility_For_Delivery) ~ "Yes",
      TRUE ~ "No"
    ),
    # HHs in which women were pregnant but did not visit the HF for ANC
    HH_women_didnt_visit_for_ANC = case_when(
      # Gender_Of_Interviewee %notin% "Female" ~ NA, # Excluding male HH members from percentages
      !grepl(paste0(prc_delivery, collapse = "|"), 
            Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401) ~ NA, 
      grepl(paste0(prc_delivery, collapse = "|"), Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401) &
        ANC_PNC_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Go_To_Health_For_At_Least_One_Antenatal_Visit %in% anc_visited_hf & 
        grepl(paste0(not_visited_HF, collapse = "|"), 
        ANC_PNC_Why_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit) ~ "Yes",
      TRUE ~ "No"
    ),
    # HHs in which women gave birth but did not visit the HF for PNC
    HH_women_didnt_visit_for_PNC = case_when(
      # Gender_Of_Interviewee %notin% "Female" ~ NA, # Excluding male HH members from percentages
      !grepl(paste0(prc_delivery_birth, collapse = "|"), 
             Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401) ~ NA,
      grepl(paste0(prc_delivery_birth, collapse = "|"), Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401) &
        ANC_PNC_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Go_To_A_Health_For_At_Least_One_Postnatal_Visit %in% "No, she didn’t go to any health facility" &
        grepl(paste0(not_visited_HF, collapse = "|"), 
              ANC_PNC_Why_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Not_Go_To_A_Health_Facility_For_A_Postnatal_Visit) ~ "Yes",
      TRUE ~ "No"
    ), Key_Unique=KEY)

### Percentages
HH_RF_Perc <- rbind(
  # HHs in which women were serisouly ill but did not visit health facility
  indicator1 %>% 
    group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator) %>% 
    filter(!is.na(HH_females_seriously_ill)) %>%
    count(HH_females_seriously_ill, name="Count") %>% 
    mutate(Percentage = round(Count/sum(Count)*100, 2)) %>% 
    filter(HH_females_seriously_ill %in% "Yes" & Percentage > 25) %>% select(-HH_females_seriously_ill),
  # HHs in which women were hurt or physically injured but did not visit the health facility
  indicator2 %>% 
    group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator) %>% 
    filter(!is.na(HH_females_injured)) %>%
    count(HH_females_injured, name="Count") %>% 
    mutate(Percentage = round(Count/sum(Count)*100, 2)) %>% 
    filter(HH_females_injured %in% "Yes" & Percentage > 25) %>% select(-HH_females_injured),
  # HHs in which women gave birth but did not visit the HF for the delivery 
  indicator3 %>% 
    group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator="HHs in which women gave birth but did not visit the HF for the delivery") %>% 
    filter(!is.na(HH_women_gave_birth)) %>%
    count(HH_women_gave_birth, name="Count") %>% 
    mutate(Percentage = round(Count/sum(Count)*100, 2)) %>% 
    filter(HH_women_gave_birth %in% "Yes" & Percentage > 25) %>% select(-HH_women_gave_birth),
  # HHs in which women were pregnant but did not visit the HF for ANC
  indicator3 %>% 
    group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator="HHs in which women were pregnant but did not visit the HF for ANC") %>% 
    filter(!is.na(HH_women_didnt_visit_for_ANC)) %>%
    count(HH_women_didnt_visit_for_ANC, name="Count") %>% 
    mutate(Percentage = round(Count/sum(Count)*100, 2)) %>% 
    filter(HH_women_didnt_visit_for_ANC %in% "Yes" & Percentage > 25) %>% select(-HH_women_didnt_visit_for_ANC),
  # HHs in which women gave birth but did not visit the HF for PNC
  indicator3 %>% 
    group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator="HHs in which women gave birth but did not visit the HF for PNC") %>% 
    filter(!is.na(HH_women_didnt_visit_for_PNC)) %>%
    count(HH_women_didnt_visit_for_PNC, name="Count") %>% 
    mutate(Percentage = round(Count/sum(Count)*100, 2)) %>% 
    filter(HH_women_didnt_visit_for_PNC %in% "Yes" & Percentage > 25) %>% select(-HH_women_didnt_visit_for_PNC)
) %>% ungroup()

### Report
HH_RF_report <- bind_rows(
  # HHs in which women were serisouly ill but did not visit health facility
  indicator1 %>% 
    filter(HH_females_seriously_ill %in% "Yes" & HF_Code_based_on_sample %in% 
             HH_RF_Perc$HF_Code_based_on_sample[HH_RF_Perc$Indicator %in% "HHs in which women were serisouly ill but did not visit health facility"]) %>% 
    mutate(Question="Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_Since_Dalwa_1401_That_Needed_Medical_Care",
           Value=Have_You_Or_Someone_In_Your_Household_Fallen_Seriously_Ill_In_The_Past_6_Months_Since_Dalwa_1401_That_Needed_Medical_Care,
           Question_2="Gender_Of_Member",
           Value_2=Gender_Of_Member,
           Question_3="Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Go_To_A_Health_Facility_To_Treat_The_Illness",
           Value_3=Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Go_To_A_Health_Facility_To_Treat_The_Illness,
           Question_4="Why_Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Not_Go_To_A_Health_Facility_To_Treat_His_Her_Illness",
           Value_4=Why_Did_The_Household_Member_Who_Was_Diagnosed_With_Illness_Not_Go_To_A_Health_Facility_To_Treat_His_Her_Illness) %>% 
    select(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator, KEY, Key_Unique, starts_with("Question"), starts_with("Value")),
  # HHs in which women were hurt or physically injured but did not visit the health facility
  indicator2 %>% 
    filter(HH_females_injured %in% "Yes" & HF_Code_based_on_sample %in% 
             HH_RF_Perc$HF_Code_based_on_sample[HH_RF_Perc$Indicator %in% "HHs in which women were hurt or physically injured but did not visit the health facility"]) %>% 
    mutate(Question="Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_Since_Dalwa_1401",
           Value=Have_You_Someone_In_Your_Household_Gotten_Injured_Or_Physically_Hurt_In_The_Past_6_Months_Since_Dalwa_1401,
           Question_2="Gender_of_HH_Who_got_injured",
           Value_2=Gender_of_HH_Who_got_injured,
           Question_3="Injury_Related_Care_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Go_To_A_Health_Facility_To_Treat_The_Injury",
           Value_3=Injury_Related_Care_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Go_To_A_Health_Facility_To_Treat_The_Injury,
           Question_4="Injury_Related_Care_Why_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Not_Go_To_A_Health_Facility_To_Treat_His_Her_Injury",
           Value_4=Injury_Related_Care_Why_Did_The_Household_Member_Who_Was_Injured_Or_Physically_Hurt_Recently_Not_Go_To_A_Health_Facility_To_Treat_His_Her_Injury) %>% 
    select(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator, KEY, Key_Unique, starts_with("Question"), starts_with("Value")),
  # HHs in which women gave birth but did not visit the HF for the delivery
  indicator3 %>% 
    filter(HH_women_gave_birth %in% "Yes" & HF_Code_based_on_sample %in% 
             HH_RF_Perc$HF_Code_based_on_sample[HH_RF_Perc$Indicator %in% "HHs in which women gave birth but did not visit the HF for the delivery"]) %>% 
    mutate(Indicator = "HHs in which women gave birth but did not visit the HF for the delivery") %>% 
    mutate(Question="Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401",
           Value=Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401,
           Question_2="Pregnancy_Related_Care_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Go_To_A_Health_Facility_For_Delivery",
           Value_2=Pregnancy_Related_Care_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Go_To_A_Health_Facility_For_Delivery,
           Question_3="Pregnancy_Related_Care_Why_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Not_Go_To_A_Health_Facility_For_Delivery",
           Value_3=Pregnancy_Related_Care_Why_Did_The_Household_Member_Who_Has_Given_Birth_Recently_Not_Go_To_A_Health_Facility_For_Delivery) %>% 
    select(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator, KEY, Key_Unique, starts_with("Question"), starts_with("Value")),
  # HHs in which women were pregnant but did not visit the HF for ANC
  indicator3 %>% 
    filter(HH_women_didnt_visit_for_ANC %in% "Yes" & HF_Code_based_on_sample %in% 
             HH_RF_Perc$HF_Code_based_on_sample[HH_RF_Perc$Indicator %in% "HHs in which women were pregnant but did not visit the HF for ANC"]) %>% 
    mutate(Indicator = "HHs in which women were pregnant but did not visit the HF for ANC") %>% 
    mutate(Question="Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401",
           Value=Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401,
           Question_2="ANC_PNC_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Go_To_Health_For_At_Least_One_Antenatal_Visit",
           Value_2=ANC_PNC_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Go_To_Health_For_At_Least_One_Antenatal_Visit,
           Question_3="ANC_PNC_Why_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit",
           Value_3=ANC_PNC_Why_Did_The_Household_Member_Who_Was_Recently_Pregnant_In_The_Past_6_Months_Not_Go_To_A_Health_Facility_For_An_Antenatal_Visit) %>% 
    select(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator, KEY, Key_Unique, starts_with("Question"), starts_with("Value")),
  # HHs in which women gave birth but did not visit the HF for PNC
  indicator3 %>% 
    filter(HH_women_didnt_visit_for_PNC %in% "Yes" & HF_Code_based_on_sample %in% 
             HH_RF_Perc$HF_Code_based_on_sample[HH_RF_Perc$Indicator %in% "HHs in which women gave birth but did not visit the HF for PNC"]) %>% 
    mutate(Indicator = "HHs in which women gave birth but did not visit the HF for PNC") %>% 
    mutate(Question="Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401",
           Value=Pregnancy_Related_Care_Have_You_Has_Someone_From_Your_Household_Been_Pregnant_And_Or_Given_Birth_In_The_Past_6_Months_Since_Dalwa_1401,
           Question_2="ANC_PNC_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Go_To_A_Health_For_At_Least_One_Postnatal_Visit",
           Value_2=ANC_PNC_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Go_To_A_Health_For_At_Least_One_Postnatal_Visit,
           Question_3="ANC_PNC_Why_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Not_Go_To_A_Health_Facility_For_A_Postnatal_Visit",
           Value_3=ANC_PNC_Why_Did_The_Household_Member_Who_Has_Given_Birth_In_The_Past_6_Months_Not_Go_To_A_Health_Facility_For_A_Postnatal_Visit) %>% 
    select(HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Indicator, KEY, Key_Unique, starts_with("Question"), starts_with("Value"))
)
HH_RF_report <- HH_RF_report %>% 
  left_join(t2_data %>% select(qa_status, KEY), by="KEY") %>% 
  select(HF_Code_based_on_sample:Key_Unique, QA_Status=qa_status,
         Question, Value, Question_2, Value_2, Question_3, Value_3, Question_4, Value_4, -KEY)

## Tests
# HH_RF_Perc %>% 
#   left_join(
#     HH_RF_report %>% group_by(HF_Code_based_on_sample, Indicator) %>% 
#       reframe(count_new = length(HF_Name_based_on_Sample))
#   ) %>% filter(Count != count_new)
# 
# HH_RF_Perc %>% 
#   janitor::get_dupes(HF_Code_based_on_sample, HF_Name_based_on_Sample, Indicator)


## Export ------------------------------------------------------------------------------------------
# HF Level
HF_red_flags_report <- list(
  Percentages=red_flag_perc,
  Report=red_flag_rep
)
# HH Level
HH_red_flags_report <- list(
  Percentages=HH_RF_Perc,
  Report=HH_RF_report
)

writexl::write_xlsx(HF_red_flags_report, paste0("output/Red Flags/HER_HF_Level_Red_Flags_", lubridate::today(),".xlsx"), format_headers = F)
writexl::write_xlsx(HH_red_flags_report, paste0("output/Red Flags/HER_HH_Level_Red_Flags_", lubridate::today(),".xlsx"))

## Remove extra objects ----------------------------------------------------------------------------
rm(red_flag_perc, red_flag_rep, HF_cols, visited_HF, not_visited_HF, prc_visited_hf, prc_delivery, 
   anc_visited_hf, indicator1, indicator2, indicator3, HH_RF_Perc, HH_RF_report, fallen_ill, injured)
