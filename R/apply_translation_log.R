# clean the translation log -----------------------------------------------------------------
tabs <- c("data", "Injuries", "Incidents", "Photos_Of_Handwashing_Stations", "Illness_Details", 
          "Fatalities", "Immunization_Details", "Income_Earning_Members_Details", "Section_B6_Other_Group")
## Filter empty rows
translation_log_filtered <- translation_log %>%
  mutate(Translation=case_when(
    !is.na(`Final Translation`) ~ `Final Translation`,
    TRUE ~ Translation
  ), old_value="") %>% 
  select(KEY, Tool, Tab_Name=`Tab Name`, question=Question, old_value, new_value=Translation)

# Identify issues
translation_log_filtered <- translation_log_filtered %>% 
  mutate(issue = case_when(
    is.na(Tool) & Tool %notin% c("1.1", "1.2", "1.3", "2", "3") ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    is.na(new_value) ~ "Translation is missing",
    Tool == "1.1" & Tab_Name == "data" & question %notin% names(hf_t1_data) ~ "question",
    Tool == "1.1" & Tab_Name == "data" & KEY %notin% hf_t1_data$KEY ~ "KEY",
    Tool == "1.2" & Tab_Name == "data" & question %notin% names(hf_t2_data) ~ "question",
    Tool == "1.2" & Tab_Name == "data" & KEY %notin% hf_t2_data$KEY ~ "KEY",
    Tool == "1.2" & Tab_Name == "Photos_Of_Handwashing_Stations" & question %notin% names(hf_t2_photos) ~ "question",
    Tool == "1.2" & Tab_Name == "Photos_Of_Handwashing_Stations" & KEY %notin% hf_t2_photos$KEY ~ "KEY",
    Tool == "1.3" & question %notin% names(hf_t3_data) ~ "question",
    Tool == "1.3" & KEY %notin% hf_t3_data$KEY ~ "KEY",
    Tool == "2" & Tab_Name == "data" & question %notin% names(t2_data) ~ "question",
    Tool == "2" & Tab_Name == "data" & KEY %notin% t2_data$KEY ~ "KEY",
    Tool == "2" & Tab_Name == "Income_Earning_Members_Details" & question %notin% names(t2_income) ~ "question",
    Tool == "2" & Tab_Name == "Income_Earning_Members_Details" & KEY %notin% t2_income$KEY ~ "KEY",
    Tool == "2" & Tab_Name == "Illness_Details" & question %notin% names(t2_illness) ~ "question",
    Tool == "2" & Tab_Name == "Illness_Details" & KEY %notin% t2_illness$KEY ~ "KEY",
    Tool == "2" & Tab_Name == "Injuries_Details" & question %notin% names(t2_injuries) ~ "question",
    Tool == "2" & Tab_Name == "Injuries_Details" & KEY %notin% t2_injuries$KEY ~ "KEY",
    Tool == "2" & Tab_Name == "Immunization_Details" & question %notin% names(t2_immunization) ~ "question",
    Tool == "2" & Tab_Name == "Immunization_Details" & KEY %notin% t2_immunization$KEY ~ "KEY",
    Tool == "2" & Tab_Name == "Section_B6_Other_Group" & question %notin% names(t2_other) ~ "question",
    Tool == "2" & Tab_Name == "Section_B6_Other_Group" & KEY %notin% t2_other$KEY ~ "KEY",
    Tool == "3" & question %notin% names(t3_data) ~ "question",
    Tool == "3" & KEY %notin% t3_data$KEY ~ "KEY"))

translation_log_filtered$duplicates <- duplicated(translation_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(translation_log_filtered[, c("KEY", "question")])

# Filter issues
translation_log_issues <- translation_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question)

# Join QA_Status with the issues
translation_log_issues <- translation_log_issues %>% 
  left_join(select(qa_log_sub, -Tool), by="KEY") %>% 
  filter(qa_status %in% c("Approved", "Excel Check Approved")) # Filtering issues of approved data only (QA)

translation_log_filtered <- translation_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue))

# apply the Translation log -------------------------------------------
## Tool 1.1
hf_t1_data_copy <- hf_t1_data
hf_t1_data <- apply_translation_log(data = hf_t1_data, log=filter(translation_log_filtered, Tool == "1.1" & Tab_Name == "data"),
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))
hf_injuries_copy <- hf_injuries
hf_injuries <- apply_translation_log(data = hf_injuries, log=filter(translation_log_filtered, Tool == "1.1" & Tab_Name == "Injuries"),
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))
hf_incidents_copy <- hf_incidents
hf_incidents <- apply_translation_log(data = hf_incidents, log=filter(translation_log_filtered, Tool == "1.1" & Tab_Name == "Incidents"),
                         data_KEY = "KEY",
                         log_columns = c(question = "question",
                                         old_value = "old_value",
                                         new_value = "new_value",
                                         KEY = "KEY"))

hf_fatalities_copy <- hf_fatalities
hf_fatalities <- apply_translation_log(data = hf_fatalities, log=filter(translation_log_filtered, Tool == "1.1" & Tab_Name == "Fatalities"),
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))
## Tool 1.2
hf_t2_data_copy <- hf_t2_data
hf_t2_data <- apply_translation_log(data = hf_t2_data, log=filter(translation_log_filtered, Tool == "1.2" & Tab_Name == "data"),
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))
hf_t2_photos_copy <- hf_t2_photos
hf_t2_photos <- apply_translation_log(data = hf_t2_photos, log=filter(translation_log_filtered, Tool == "1.2" & Tab_Name == "Photos_Of_Handwashing_Stations"),
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))
## Tool 1.3
hf_t3_data_copy <- hf_t3_data
hf_t3_data <- apply_translation_log(data = hf_t3_data, log=filter(translation_log_filtered, Tool == "1.3" & Tab_Name == "data"),
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))
## Tool 2
t2_data_copy <- t2_data
t2_data <- apply_translation_log(data = t2_data, log=filter(translation_log_filtered, Tool == "2" & Tab_Name == "data"),
                     data_KEY = "KEY",
                     log_columns = c(question = "question",
                                     old_value = "old_value",
                                     new_value = "new_value",
                                     KEY = "KEY"))
t2_income_copy <- t2_income
t2_income <- apply_translation_log(data = t2_income, log=filter(translation_log_filtered, Tool == "2" & Tab_Name == "Income_Earning_Members_Details"),
                       data_KEY = "KEY",
                       log_columns = c(question = "question",
                                       old_value = "old_value",
                                       new_value = "new_value",
                                       KEY = "KEY"))
t2_illness_copy <- t2_illness
t2_illness <- apply_translation_log(data = t2_illness, log=filter(translation_log_filtered, Tool == "2" & Tab_Name == "Illness_Details"),
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))
t2_injuries_copy <- t2_injuries
t2_injuries <- apply_translation_log(data = t2_injuries, log=filter(translation_log_filtered, Tool == "2" & Tab_Name == "Injuries_Details"),
                         data_KEY = "KEY",
                         log_columns = c(question = "question",
                                         old_value = "old_value",
                                         new_value = "new_value",
                                         KEY = "KEY"))
t2_immunization_copy <- t2_immunization
t2_immunization <- apply_translation_log(data = t2_immunization, log=filter(translation_log_filtered, Tool == "2" & Tab_Name == "Immunization_Details"),
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))
t2_other_copy <- t2_other
t2_other <- apply_translation_log(data = t2_other, log=filter(translation_log_filtered, Tool == "2" & Tab_Name == "Section_B6_Other_Group"),
                      data_KEY = "KEY",
                      log_columns = c(question = "question",
                                      old_value = "old_value",
                                      new_value = "new_value",
                                      KEY = "KEY"))
## Tool 3
t3_data_copy <- t3_data
t3_data <- apply_translation_log(data = t3_data, log=filter(translation_log_filtered, Tool == "3" & Tab_Name == "data"),
                     data_KEY = "KEY",
                     log_columns = c(question = "question",
                                     old_value = "old_value",
                                     new_value = "new_value",
                                     KEY = "KEY"))

# Verify Translation log -------------------------------------------
message("Verifying Correction log, please wait!")
translation_log_discrep <- rbind(
  ## Tool 1.1
  compare_dt(df1 = hf_t1_data_copy, df2 = hf_t1_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 1.1")
  ,
  compare_dt(df1 = hf_injuries_copy, df2 = hf_injuries,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Injuries")
  ,
  compare_dt(df1 = hf_incidents_copy, df2 = hf_incidents,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Incidents")
  ,
  compare_dt(df1 = hf_fatalities_copy, df2 = hf_fatalities,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Fatalities")
  ,
  ## Tool 1.2
  compare_dt(df1 = hf_t2_data_copy, df2 = hf_t2_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 1.2"),
  ## Tool 1.3
  compare_dt(df1 = hf_t3_data_copy, df2 = hf_t3_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 1.3"),
  ## Tool 2
  compare_dt(df1 = t2_data_copy, df2 = t2_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 2"),
  compare_dt(df1 = t2_income_copy, df2 = t2_income,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 2"),
  compare_dt(df1 = t2_illness_copy, df2 = t2_illness,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 2"),
  compare_dt(df1 = t2_injuries_copy, df2 = t2_injuries,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 2"),
  compare_dt(df1 = t2_immunization_copy, df2 = t2_immunization,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 2"),
  compare_dt(df1 = t2_other_copy, df2 = t2_other,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 2"),
  ## Tool 3
  compare_dt(df1 = t3_data_copy, df2 = t3_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Tool 3")
  ) 

# Removing extra spaces from new_value before joining 
translation_log_discrep <- translation_log_discrep %>%
  anti_join(translation_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(hf_t1_data_copy, hf_injuries_copy, hf_incidents_copy, hf_fatalities_copy, hf_t2_data_copy,
   hf_t3_data_copy, t2_data_copy, t3_data_copy, translation_log_filtered, tabs, t2_income_copy,
   t2_illness_copy, t2_injuries_copy, t2_immunization_copy, t2_other_copy)



