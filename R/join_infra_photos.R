### Join missing photos in infra dataset -----------------------------------------------------------

# Checks
infra_checklist %>% 
  janitor::get_dupes("Site_Visit_ID")
infra_checklist %>% 
  filter(Site_Visit_ID %notin% infra_data$Site_Visit_ID)
infra_data %>% 
  filter(Site_Visit_ID %in% infra_checklist$Site_Visit_ID) %>% 
  janitor::get_dupes("Site_Visit_ID")

## Filter photo cols
checklist_cols <- infra_checklist %>% select(ends_with("_Photo")) %>% names()
infra_data_sub <- infra_data %>% 
  filter(Site_Visit_ID %in% infra_checklist$Site_Visit_ID)

#test
infra_data_copy <- infra_data

# Loops through each question and joins them with the dataset
photo_log <- data.frame()
for(main_col_ph in checklist_cols){
  main_col <- str_remove(main_col_ph, "_Photo\\b")
  
  # Join new photo column with the dataset
  joined_data <- left_join(
    infra_data_sub %>% select(Site_Visit_ID, infra_value=main_col, infra_photo=main_col_ph),
    infra_checklist %>% select(Site_Visit_ID, checklist_value=main_col, checklist_photo=main_col_ph),
    by="Site_Visit_ID"
  ) %>% mutate(Question_name = main_col, .after=Site_Visit_ID)
  
  # Log cases where main question response is different
  photo_log <- rbind(
    photo_log,
    joined_data %>% filter(infra_value != checklist_value)
  )
  
  # # Cases where response is the same but infra already has photo (ignored)
  # joined_data %>% filter(infra_value == checklist_value & !is.na(infra_photo))
  
  # Cases where response is the same but infra is missing photo (used for merging)
  joined_data <- joined_data %>% 
    filter((infra_value == checklist_value & is.na(infra_photo))) %>% 
    select(Site_Visit_ID, checklist_photo)
  
  # Join
  infra_data <- infra_data %>% 
    left_join(joined_data, by="Site_Visit_ID") %>% 
    mutate(!!main_col_ph := case_when(
      is.na(get(main_col_ph)) & !is.na(checklist_photo) ~ checklist_photo,
      TRUE ~ get(main_col_ph)
    )) %>% select(-checklist_photo)
}

# Check if everything applied correctly
correction_log_discrep <-   compare_dt(df1 = infra_data_copy, df2 = infra_data,
                                       unique_id_df1 = "KEY", unique_id_df2 = "KEY")
correction_log_discrep %>% filter(question %notin% checklist_cols)

infra_data %>% filter(KEY %in% correction_log_discrep$KEY) %>% filter(Site_Visit_ID %notin% infra_checklist$Site_Visit_ID)

rm(infra_data_sub, checklist_cols, correction_log_discrep, infra_data_copy, joined_data)
# # Tests --------------------------------------------------------------------------------------------
# infra_groups <- read_excel("input/Infra_question_groups.xlsx", sheet = "HER_INFRA_Updated")
# infra_tool <- read_excel("input/tools/HER_HF_Combined_Infra_V01+(1)+(6).xlsx", sheet = "survey")
# 
# # Filter sec 2 only
# infra_tool <- infra_tool %>% slice(166:328) %>% pull(name)
# infra_groups <- infra_groups %>% filter(section == "section2") %>% pull(name)
# 
# # Checks
# names(infra_checklist)[names(infra_checklist) %notin% infra_tool]
# names(infra_checklist)[names(infra_checklist) %notin% infra_groups]
# names(infra_checklist)[names(infra_checklist) %notin% names(infra_data)]
