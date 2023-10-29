## Tool 1.1
hf_injuries <- hf_injuries %>%
  filter(PARENT_KEY %in% hf_t1_data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
hf_fatalities <- hf_fatalities %>%
  filter(PARENT_KEY %in% hf_t1_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)
hf_incidents <- hf_incidents %>%
  filter(PARENT_KEY %in% hf_t1_data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)

## Tool 1.2
hf_t2_photos <- hf_t2_photos %>%
  filter(PARENT_KEY %in% hf_t2_data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)

## Tool 2
# Subset 
t2_data_sub <- t2_data %>% 
  select(Site_Visit_ID,	Province,	District,	Village,	Interviewee_Respondent_Type,	Survey_Number, KEY)

# Filter & Join
t2_income <- t2_income %>%
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:Survey_Number, .before = 1)
t2_illness <- t2_illness %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:Survey_Number, .before = 1)
t2_injuries <- t2_injuries %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:Survey_Number, .before = 1)
t2_immunization <- t2_immunization %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:Survey_Number, .before = 1)
t2_other <- t2_other %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  left_join(t2_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:Survey_Number, .before = 1)

## Tool 4
infra_rep <- infra_rep %>%
  filter(PARENT_KEY %in% infra_data$KEY)
infra_doc <- infra_doc %>%
  filter(PARENT_KEY %in% infra_data$KEY)
infra_env <- infra_env %>%
  filter(PARENT_KEY %in% infra_data$KEY)
infra_feat <- infra_feat %>%
  filter(PARENT_KEY %in% infra_data$KEY)
infra_elem <- infra_elem %>%
  filter(PARENT_KEY %in% infra_feat$KEY) # Parent is Features
