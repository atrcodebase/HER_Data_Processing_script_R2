### Remove Rejected QA status and keys -------------------------------------------------------------
rejected_qa_status <- "Rejected"

## Tool 1.1 ----------------------------------------------------------------------------------------
hf_t1_data <- hf_t1_data %>% 
  filter(KEY %notin% rejection_log$KEY_Unique) %>% 
  filter(qa_status %notin% rejected_qa_status)
hf_injuries <- hf_injuries %>%
  filter(PARENT_KEY %in% hf_t1_data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)
hf_fatalities <- hf_fatalities %>%
  filter(PARENT_KEY %in% hf_t1_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)
hf_incidents <- hf_incidents %>%
  filter(PARENT_KEY %in% hf_t1_data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)

## Tool 1.2 ----------------------------------------------------------------------------------------
hf_t2_data <- hf_t2_data %>% filter(qa_status %notin% rejected_qa_status)
hf_t2_photos <- hf_t2_photos %>%
  filter(PARENT_KEY %in% hf_t2_data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)

## Tool 1.3 ----------------------------------------------------------------------------------------
hf_t3_data <- hf_t3_data %>% 
  filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)
 
## Tool 2 ------------------------------------------------------------------------------------------
t2_data <- t2_data %>% 
  filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)
t2_income <- t2_income %>%
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)
t2_illness <- t2_illness %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)
t2_injuries <- t2_injuries %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)
t2_immunization <- t2_immunization %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)
t2_other <- t2_other %>% 
  filter(PARENT_KEY %in% t2_data$KEY) %>% 
  filter(KEY %notin% rejection_log$KEY_Unique)

## Tool 3 ------------------------------------------------------------------------------------------
t3_data <- t3_data %>% 
  filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)

# ## Tool 4 ------------------------------------------------------------------------------------------
# infra_data <- infra_data %>%
#   filter(review_status %notin% rejected_qa_status)
# 
# infra_rep <- infra_rep %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_doc <- infra_doc %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_env <- infra_env %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_feat <- infra_feat %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_elem <- infra_elem %>%
#   filter(PARENT_KEY %in% infra_feat$KEY) # Parent is Features

## Remove extra objects ----------------------------------------------------------------------------
rm(rejected_qa_status)

