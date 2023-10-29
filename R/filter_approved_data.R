### Filter Approved data for client
## Tool 1.1 ----------------------------------------------------------------------------------------
hf_t1_data_wide <- hf_t1_data_wide %>% filter(qa_status %in% approved_qa_status)

## Tool 1.2 ----------------------------------------------------------------------------------------
hf_t2_data_wide <- hf_t2_data_wide %>% filter(qa_status %in% approved_qa_status)

## Tool 1.3 ----------------------------------------------------------------------------------------
hf_t3_data_filtered <- hf_t3_data %>% 
  filter(qa_status %in% approved_qa_status) 

## Tool 2 ------------------------------------------------------------------------------------------
t2_data_filtered <- t2_data %>% 
  filter(qa_status %in% approved_qa_status)
t2_income_filtered <- t2_income %>%
  filter(PARENT_KEY %in% t2_data_filtered$KEY)
t2_illness_filtered <- t2_illness %>% 
  filter(PARENT_KEY %in% t2_data_filtered$KEY)
t2_injuries_filtered <- t2_injuries %>% 
  filter(PARENT_KEY %in% t2_data_filtered$KEY)
t2_immunization_filtered <- t2_immunization %>% 
  filter(PARENT_KEY %in% t2_data_filtered$KEY)
t2_other_filtered <- t2_other %>% 
  filter(PARENT_KEY %in% t2_data_filtered$KEY)

## Tool 3 ------------------------------------------------------------------------------------------
t3_data_filtered <- t3_data %>% 
  filter(qa_status %in% approved_qa_status)

## Tool 4 ------------------------------------------------------------------------------------------
# infra_data_filtered <- infra_data %>% filter(qa_status %in% approved_qa_status)
# infra_rep_filtered <- infra_rep %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_doc_filtered <- infra_doc %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_env_filtered <- infra_env %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_feat_filtered <- infra_feat %>%
#   filter(PARENT_KEY %in% infra_data$KEY)
# infra_elem_filtered <- infra_elem %>%
#   filter(PARENT_KEY %in% infra_feat$KEY) # Parent is Features

## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)

