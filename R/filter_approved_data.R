### Filter Approved data for client
## Tool 1.1 ----------------------------------------------------------------------------------------
hf_t1_data_wide <- hf_t1_data_wide %>% filter(qa_status %in% approved_qa_status)
# Created the long version based on QA request
hf_t1_data_filtered <- hf_t1_data %>% 
  filter(qa_status %in% approved_qa_status)

hf_injuries_filtered <- hf_injuries %>%
  filter(PARENT_KEY %in% hf_t1_data_filtered$KEY)
hf_fatalities_filtered <- hf_fatalities %>%
  filter(PARENT_KEY %in% hf_t1_data_filtered$KEY)
hf_incidents_filtered <- hf_incidents %>%
  filter(PARENT_KEY %in% hf_t1_data_filtered$KEY)

## Tool 1.2 ----------------------------------------------------------------------------------------
hf_t2_data_wide <- hf_t2_data_wide %>% filter(qa_status %in% approved_qa_status)
# Created the long version based on QA request
hf_t2_data_filtered <- hf_t2_data %>% filter(qa_status %in% approved_qa_status)

hf_t2_photos_filtered <- hf_t2_photos %>% 
  filter(PARENT_KEY %in% hf_t2_data_filtered$KEY)
  
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

## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)

