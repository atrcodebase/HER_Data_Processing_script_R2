### Extra columns
extra_cols <- read_excel("input/extra_columns.xlsx")
extra_cols %>% count(Tool, Sheet)

## Remove Extra columns ----------------------------------------------------------------------------
# hf_t1_data_wide <- hf_t1_data_wide %>% select(-all_of(t1.1_extra_cols))
# hf_t2_data_wide <- hf_t2_data_wide %>% select(-all_of(t1.2_extra_cols))

# Tool 1.1
hf_t1_data_filtered <- hf_t1_data_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.1" & extra_cols$Sheet %in% "data"]))

hf_injuries_filtered <- hf_injuries_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.1" & extra_cols$Sheet %in% "T1_Injuries"]))
hf_fatalities_filtered <- hf_fatalities_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.1" & extra_cols$Sheet %in% "Fatalities"]))
hf_incidents_filtered <- hf_incidents_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.1" & extra_cols$Sheet %in% "Incidents"]))

# Tool 1.2
hf_t2_data_filtered <- hf_t2_data_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.2" & extra_cols$Sheet %in% "data"]))

hf_t2_photos_filtered <- hf_t2_photos_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.2" & extra_cols$Sheet %in% "T1.2_photo"]))

# Tool 1.3
hf_t3_data_filtered <- hf_t3_data_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "1.3" & extra_cols$Sheet %in% "data"]))

# Tool 2
t2_data_filtered <- t2_data_filtered %>%
  select(-any_of(extra_cols$questions[extra_cols$Tool %in% "2" & extra_cols$Sheet %in% "data"]))
t2_income_filtered <- t2_income_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "2" & extra_cols$Sheet %in% "T2_Income"]))
t2_illness_filtered <- t2_illness_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "2" & extra_cols$Sheet %in% "T2_Illness"])) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_injuries_filtered <- t2_injuries_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "2" & extra_cols$Sheet %in% "T2_Injuries"])) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_immunization_filtered <- t2_immunization_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "2" & extra_cols$Sheet %in% "T2_Immunization"])) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
t2_other_filtered <- t2_other_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "2" & extra_cols$Sheet %in% "T2_Other"])) %>% 
  mutate(KEY_Unique = KEY,
         KEY=PARENT_KEY)
  
# Tool 3
t3_data_filtered <- t3_data_filtered %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "3" & extra_cols$Sheet %in% "data"])) 

# remove extra objects -----------------------------------------------------------------------------
rm(
  # t1.1_extra_cols, t1.2_extra_cols, t1.1_caption_cols, t1.2_caption_cols, t4_extra_cols, 
  t1.3_extra_cols, t2_extra_cols, t3_extra_cols)

