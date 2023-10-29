### Tool 1.1 ---------------------------------------------------------------------------------------
## DOuble check these, instead of index_inju, ..., use the count per grpup of parent keys
# Reshape 
hf_injuries_wide <- hf_injuries %>% 
  select(-c(`SET-OF-Injuries_Details`, KEY)) %>% 
  pivot_wider(names_from = "index_injury",
              values_from = c("When_Did_This_Injury_Occur", "What_Caused_The_Injury", 
                              "What_Caused_The_Injury_Translation"),
              names_vary = "slowest")
  
hf_fatalities_wide <- hf_fatalities %>% 
  select(-c(`SET-OF-Fatalities_Details`, KEY)) %>% 
  pivot_wider(names_from = "index_fatality",
              values_from = c("When_Did_This_Fatality_Occur",	"What_Caused_The_Fatality",
                              "What_Caused_The_Fatality_Translation"),
              names_vary = "slowest") 
hf_incidents_wide <- hf_incidents %>% 
  select(-c(`SET-OF-Incidents`, KEY)) %>% 
  pivot_wider(names_from = "index_incidents",
              values_from = c("Kind_of_Incident",	"Can_You_Please_Elaborate_On_The_Safety_Security_Incident_S",
                              "Can_You_Please_Elaborate_On_The_Safety_Security_Incident_S_Translation"),
              names_vary = "slowest") 

# Join with main sheet
hf_t1_data_wide <- hf_t1_data %>% 
  left_join(hf_injuries_wide, by=c("KEY"="PARENT_KEY")) %>% 
  relocate(names(hf_injuries_wide)[names(hf_injuries_wide) != "PARENT_KEY"], .after = Injuries_Details_count) # Relocate new columns

hf_t1_data_wide <- hf_t1_data_wide %>% 
  left_join(hf_fatalities_wide, by=c("KEY"="PARENT_KEY")) %>% 
  relocate(names(hf_fatalities_wide)[names(hf_fatalities_wide) != "PARENT_KEY"], .after = Fatalities_Details_count) # Relocate new columns

hf_t1_data_wide <- hf_t1_data_wide %>% 
  left_join(hf_incidents_wide, by=c("KEY"="PARENT_KEY")) %>% 
  relocate(names(hf_incidents_wide)[names(hf_incidents_wide) != "PARENT_KEY"], .after = Incidents_count) # Relocate new columns

### Tool 1.2 ---------------------------------------------------------------------------------------
# Reshape
hf_t2_photos_wide <- hf_t2_photos %>% 
  group_by(PARENT_KEY) %>% 
  mutate(index_photos = row_number()) %>% ungroup() %>% 
  select(-c(Please_Take_Photo_Of_The_Handwashing_Area_Caption, 
            `SET-OF-Photos_Of_Handwashing_Stations`, KEY)) %>% 
  pivot_wider(names_from = "index_photos",
              values_from = c("Please_Take_Photo_Of_The_Handwashing_Area",
                              "Please_Take_Photo_Of_The_Handwashing_Area_QA"),
              names_vary = "slowest") 

# Join with main sheet
hf_t2_data_wide <- hf_t2_data %>% 
  left_join(hf_t2_photos_wide, by=c("KEY"="PARENT_KEY")) %>% 
  relocate(starts_with("Please_Take_Photo_Of_The_Handwashing_Area"), .after = Photos_Of_Handwashing_Stations_count) # Relocate new columns

# remove extra objects -----------------------------------------------------------------------------
rm(hf_injuries_wide, hf_fatalities_wide, hf_incidents_wide, hf_t2_photos_wide)

