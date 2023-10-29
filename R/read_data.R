### Read Data 
guess_max <- 5000000
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA

# Tool 1.1 Health Facility Level
t1.1_path <- "input/raw_data/HER ESS Tool 1.1 Health Facility Level.xlsx"
hf_t1_data <- read_excel(t1.1_path, sheet = "data", guess_max = guess_max, na = convert_to_na)
hf_injuries <- read_excel(t1.1_path, sheet = "Injuries_Details", guess_max = guess_max, na = convert_to_na)
hf_fatalities <- read_excel(t1.1_path, sheet = "Fatalities_Details", guess_max = guess_max, na = convert_to_na)
hf_incidents <- read_excel(t1.1_path, sheet = "Incidents", guess_max = guess_max, na = convert_to_na)

# Tool 1.2 Health Facility Level
t1.2_path <- "input/raw_data/HER ESS Tool 1.2 Health Facility Level.xlsx"
hf_t2_data <- read_excel(t1.2_path, sheet = "data", guess_max = guess_max, na = convert_to_na)
hf_t2_photos <- read_excel(t1.2_path, sheet = "Photos_Of_Handwashing_Stations", guess_max = guess_max, na = convert_to_na)

# Tool 1.3 Health Facility Level
t1.3_path <- "input/raw_data/HER ESS Tool 1.3 Nutrition Counsellor Interview Tool.xlsx"
hf_t3_data <- read_excel(t1.3_path, sheet = "data", guess_max = guess_max, na = convert_to_na)

# Tool 2 Household Level Survey
t2_path <- "input/raw_data/HER ESS Tool 2 Household Level Surveys.xlsx"
t2_data <- read_excel(t2_path, sheet = "data", guess_max = guess_max, na = convert_to_na)
t2_income <- read_excel(t2_path, sheet = "Income_Earning_Members_Details", guess_max = guess_max, na = convert_to_na)
t2_illness <- read_excel(t2_path, sheet = "Illness_Details", guess_max = guess_max, na = convert_to_na)
t2_injuries <- read_excel(t2_path, sheet = "Injuries_Details", guess_max = guess_max, na = convert_to_na)
t2_immunization <- read_excel(t2_path, sheet = "Immunization_Details", guess_max = guess_max, na = convert_to_na)
t2_other <- read_excel(t2_path, sheet = "Section_B6_Other_Group", guess_max = guess_max, na = convert_to_na)

# Tool 3 Community Level Survey
t3_path <- "input/raw_data/HER ESS Tool 3 Community Actors Survey Tool.xlsx"
t3_data <- read_excel(t3_path, sheet = "data", guess_max = guess_max, na = convert_to_na)

# # Tool 4 Infrastructure
# t4_path <- "input/raw_data/HER_HF_INFRASTRUCTURE_V01.xlsx"
# infra_data <- read_excel(t4_path, sheet = "data", guess_max = guess_max, na = convert_to_na)
# infra_rep <- read_excel(t4_path, sheet = "Representatives", guess_max = guess_max, na = convert_to_na)
# infra_doc <- read_excel(t4_path, sheet = "Documents", guess_max = guess_max, na = convert_to_na)
# infra_env <- read_excel(t4_path, sheet = "Environmental_And_Social_Sta...", guess_max = guess_max, na = convert_to_na)
# infra_feat <- read_excel(t4_path, sheet = "Features", guess_max = guess_max, na = convert_to_na)
# infra_elem <- read_excel(t4_path, sheet = "Elements", guess_max = guess_max, na = convert_to_na)
# Extra missing Infra questions
# infra_checklist <- read_excel("input/raw_data/HER Health Facility Infrastructure Checklist.xlsx", guess_max = guess_max, na = convert_to_na)

# Remove Extra Objects -----------------------------------------------------------------------------
rm(t1.1_path, t1.2_path, convert_to_na, t1.3_path, t2_path, t3_path, guess_max)
