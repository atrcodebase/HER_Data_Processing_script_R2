## Check for any values in the dataset that cannot be found in the tool ---------------------------- 
## Tool 1.1
t1.1_tool <- "input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level.xlsx"
t1.1_response_log <- rbind(
  check_responses(data=hf_t1_data, tool_path=t1.1_tool, sheet="data"),
  check_responses(data=hf_injuries, tool_path=t1.1_tool, sheet="Injuries"),
  check_responses(data=hf_fatalities, tool_path=t1.1_tool, sheet="Fatalities"),
  check_responses(data=hf_incidents, tool_path=t1.1_tool, sheet="Incidents")
)

## Tool 1.2
t1.2_tool <- "input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level.xlsx"
t1.2_response_log <- rbind(
  check_responses(data=hf_t2_data, tool_path=t1.2_tool, sheet="data"),
  check_responses(data=hf_t2_photos, tool_path=t1.2_tool, sheet="Photos")
)


## Tool 1.3
t1.3_tool <- "input/tools/+HER+ESS+Tool+1.3_+Nutrition+Counsellor+Interview+Tool.xlsx"
t1.3_response_log <- check_responses(data=hf_t3_data_filtered, tool_path=t1.3_tool, sheet="data")

## Tool 2
t2_tool <- "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx"
t2_response_log <- rbind(
  check_responses(data=t2_data_filtered, tool_path=t2_tool, sheet="data", excluded_cols = c("Selected_HH_Member", "PC_Selected_HH_Member", "Selected_Member_Inj")),
  check_responses(data=t2_income_filtered, tool_path=t2_tool, sheet="Income"),
  check_responses(data=t2_illness_filtered, tool_path=t2_tool, sheet="Illness"),
  check_responses(data=t2_injuries_filtered, tool_path=t2_tool, sheet="Injuries"),
  check_responses(data=t2_immunization_filtered, tool_path=t2_tool, sheet="Immunization"),
  check_responses(data=t2_other_filtered, tool_path=t2_tool, sheet="Other")) 
# t2_response_log <- t2_response_log %>% filter(!(question %in% "Reason_for_noconsent_of_Female_Member_Interview" & value %in% "IEA did not allow female interviews to be conducted."))
## Tool 3
t3_tool <- "input/tools/HER+ESS+Tool+3_+Community+Actors+Survey+Tool.xlsx"
t3_response_log <- check_responses(data=t3_data_filtered, tool_path=t3_tool, sheet="data")

# Export List
response_log_list <- list(
  "Tool 1.1"=t1.1_response_log,
  "Tool 1.2"=t1.2_response_log,
  "Tool 1.3"=t1.3_response_log,
  "Tool 2"=t2_response_log,
  "Tool 3"=t3_response_log
)

