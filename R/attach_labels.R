# apply the value labels ---------------------------------------------------------------------------
## Tool 1.1
t1.1_tool <- "input/tools/HER+ESS+Tool+1.1_+Health+Facility+Level.xlsx"
hf_t1_data <- labeler(data = hf_t1_data,
                        tool = t1.1_tool,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";")
hf_injuries <- labeler(data = hf_injuries,
                      tool = t1.1_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")
hf_fatalities <- labeler(data = hf_fatalities,
                      tool = t1.1_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")
hf_incidents <- labeler(data = hf_incidents,
                      tool = t1.1_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")

## Tool 1.2
t1.2_tool <- "input/tools/HER+ESS+Tool+1.2_+Health+Facility+Level.xlsx"
hf_t2_data <- labeler(data = hf_t2_data,
                      tool = t1.2_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")
hf_t2_photos <- labeler(data = hf_t2_photos,
                      tool = t1.2_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")

## Tool 1.3
t1.3_tool <- "input/tools/+HER+ESS+Tool+1.3_+Nutrition+Counsellor+Interview+Tool.xlsx"
hf_t3_data <- labeler(data = hf_t3_data,
                      tool = t1.3_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")

## Tool 2
t2_tool <- "input/tools/HER+ESS+Tool+2_+Household+Level+Surveys.xlsx"
t2_data <- labeler(data = t2_data,
                      tool = t2_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")
t2_income <- labeler(data = t2_income,
                   tool = t2_tool,
                   survey_label = "label",
                   choice_lable = "label",
                   multi_response_sep = ";")
t2_illness <- labeler(data = t2_illness,
                   tool = t2_tool,
                   survey_label = "label",
                   choice_lable = "label",
                   multi_response_sep = ";")
t2_injuries <- labeler(data = t2_injuries,
                   tool = t2_tool,
                   survey_label = "label",
                   choice_lable = "label",
                   multi_response_sep = ";")
t2_immunization <- labeler(data = t2_immunization,
                   tool = t2_tool,
                   survey_label = "label",
                   choice_lable = "label",
                   multi_response_sep = ";")
t2_other <- labeler(data = t2_other,
                   tool = t2_tool,
                   survey_label = "label",
                   choice_lable = "label",
                   multi_response_sep = ";")

## Tool 3
t3_tool <- "input/tools/HER+ESS+Tool+3_+Community+Actors+Survey+Tool.xlsx"
t3_data <- labeler(data = t3_data,
                      tool = t3_tool,
                      survey_label = "label",
                      choice_lable = "label",
                      multi_response_sep = ";")

# ## Tool 4
# t4_tool <- "input/tools/HER_HF_Combined_Infra_V01+(1)+(6).xlsx"
# infra_data <- labeler(data = infra_data,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")
# infra_rep <- labeler(data = infra_rep,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")
# infra_doc <- labeler(data = infra_doc,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")
# infra_env <- labeler(data = infra_env,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")
# infra_feat <- labeler(data = infra_feat,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")
# infra_elem <- labeler(data = infra_elem,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")
# infra_checklist <- labeler(data = infra_checklist,
#                       tool = t4_tool,
#                       survey_label = "label",
#                       choice_lable = "label",
#                       multi_response_sep = ";")

# remove extra objects -------------------------------------------
rm(t1.1_tool, t1.2_tool, t1.3_tool, t2_tool, t3_tool) #t4_tool

