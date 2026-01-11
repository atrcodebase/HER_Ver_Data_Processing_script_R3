# apply the value labels ---------------------------------------------------------------------------
## HF Data Verification 
for(sheet in names(HF_data)){
  # Apply label
  HF_data[[sheet]] <- labeler(data = HF_data[[sheet]],
                              tool = hf_tool_path,
                              survey_label = "label",
                              choice_lable = "label",
                              multi_response_sep = ";",
                              excluded_cols = "HF_Type") # New Update
}

## QoC
qoc_data$data <- labeler(data = qoc_data$data,
                    tool = qoc_tool_path,
                    survey_label = "label",
                    choice_lable = "label",
                    multi_response_sep = ";")
qoc_data$Health_Worker_Interview_Ques... <- labeler(data = qoc_data$Health_Worker_Interview_Ques...,
                         tool = qoc_tool_path,
                         survey_label = "label",
                         choice_lable = "label",
                         multi_response_sep = ";")
## QQC
qqc_data$data <- labeler(data = qqc_data$data,
                         tool = qqc_tool_path,
                         survey_label = "label",
                         choice_lable = "label",
                         multi_response_sep = ";",
                         excluded_cols = "HF_Type") # New Update

## HMIS Service assessment
for(sheet in names(hmis_data)){
  # Apply label
  hmis_data[[sheet]] <- labeler(data = hmis_data[[sheet]],
                              tool = hmis_tool_path,
                              survey_label = "label",
                              choice_lable = "label",
                              multi_response_sep = ";",
                              excluded_cols = "HF_Type") # New Update
}

## SP Personnel 
for(sheet in names(sp_data)){
  # Apply label
  sp_data[[sheet]] <- labeler(data = sp_data[[sheet]],
                                tool = sp_tool_path,
                                survey_label = "label",
                                choice_lable = "label",
                                multi_response_sep = ";")
}

## Vignette 
vignette_data <- labeler(data = vignette_data,
                         tool = vignette_tool_path,
                         survey_label = "label",
                         choice_lable = "label",
                         multi_response_sep = ";")
## Patient Verification 
patient_data <- labeler(data = patient_data,
                        tool = patient_tool_path,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";",
                        excluded_cols = "HF_Type") # New Update

# remove extra objects -------------------------------------------
rm() 

