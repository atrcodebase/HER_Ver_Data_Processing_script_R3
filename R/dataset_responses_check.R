## Check for any values in the dataset that cannot be found in the tool ---------------------------- 
# ## HF Level Data Verification
# hf_response_log <- data.frame() 
# for(sheet in names(HF_data_approved)){
#   # Log
#   hf_response_log = rbind(
#     hf_response_log,
#     check_responses(data=HF_data_approved[[sheet]], tool_path=hf_tool_path, sheet=sheet, print = FALSE)
#   )
# }


# ## QoC - Interview with Health Workers
# qoc_response_log <- rbind(
#   check_responses(data=qoc_data_approved$data, tool_path=qoc_tool_path, sheet="data"),
#   check_responses(data=qoc_data_approved$Health_Worker_Interview_Ques..., tool_path=qoc_tool_path, sheet="Health_Worker_Interview_Ques...")
#   ) 
# 
# ## QQC (no select one/multiple in QQC repeat sheets)
# qqc_response_log <- check_responses(data=qqc_data_approved$data, tool_path=qqc_tool_path, sheet="data") 

## HMIS Service assessment
hmis_response_log <- data.frame() 
for(sheet in names(hmis_data_approved)){
  # Log
  hmis_response_log = rbind(
    hmis_response_log,
    check_responses(data=hmis_data_approved[[sheet]], tool_path=hmis_tool_path, sheet=sheet)
  )
}

## SP Personal Attendance Check
sp_response_log <- rbind(
  check_responses(data=sp_data_approved$data, tool_path=sp_tool_path, sheet="data"),
  check_responses(data=sp_data_approved$Personnel, tool_path=sp_tool_path, sheet="Personnel"),
  check_responses(data=sp_data_approved$Absent_Days, tool_path=sp_tool_path, sheet="Absent_Days")
) 


## Vignette
vign_response_log <- check_responses(data=vignette_data_approved, tool_path=vignette_tool_path, sheet="data")

## Patient Verification
patient_response_log <- check_responses(data=patient_data_approved, tool_path=patient_tool_path, sheet="data")




response_log_list <- plyr::rbind.fill(
  # hf_response_log %>% mutate(Tool="HF_level_Verification"),
  # qoc_response_log %>% mutate(Tool="QoC"),
  # qqc_response_log %>% mutate(Tool="QQC"),
  hmis_response_log %>% mutate(Tool="HMIS"),
  sp_response_log %>% mutate(Tool="SP_Attendance"),
  vign_response_log %>% mutate(Tool="Vignette"),
  patient_response_log %>% mutate(Tool="Patient_Verification")
) 

# remove extra objects -------------------------------------------
rm() 
