# Update Select_multiple series columns ------------------------------------------------------------
### QoC
qoc_data <- update_series_cols(data=qoc_data,
                                 tool_path = qoc_tool_path,
                                 question_separator="_")
qoc_interview <- update_series_cols(data=qoc_interview,
                               tool_path = qoc_tool_path,
                               question_separator="_")
# Check if updated correctly
qoc_SM_issues <- rbind(
  check_select_multiple(data=qoc_data,
                        tool_path = qoc_tool_path,
                        question_separator="_"),
  check_select_multiple(data=qoc_interview,
                        tool_path = qoc_tool_path,
                        question_separator="_")
) 

### QQC
qqc_data$data <- update_series_cols(data=qqc_data$data,
                               tool_path = qqc_tool_path,
                               question_separator="_",
                               excluded_col="q2_3")
# Manually update these columns 
qqc_data$data <- update_series_cols_1(data=qqc_data$data,
                     question = "q2_3",
                     series_cols = c("q2_3.1", "q2_3.2", "q2_3.3", "q2_3.4", "q2_3.0"),
                     question_separator=".") 
# Check if updated correctly
qqc_SM_issues <- check_select_multiple(data=qqc_data$data,
                                  tool_path = qqc_tool_path,
                                  question_separator="_")


### HF Level Data Verification 
HF_data$Staff_Absentees <- update_series_cols(data=HF_data$Staff_Absentees,
                               tool_path = hf_tool_path,
                               question_separator="_")
HF_data$Lab <- update_series_cols(data=HF_data$Lab,
                             tool_path = hf_tool_path,
                             question_separator="_")
# Check if updated correctly
HF_SM_issues <- rbind(
  check_select_multiple(data=HF_data$Staff_Absentees,
                        tool_path = hf_tool_path,
                        question_separator="_"),
  check_select_multiple(data=HF_data$Lab,
                        tool_path = hf_tool_path,
                        question_separator="_")
)

### SP Personal Attendance Check
sp_data <- update_series_cols(data=sp_data,
                                   tool_path = sp_tool_path,
                                   question_separator="_")
sp_absent <- update_series_cols(data=sp_absent,
                                  tool_path = sp_tool_path,
                                  question_separator="_")
sp_personnel <- update_series_cols(data=sp_personnel,
                                          tool_path = sp_tool_path,
                                          question_separator="_")
# Check if updated correctly
sp_SM_issues <- rbind(
  check_select_multiple(data=sp_data,
                        tool_path = sp_tool_path,
                        question_separator="_"),
  check_select_multiple(data=sp_absent,
                        tool_path = sp_tool_path,
                        question_separator="_"),
  check_select_multiple(data=sp_personnel,
                        tool_path = sp_tool_path,
                        question_separator="_")
)
## Export List -------------------------------------------------------------------------------------
## Select Multiple issues
SM_issues <- plyr::rbind.fill(
  qoc_SM_issues %>% mutate(Tool="QoC"),
  qqc_SM_issues %>% mutate(Tool="QQC"),
  HF_SM_issues %>% mutate(Tool="HF_level_data"),
  # service_SM_issues %>% mutate(Tool="Service_Assessment"),
  sp_SM_issues %>% mutate(Tool="SP_Personnel")
) %>% filter(!is.na(KEY))

# remove extra objects -----------------------------------------------------------------------------
rm(qoc_SM_issues, qqc_SM_issues, HF_SM_issues, sp_SM_issues)
