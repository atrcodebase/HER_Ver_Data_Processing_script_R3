### Extra columns
extra_cols <- read_excel("input/extra_columns.xlsx", sheet="R3")
extra_cols %>% count(Tool, Sheet)

# Temp
extra_cols <- extra_cols %>% filter(Reason_for_removal %notin% "PII") %>% unique()
extra_cols %>% count(Reason_for_removal)

## Remove Extra columns ----------------------------------------------------------------------------
## HF Level Data Verification
for(sheet in names(HF_data_approved)){
  # Remove
  HF_data_approved[[sheet]] <- HF_data_approved[[sheet]] %>% 
    select(-all_of(extra_cols$questions[extra_cols$Tool %in% "HF_Verification" & extra_cols$Sheet %in% sheet]))
}
## QoC
qoc_data_approved$data <- qoc_data_approved$data %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "QoC" & extra_cols$Sheet %in% "data"]))
qoc_data_approved$Health_Worker_Interview_Ques... <- qoc_data_approved$Health_Worker_Interview_Ques... %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "QoC" & extra_cols$Sheet %in% "Health_Worker"]))

## QQC
for(sheet in names(qqc_data_approved)){
  # Remove
  qqc_data_approved[[sheet]] <- qqc_data_approved[[sheet]] %>% 
    select(-all_of(extra_cols$questions[extra_cols$Tool %in% "QQC" & extra_cols$Sheet %in% sheet]))
}
## HMIS Service assessment
for(sheet in names(hmis_data_approved)){
  # Remove
  hmis_data_approved[[sheet]] <- hmis_data_approved[[sheet]] %>% 
    select(-any_of(extra_cols$questions[extra_cols$Tool %in% "HMIS" & extra_cols$Sheet %in% sheet]))
}

## SP Personnel 
sp_data_approved$data <- sp_data_approved$data %>% 
  select(-any_of(extra_cols$questions[extra_cols$Tool %in% "SP_Personnel" & extra_cols$Sheet %in% "data"]))
sp_data_approved$Personnel <- sp_data_approved$Personnel %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "SP_Personnel" & extra_cols$Sheet %in% "Personnel"]))
sp_data_approved$Absent_Days <- sp_data_approved$Absent_Days %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "SP_Personnel" & extra_cols$Sheet %in% "Absent_Days"]))


## Vignette 
vignette_data_approved <- vignette_data_approved %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "Vignette" & extra_cols$Sheet %in% "data"]))

## Patient Verification 
patient_data_approved <- patient_data_approved %>% 
  select(-all_of(extra_cols$questions[extra_cols$Tool %in% "Patient_Verification" & extra_cols$Sheet %in% "data"]))


# remove extra objects -----------------------------------------------------------------------------
rm(extra_cols)
