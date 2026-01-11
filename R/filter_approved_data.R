### Filter Approved data for client
HF_data_approved <- HF_data
qoc_data_approved <- qoc_data
qqc_data_approved <- qqc_data
hmis_data_approved <- hmis_data
sp_data_approved <- sp_data
vignette_data_approved <- vignette_data
patient_data_approved <- patient_data

## HF Level Data Verification ----------------------------------------------------------------------
HF_data_approved$data <- HF_data_approved$data %>% filter(qa_status %in% approved_qa_status)

# filter sheets
for(sheet in names(HF_data_approved)[names(HF_data_approved) != "data"]){
  # filter
  HF_data_approved[[sheet]] <- HF_data_approved[[sheet]] %>%
    filter(KEY %in% HF_data_approved$data[["KEY"]]) # New Key
}
# # Filter specific columns
# HF_data_filtered$Drug_Out_Of_Stock <- HF_data_filtered$Drug_Out_Of_Stock %>%
#   filter(Was_Drug_Out_Of_Stock_RP %in% "Yes") # Filtered this based on QA request
# HF_data_filtered$Consumable_Out_Of_Stock_RP <- HF_data_filtered$Consumable_Out_Of_Stock_RP %>%
#   filter(Was_Consumable_Out_Of_Stock_RP %in% "Yes") # Filtered this based on QA request

## QoC - Interview with Health Workers -------------------------------------------------------------
qoc_data_approved$data <- qoc_data_approved$data %>% filter(qa_status %in% approved_qa_status)
qoc_data_approved$Health_Worker_Interview_Ques... <- qoc_data_approved$Health_Worker_Interview_Ques... %>% filter(PARENT_KEY %in% qoc_data_approved$data$KEY)

## QQC ---------------------------------------------------------------------------------------------
qqc_data_approved$data <- qqc_data_approved$data %>% filter(qa_status %in% approved_qa_status)

# filter sheets
for(sheet in names(qqc_data_approved)[names(qqc_data_approved) != "data"]){
  # filter
  qqc_data_approved[[sheet]] <- qqc_data_approved[[sheet]] %>%
    filter(PARENT_KEY %in% qqc_data_approved$data[["KEY"]])
}

## HMIS Service assessment -------------------------------------------------------------------------
hmis_data_approved$data <- hmis_data_approved$data %>% filter(qa_status %in% approved_qa_status)

# filter sheets
for(sheet in names(hmis_data_approved)[names(hmis_data_approved) != "data"]){
  # filter
  hmis_data_approved[[sheet]] <- hmis_data_approved[[sheet]] %>%
    filter(KEY %in% hmis_data_approved$data[["KEY"]]) # New Key
}


## SP Personal Attendance Check --------------------------------------------------------------------
sp_data_approved$data <- sp_data_approved$data %>% filter(qa_status %in% approved_qa_status)

sp_data_approved$Personnel <- sp_data_approved$Personnel %>% filter(PARENT_KEY %in% sp_data_approved$data$KEY)
sp_data_approved$Absent_Days <- sp_data_approved$Absent_Days %>% filter(PARENT_KEY %in% sp_data_approved$data$KEY)


## Vignette ----------------------------------------------------------------------------------------
vignette_data <- vignette_data %>% filter(qa_status %in% approved_qa_status)

## Patient Verification ----------------------------------------------------------------------------
patient_data <- patient_data %>% filter(qa_status %in% approved_qa_status)


## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)
