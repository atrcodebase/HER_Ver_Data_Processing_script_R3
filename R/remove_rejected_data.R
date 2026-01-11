### Remove Rejected QA status and keys -------------------------------------------------------------


## HF Level Data Verification ----------------------------------------------------------------------
HF_data$data <- HF_data$data %>% 
  mutate(KEY_Unique=KEY) %>% 
  filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)
# filter sheets
for(sheet in names(HF_data)[names(HF_data) != "data"]){ 
  # filter
  HF_data[[sheet]] <- HF_data[[sheet]] %>% 
    mutate(KEY_Unique=KEY, KEY=str_split_fixed(PARENT_KEY, "/", 2)[,1]) %>% # New KEY
    filter(KEY %in% HF_data$data$KEY_Unique) %>%
    filter(KEY_Unique %notin% rejection_log$KEY_Unique)
  
}

## QoC - Interview with Health Workers -------------------------------------------------------------
qoc_data$data <- qoc_data$data %>% filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)
qoc_data$Health_Worker_Interview_Ques... <- qoc_data$Health_Worker_Interview_Ques... %>% 
  filter(PARENT_KEY %in% qoc_data$data$KEY & KEY %notin% rejection_log$KEY_Unique)

## QQC ---------------------------------------------------------------------------------------------
qqc_data$data <- qqc_data$data %>% 
  filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)
# Update URL links in all sheets
for(sheet in names(qqc_data)[names(qqc_data) %notin% "data"]){
  # Update Link
  qqc_data[[sheet]] <- qqc_data[[sheet]] %>% 
    filter(PARENT_KEY %in% qqc_data$data[["KEY"]] & KEY %notin% rejection_log$KEY_Unique) # All PARENT_KEYs are from main sheet
}

## HMIS Service assessment -------------------------------------------------------------------------
hmis_data$data <- hmis_data$data %>% 
  mutate(KEY_Unique=KEY) %>% 
  filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)
# filter sheets
for(sheet in names(hmis_data)[names(hmis_data) != "data"]){ 
  # filter
  hmis_data[[sheet]] <- hmis_data[[sheet]] %>% 
    mutate(KEY_Unique=KEY, KEY=str_split_fixed(PARENT_KEY, "/", 2)[,1]) %>% # New KEY
    filter(KEY %in% hmis_data$data$KEY_Unique) %>%
    filter(KEY_Unique %notin% rejection_log$KEY_Unique)
  
}

## SP Personal Attendance Check --------------------------------------------------------------------
sp_data$data <- sp_data$data %>% filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)

sp_data$Personnel <- sp_data$Personnel %>% 
  filter(PARENT_KEY %in% sp_data$data$KEY & KEY %notin% rejection_log$KEY_Unique)
sp_data$Absent_Days <- sp_data$Absent_Days %>% 
  filter(PARENT_KEY %in% sp_data$data$KEY & KEY %notin% rejection_log$KEY_Unique)

## Vignette ----------------------------------------------------------------------------------------
vignette_data <- vignette_data %>% filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)

## Patient Verification ----------------------------------------------------------------------------
patient_data <- patient_data %>% filter(qa_status %notin% rejected_qa_status & KEY %notin% rejection_log$KEY_Unique)


## Remove extra objects ----------------------------------------------------------------------------
rm(rejected_qa_status)

