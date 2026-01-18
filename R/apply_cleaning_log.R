# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
tabs <- c(names(HF_data), names(qoc_data), names(qqc_data), names(hmis_data), names(sp_data)) # Patient & Vignette only 1 sheet
sm_variables <- read_excel("input/select_multiple_questions.xlsx") %>% pull(questions) 
tool_names <- c("Data Verification", "HMIS", "QQC", "QoC", "SP", "Patient", "Vignette", 
                "Red Flags Verification Tool" # Data not processed yet
                )

## Filter empty rows
correction_log_filtered <- correction_log %>%
  filter(!(is.na(KEY_Unique) & is.na(Question) & is.na(old_value))) %>%
  mutate(New_Value = case_when(
    Question %in% sm_variables ~ str_replace_all(New_Value, "-|,|  | - ", " ") %>% str_squish(),
    TRUE ~ str_squish(New_Value)
  ), Log_type = "Correction_Log"
  # KEY= case_when(
  #   is.na(KEY) & !is.na(`Full_ KEY`) ~ str_squish(`Full_ KEY`),
  #   TRUE ~ str_squish(KEY)
  # )
  ) %>% 
  select(key=KEY, KEY=KEY_Unique, Tool, Tab_Name, question=Question, old_value, 
         new_value=New_Value, QAed_by=`Logged by:`, Remarks, Log_type)

# Different Image QA columns 
image_diff_cols <- list("photo_of_service-related_document"="Services_data_Photo_QA", 
     "Doortag1_Photo"="Doortag_Photo_QA",
     # HF Verification
     "Area_Free_Moisture_Photo"="Area_Free_Moisture_QA",
     "Medicine_Stored_Systematic_Photo"="Medicine_Stored_Systematic_QA",
     "Old_Storage_available_Photo"="Old_Storage_available_QA",
     "Temperature_Chart_Photo"="Temperature_Chart_QA",
     "Sunlight_cannot_enter_Photo"="Sunlight_cannot_enter_QA"
     ) %>% unlist()

# New Log: should be applied before correction log
detailed_check_log <- detailed_check %>% 
  filter(Question %notin% c("Signature_Of_Respondent")) %>% # Image questions with no QA column
  mutate(Question=case_when(
    Check_Type %in% "audio" ~ paste0(Question,"_Translation"),
    # Image
    Check_Type %in% "image" & Question %in% names(image_diff_cols) ~ image_diff_cols[Question],
    Check_Type %in% "image" & Question %notin% names(image_diff_cols) ~ paste0(Question, "_QA"), 
    TRUE ~ as.character(Question)
  ),
  New_Value=case_when(
    Check_Status %in% "Verified" & Check_Type %in% c("audio", "image") ~ "Verified",
    Check_Status %in% "Error/Irrelevant" & Check_Type %in% c("audio", "image") ~ "Error/Irrelevant",
    TRUE ~ as.character(New_Value)
  ), Log_type = "Detailed_Check") %>% 
  filter(Check_Type %notin% "audio audit") %>% # Audio Audit is only used for QA)
  filter(Check_Status %in% "Error/Irrelevant" | Check_Type %in% c("audio", "image")) %>% 
  select(key=KEY, KEY=KEY_Unique, Tab_Name, Check_Type, QAed_by=`QA'ed By`, 
         question=Question, old_value=Value, new_value=New_Value, Check_Status, Tool, Log_type)

# Merge logs
correction_log_filtered <- plyr::rbind.fill(
  detailed_check_log, # should be applied first
  correction_log_filtered
)

## Update Tool names across both logs 
correction_log_filtered <- correction_log_filtered %>% 
  mutate(Tool = case_when(
    Tool %in% "HER_Re_HF_Level_Data_Verification_Tool_R3" ~ "Data Verification",
    Tool %in% "HER_Re_QQC_R3" ~ "QQC",
    Tool %in% c("HER_Re_QoC_Interview_with_Health_Workers_R3", "Interview with Health Workers (QoC)") ~ "QoC",
    Tool %in% c("HER_Re_SP_Data_Verification_Tool_R3", "SP Personnel Attendance Check") ~ "SP",
    Tool %in% c("HER_Re_Patient_Verification_R3", "HER Re Patient Verification") ~ "Patient",
    Tool %in% c("HER_Rev_Service_Assessment_Sampling_Verification_R3", "Service Assessment & Sampling Verification Tool") ~ "HMIS",
    TRUE ~ Tool
  ),
  # Fixing it in here because QA log messes up the tab_name
  question = case_when(
    question %in% "q2_3_7_photos_Count" ~ "q2_3_7_Photos_Count",
    question %in% "HF_Name_based_on_sample" ~ "HF_Name_based_on_Sample",
    TRUE ~ question
  ),
  Tab_Name = case_when(
    question %in% c("q2_3_7_photos_Count", "q2_3_7_Photos_Count") ~ "data",
    question %in% c("HF_Name_based_on_sample", "HF_Name_based_on_Sample") ~ "data",
    TRUE ~ Tab_Name
  )
  ) #%>% 
  # Temporary Filter until I check this in detail
  # filter(question %notin% "Signature_Of_Respondent")
correction_log_filtered %>% count(Tool)

# Identify issues (Across Tools)
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    is.na(Tool) | Tool %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name"))

# correct_log=correction_log_filtered; data=HF_data; Log_Tool_name="Data Verification"; data_KEY="KEY"
correction_log_filtered <- check_log(correction_log_filtered, HF_data, "Data Verification")
correction_log_filtered <- check_log(correction_log_filtered, qoc_data, "QoC")
correction_log_filtered <- check_log(correction_log_filtered, qqc_data, "QQC")
correction_log_filtered <- check_log(correction_log_filtered, hmis_data, "HMIS")
correction_log_filtered <- check_log(correction_log_filtered, sp_data, "SP")
correction_log_filtered <- check_log(correction_log_filtered, vignette_data, "Vignette")
correction_log_filtered <- check_log(correction_log_filtered, patient_data, "Patient")

# Check duplicates
correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("KEY", "question", "Log_type")], fromLast = T) | duplicated(correction_log_filtered[, c("KEY", "question", "Log_type")])

# Filter issues
correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question) %>% 
  left_join(
    select(qa_log, QA_Status, key=KEY_Unique)
  )
correction_log_filtered <- correction_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue)) 

#### Convert audio translation columns to character
## HF Data Verification 
for(sheet in names(HF_data)){
  HF_data[[sheet]] <- convert_to_char(data=HF_data[[sheet]], tool_path=hf_tool_path, image_suffix="QA", audio_suffix="Translation")
}
## QoC
qoc_data$data <- convert_to_char(data=qoc_data$data, tool_path=qoc_tool_path, image_suffix="QA", audio_suffix="translation")
qoc_data$Health_Worker_Interview_Ques... <- convert_to_char(data=qoc_data$Health_Worker_Interview_Ques..., tool_path=qoc_tool_path, image_suffix="QA", audio_suffix="translation")
## QQC
for(sheet in names(qqc_data)){
  qqc_data[[sheet]] <- convert_to_char(data=qqc_data[[sheet]], tool_path=qqc_tool_path, image_suffix="QA", audio_suffix="Translation")
}
## HMIS Service assessment
for(sheet in names(hmis_data)){
  hmis_data[[sheet]] <- convert_to_char(data=hmis_data[[sheet]], tool_path=hmis_tool_path, image_suffix="QA", audio_suffix="Translation")
}
## SP Personnel 
sp_data$data <- convert_to_char(data=sp_data$data, tool_path=sp_tool_path, image_suffix="QA", audio_suffix="translation")
sp_data$Personnel <- convert_to_char(data=sp_data$Personnel, tool_path=sp_tool_path, image_suffix="QA", audio_suffix="translation")
sp_data$Absent_Days <- convert_to_char(data=sp_data$Absent_Days, tool_path=sp_tool_path, image_suffix="QA", audio_suffix="translation")
## Vignette 
vignette_data <- convert_to_char(data=vignette_data, tool_path=vignette_tool_path, image_suffix="QA", audio_suffix="translation")
## Patient Verification 
patient_data <- convert_to_char(data=patient_data, tool_path=patient_tool_path, image_suffix="QA", audio_suffix="translation")
 

# apply the correction-log -------------------------------------------------------------------------
## HF Data Verification 
HF_data_copy <- HF_data

for(sheet in names(HF_data)){
  # Apply Log
  HF_data[[sheet]] <- apply_log(data = HF_data[[sheet]], 
                                log=filter(correction_log_filtered, Tool %in% "Data Verification" & Tab_Name %in% sheet),
                                data_KEY = "KEY",
                                log_columns = c(question = "question",
                                                old_value = "old_value",
                                                new_value = "new_value",
                                                KEY = "KEY"))
}

## QoC
qoc_data_copy <- qoc_data

qoc_data$data <- apply_log(data = qoc_data$data, log=filter(correction_log_filtered, Tool %in% "QoC" & Tab_Name %in% "data"),
                      data_KEY = "KEY",
                      log_columns = c(question = "question",
                                      old_value = "old_value",
                                      new_value = "new_value",
                                      KEY = "KEY"))
qoc_data$Health_Worker_Interview_Ques... <- apply_log(data = qoc_data$Health_Worker_Interview_Ques...,
                                                      log=filter(correction_log_filtered, Tool %in% "QoC" & Tab_Name %in% "Health_Worker_Interview_Ques..."),
                                                      data_KEY = "KEY",
                                                      log_columns = c(question = "question",
                                                                      old_value = "old_value",
                                                                      new_value = "new_value",
                                                                      KEY = "KEY"))
## QQC
qqc_data_copy <- qqc_data

for(sheet in names(qqc_data)){
  log_sub <- filter(correction_log_filtered, Tool %in% "QQC" & Tab_Name %in% sheet)
  if(nrow(log_sub)>0){
    # Apply log
    qqc_data[[sheet]] <- apply_log(data = qqc_data[[sheet]],
                                   log=filter(correction_log_filtered, Tool == "QQC" & Tab_Name == sheet),
                                   data_KEY = "KEY",
                                   log_columns = c(question = "question",
                                                   old_value = "old_value",
                                                   new_value = "new_value",
                                                   KEY = "KEY"))
  } else {
    next
  }
}

## HMIS Service assessment
hmis_data_copy <- hmis_data

for(sheet in names(hmis_data)){
  # Apply log
  hmis_data[[sheet]] <- apply_log(data = hmis_data[[sheet]], 
                                 log=filter(correction_log_filtered, Tool %in% "HMIS" & Tab_Name %in% sheet),
                                 data_KEY = "KEY",
                                 log_columns = c(question = "question",
                                                 old_value = "old_value",
                                                 new_value = "new_value",
                                                 KEY = "KEY"))
}
## SP Personnel 
sp_data_copy <- sp_data
sp_data$data <- apply_log(data = sp_data$data, log=filter(correction_log_filtered, Tool %in% "SP" & Tab_Name %in% "data"),
                     data_KEY = "KEY",
                     log_columns = c(question = "question",
                                     old_value = "old_value",
                                     new_value = "new_value",
                                     KEY = "KEY"))
sp_data$Personnel <- apply_log(data = sp_data$Personnel, log=filter(correction_log_filtered, Tool %in% "SP" & Tab_Name %in% "Personnel"),
                               data_KEY = "KEY",
                               log_columns = c(question = "question",
                                               old_value = "old_value",
                                               new_value = "new_value",
                                               KEY = "KEY"))
sp_data$Absent_Days <- apply_log(data = sp_data$Absent_Days, log=filter(correction_log_filtered, Tool %in% "SP" & Tab_Name %in% "Absent_Days"),
                       data_KEY = "KEY",
                       log_columns = c(question = "question",
                                       old_value = "old_value",
                                       new_value = "new_value",
                                       KEY = "KEY"))

## Vignette 
vignette_data_copy <- vignette_data
vignette_data <- apply_log(data = vignette_data, log=filter(correction_log_filtered, Tool %in% "Vignette" & Tab_Name %in% "data"),
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))
## Patient Verification 
patient_data_copy <- patient_data
patient_data <- apply_log(data = patient_data, log=filter(correction_log_filtered, Tool %in% "Patient" & Tab_Name %in% "data"),
                           data_KEY = "KEY",
                           log_columns = c(question = "question",
                                           old_value = "old_value",
                                           new_value = "new_value",
                                           KEY = "KEY"))

# Verify correction log -------------------------------------------
message("Verifying Correction log, please wait!")
correction_log_discrep <- rbind(
  ## QoC - Interview with Health Workers
  compare_dt(df1 = qoc_data_copy$data, df2 = qoc_data$data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "QoC"),
  compare_dt(df1 = qoc_data_copy$Health_Worker_Interview_Ques..., df2 = qoc_data$Health_Worker_Interview_Ques...,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "QoC_Interviews"),
  ## SP Personal Attendance Check
  compare_dt(df1 = sp_data_copy$data, df2 = sp_data$data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "SP_data"),
  compare_dt(df1 = sp_data_copy$Personnel, df2 = sp_data$Personnel,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "SP_Personnel"),
  compare_dt(df1 = sp_data_copy$Absent_Days, df2 = sp_data$Absent_Days,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "SP_Absent"),
  ## Vignette 
  compare_dt(df1 = vignette_data_copy, df2 = vignette_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Vignette"),
  ## Patient Verification 
  compare_dt(df1 = patient_data_copy, df2 = patient_data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Patient")
)
## HF Data Verification 
for(sheet in names(HF_data)){
  # Compare
  correction_log_discrep <- rbind(
    correction_log_discrep, 
    compare_dt(df1 = HF_data_copy[[sheet]], df2 = HF_data[[sheet]],
               unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
      mutate(`Tool Type` = paste0("HF_", sheet))
  )
}
## QQC
for(sheet in names(qqc_data)){
  # Compare
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(df1 = qqc_data_copy[[sheet]], df2 = qqc_data[[sheet]],
               unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
      mutate(`Tool Type` = paste0("QQC_", sheet))
  )
}
## HMIS Service assessment
for(sheet in names(hmis_data)){
  # Compare
  correction_log_discrep <- rbind(
    correction_log_discrep, 
    compare_dt(df1 = hmis_data_copy[[sheet]], df2 = hmis_data[[sheet]],
               unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
      mutate(`Tool Type` = paste0("HMIS_", sheet))
  )
}

# Removing extra spaces from new_value before joining
correction_log_discrep <- correction_log_discrep %>%
  anti_join(correction_log_filtered %>%
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
# rm(# All copy data created
#    list=ls()[grepl("_copy", ls())])
rm(sm_variables, tabs, tool_names, correction_log_filtered, HF_data_copy, qoc_data_copy, qqc_data_copy, 
   hmis_data_copy, sp_data_copy, vignette_data_copy, patient_data_copy, log_sub)
