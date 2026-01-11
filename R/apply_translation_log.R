# clean the translation log -----------------------------------------------------------------
tool_names <- c("Data Verification", "HMIS", "QQC", "QoC", "SP", "Patient", "Vignette")
tabs <- c(names(HF_data), names(qoc_data), names(qqc_data), names(hmis_data), names(sp_data))


## Filter empty rows
translation_log_filtered <- translation_log %>%
  filter(!(is.na(KEY_Unique) & is.na(Question) & is.na(Old_value))) %>%
  # mutate(Translation=case_when(
  #   !is.na(`Final Translation`) ~ `Final Translation`,
  #   TRUE ~ Translation
  # ), old_value="") %>% 
  mutate(
    # Question=case_when(
    #   Check_Type %in% "audio" ~ paste0(Question,"_Translation"),
    #   Check_Type %in% "image" ~ paste0(Question, "_QA"), 
    #   TRUE ~ Question),
    # old_value="",
    new_value=case_when(
      !is.na(Reviewed) ~ Reviewed, # Translations are corrected in here
      TRUE ~ new_value
    ),
    Tool = case_when(
      Tool %in% "HF_Verification" ~ "Data Verification",
      Tool %in% "QQC" ~ "QQC",
      Tool %in% c("QoC", "QoC") ~ "QoC",
      Tool %in% "SP_Attendance" ~ "SP",
      Tool %in% c("Patient", "Patient_Verification") ~ "Patient",
      Tool %in% c("HER_Rev_Service_Assessment_Sampling_Verification_R3", "Service Assessment & Sampling Verification Tool") ~ "HMIS",
      TRUE ~ Tool
    )
  ) %>% 
  select(key=KEY, KEY=KEY_Unique, Tool, Tab_Name, question=Question, old_value=Old_value, new_value)

# Identify issues (Across Tools)
translation_log_filtered <- translation_log_filtered %>% 
  mutate(issue = case_when(
    is.na(Tool) | Tool %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name"))

translation_log_filtered <- check_log(translation_log_filtered, HF_data, "Data Verification", data_KEY="KEY_Unique")
translation_log_filtered <- check_log(translation_log_filtered, qoc_data, "QoC")
translation_log_filtered <- check_log(translation_log_filtered, qqc_data, "QQC")
translation_log_filtered <- check_log(translation_log_filtered, hmis_data, "HMIS", data_KEY="KEY_Unique")
translation_log_filtered <- check_log(translation_log_filtered, sp_data, "SP")
translation_log_filtered <- check_log(translation_log_filtered, vignette_data, "Vignette")
translation_log_filtered <- check_log(translation_log_filtered, patient_data, "Patient")


translation_log_filtered$duplicates <- duplicated(translation_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(translation_log_filtered[, c("KEY", "question")])

# Filter issues
translation_log_issues <- translation_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question) %>% 
  left_join(
    select(qa_log, QA_Status, key=KEY_Unique)
  ) %>% 
  filter(QA_Status %notin% "Rejected")

translation_log_filtered <- translation_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue))


# apply the Translation log ------------------------------------------------------------------------
## HF Data Verification 
HF_data_copy <- HF_data

for(sheet in names(HF_data)){
  # Apply Log
  HF_data[[sheet]] <- apply_log(data = HF_data[[sheet]], 
                                log=filter(translation_log_filtered, Tool == "Data Verification" & Tab_Name == sheet),
                                data_KEY = "KEY_Unique",
                                log_columns = c(question = "question",
                                                old_value = "old_value",
                                                new_value = "new_value",
                                                KEY = "KEY"))
}

## QoC
qoc_data_copy <- qoc_data

qoc_data$data <- apply_log(data = qoc_data$data, log=filter(translation_log_filtered, Tool == "QoC" & Tab_Name == "data"),
                           data_KEY = "KEY",
                           log_columns = c(question = "question",
                                           old_value = "old_value",
                                           new_value = "new_value",
                                           KEY = "KEY"))
qoc_data$Health_Worker_Interview_Ques... <- apply_log(data = qoc_data$Health_Worker_Interview_Ques..., 
                                                      log=filter(translation_log_filtered, Tool == "QoC" & Tab_Name == "Health_Worker_Interview_Ques..."),
                                                      data_KEY = "KEY",
                                                      log_columns = c(question = "question",
                                                                      old_value = "old_value",
                                                                      new_value = "new_value",
                                                                      KEY = "KEY"))
## QQC
qqc_data_copy <- qqc_data

for(sheet in names(qqc_data)){
  log_sub <- filter(translation_log_filtered, Tool %in% "QQC" & Tab_Name %in% sheet)
  if(nrow(log_sub)>0){
    # Apply log
    qqc_data[[sheet]] <- apply_log(data = qqc_data[[sheet]], 
                                   log=filter(translation_log_filtered, Tool %in% "QQC" & Tab_Name %in% sheet),
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
                                  log=filter(translation_log_filtered, Tool == "HMIS" & Tab_Name == sheet),
                                  data_KEY = "KEY_Unique",
                                  log_columns = c(question = "question",
                                                  old_value = "old_value",
                                                  new_value = "new_value",
                                                  KEY = "KEY"))
}
## SP Personnel 
sp_data_copy <- sp_data
sp_data$data <- apply_log(data = sp_data$data, log=filter(translation_log_filtered, Tool == "SP" & Tab_Name == "data"),
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))
sp_data$Personnel <- apply_log(data = sp_data$Personnel, log=filter(translation_log_filtered, Tool == "SP" & Tab_Name == "Personnel"),
                               data_KEY = "KEY",
                               log_columns = c(question = "question",
                                               old_value = "old_value",
                                               new_value = "new_value",
                                               KEY = "KEY"))
sp_data$Absent_Days <- apply_log(data = sp_data$Absent_Days, log=filter(translation_log_filtered, Tool == "SP" & Tab_Name == "Absent_Days"),
                                 data_KEY = "KEY",
                                 log_columns = c(question = "question",
                                                 old_value = "old_value",
                                                 new_value = "new_value",
                                                 KEY = "KEY"))

## Vignette 
vignette_data_copy <- vignette_data
vignette_data <- apply_log(data = vignette_data, log=filter(translation_log_filtered, Tool == "Vignette" & Tab_Name == "data"),
                           data_KEY = "KEY",
                           log_columns = c(question = "question",
                                           old_value = "old_value",
                                           new_value = "new_value",
                                           KEY = "KEY"))
## Patient Verification 
patient_data_copy <- patient_data
patient_data <- apply_log(data = patient_data, log=filter(translation_log_filtered, Tool == "Patient" & Tab_Name == "data"),
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))

# Verify Translation log -------------------------------------------
message("Verifying Translation log, please wait!")
translation_log_discrep <- rbind(
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
sheets_sub <- names(HF_data)[names(HF_data) %in% translation_log_filtered$Tab_Name]
for(sheet in sheets_sub){
  # Compare
  translation_log_discrep <- rbind(
    translation_log_discrep, 
    compare_dt(df1 = HF_data_copy[[sheet]], df2 = HF_data[[sheet]],
               unique_id_df1 = "KEY_Unique", unique_id_df2 = "KEY_Unique") %>%
      mutate(`Tool Type` = paste0("HF_", sheet))
  )
}
## QQC
sheets_sub <- names(qqc_data)[names(qqc_data) %in% translation_log_filtered$Tab_Name]
for(sheet in sheets_sub){
  # Compare
  translation_log_discrep <- rbind(
    translation_log_discrep, 
    compare_dt(df1 = qqc_data_copy[[sheet]], df2 = qqc_data[[sheet]],
               unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
      mutate(`Tool Type` = paste0("QQC_", sheet))
  )
}
## HMIS Service assessment
sheets_sub <- names(hmis_data)[names(hmis_data) %in% translation_log_filtered$Tab_Name]
for(sheet in sheets_sub){
  # Compare
  translation_log_discrep <- rbind(
    translation_log_discrep, 
    compare_dt(df1 = hmis_data_copy[[sheet]], df2 = hmis_data[[sheet]],
               unique_id_df1 = "KEY_Unique", unique_id_df2 = "KEY_Unique") %>%
      mutate(`Tool Type` = paste0("HMIS_", sheet))
  )
}
# Removing extra spaces from new_value before joining 
translation_log_discrep <- translation_log_discrep %>%
  anti_join(translation_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(tabs, tool_names, translation_log_filtered, HF_data_copy, qoc_data_copy, qqc_data_copy, 
   hmis_data_copy, sp_data_copy, vignette_data_copy, patient_data_copy, sheets_sub)


