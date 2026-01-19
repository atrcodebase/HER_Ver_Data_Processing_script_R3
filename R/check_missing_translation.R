# Log missing audio translation and missing image QA -----------------------------------------------
## HF Data Verification 
# HF_tool <- read_excel(hf_tool_path, "survey", guess_max = 100000)
# # Different Image QA columns 
# hf_diff_cols <- list("enumerator_photo_signboard"="enumerator_photo_signboard_Photo_QA",
#      "Area_Free_Moisture_Photo"="Area_Free_Moisture_QA",
#      "Medicine_Stored_Systematic_Photo"="Medicine_Stored_Systematic_QA",
#      "Old_Storage_available_Photo"="Old_Storage_available_QA",
#      "Temperature_Chart_Photo"="Temperature_Chart_QA",
#      "Sunlight_cannot_enter_Photo"="Sunlight_cannot_enter_QA"
# )
# 
# HF_missing_log <- data.frame()
# # Loop through all the sheets
# for(sheet in names(HF_data_approved)){
#   HF_audio_cols <- HF_tool %>% filter(type %in% c("audio") & name %in% names(HF_data_approved[[sheet]])) %>% pull(name) 
#   HF_image_cols <- HF_tool %>% filter(type %in% c("image") & name %in% names(HF_data_approved[[sheet]])) %>% pull(name)
#   
#   # Log
#   HF_missing_log <-  rbind(
#     HF_missing_log,
#     # Translation
#     log_questions(data=HF_data_approved[[sheet]], columns=HF_audio_cols, key_col="KEY_Unique", suffix="Translation", sheet=sheet),
#     # Image QA
#     log_questions(data=HF_data_approved[[sheet]], columns=HF_image_cols, key_col="KEY_Unique", suffix="QA", 
#                   sheet=sheet, columns_different = hf_diff_cols)
#   )
# }
# 
# 
# ## QoC
# qoc_tool <- read_excel(qoc_tool_path, "survey", guess_max = 100000)
# qoc_audio_cols <- qoc_tool %>% filter(type %in% c("audio")) %>% pull(name) # No Audio cols
# qoc_image_cols <- qoc_tool %>% filter(type %in% c("image")) %>% pull(name)
# 
# QoC_missing_log <- rbind(
#   # Translation
#   log_questions(data=qoc_data_approved$data,
#                 columns=qoc_audio_cols[qoc_audio_cols %in% names(qoc_data_approved$data)],
#                 suffix="Translation", sheet="data"),
#   log_questions(data=qoc_data_approved$Health_Worker_Interview_Ques...,
#                 columns=qoc_audio_cols[qoc_audio_cols %in% names(qoc_data_approved$Health_Worker_Interview_Ques...)],
#                 suffix="Translation", sheet="Health_Worker_Interview_Ques..."),
#   # Image QA
#   log_questions(data=qoc_data_approved$data,
#                 columns=qoc_image_cols[qoc_image_cols %in% names(qoc_data_approved$data)],
#                 suffix="QA", sheet="data"),
#   log_questions(data=qoc_data_approved$Health_Worker_Interview_Ques...,
#                 columns=qoc_image_cols[qoc_image_cols %in% names(qoc_data_approved$Health_Worker_Interview_Ques...)],
#                 suffix="QA", sheet="Health_Worker_Interview_Ques...")
# )
# 
# ## QQC
# qqc_tool <- read_excel(qqc_tool_path, "survey", guess_max = 100000)
# qqc_missing_log <- data.frame()
# # Loop through all the sheets
# for(sheet in names(qqc_data_approved)){
#   qqc_image_cols <- qqc_tool %>% filter(type %in% c("image") & name %in% names(qqc_data_approved[[sheet]])) %>% pull(name)
#   qqc_audio_cols <- qqc_tool %>% filter(type %in% c("audio") & name %in% names(qqc_data_approved[[sheet]])) %>% pull(name)
#   # Log
#   qqc_missing_log <-  rbind(
#     qqc_missing_log,
#     # Translation
#     log_questions(data=qqc_data_approved[[sheet]], columns=qqc_audio_cols, suffix="Translation", sheet=sheet),
#     # Image QA
#     log_questions(data=qqc_data_approved[[sheet]], columns=qqc_image_cols, 
#                   columns_different = list("q10_7_photo"="q10_7_QA", "q2_3_3_hg_photo"="q2_3_2_hg_photo_QA"), suffix="QA", sheet=sheet)
#   )
# }

## HMIS Service assessment
hmis_tool <- read_excel(hmis_tool_path, "survey", guess_max = 100000)
hmis_missing_log <- data.frame()
# Loop through all the sheets
for(sheet in names(hmis_data_approved)){
  hmis_image_cols <- hmis_tool %>% filter(type %in% c("image") & name %in% names(hmis_data_approved[[sheet]])) %>% pull(name)
  hmis_audio_cols <- hmis_tool %>% filter(type %in% c("audio") & name %in% names(hmis_data_approved[[sheet]])) %>% pull(name)
  # Log
  hmis_missing_log <-  rbind(
    hmis_missing_log,
    # Translation
    log_questions(data=hmis_data_approved[[sheet]], columns=hmis_audio_cols, suffix="Translation", sheet=sheet),
    # Image QA
    log_questions(data=hmis_data_approved[[sheet]], columns=hmis_image_cols, suffix="QA", sheet=sheet)
  )
}

## SP Personnel 
sp_tool <- read_excel(sp_tool_path, "survey", guess_max = 100000)
sp_missing_log <- data.frame()
# Loop through all the sheets
for(sheet in names(sp_data_approved)){
  sp_image_cols <- sp_tool %>% filter(type %in% c("image") & name %in% names(sp_data_approved[[sheet]])) %>% pull(name)
  sp_audio_cols <- sp_tool %>% filter(type %in% c("audio") & name %in% names(sp_data_approved[[sheet]])) %>% pull(name)
  # Log
  sp_missing_log <-  rbind(
    sp_missing_log,
    # Translation
    log_questions(data=sp_data_approved[[sheet]], columns=sp_audio_cols, suffix="Translation", sheet=sheet),
    # Image QA
    log_questions(data=sp_data_approved[[sheet]], columns=sp_image_cols, suffix="QA", sheet=sheet)
  )
}

## Vignette 
vig_tool <- read_excel(vignette_tool_path, "survey", guess_max = 100000)
vig_audio_cols <- vig_tool %>% filter(type %in% c("audio")) %>% pull(name) 
vig_image_cols <- vig_tool %>% filter(type %in% c("image")) %>% pull(name)

vig_missing_log <- rbind(
  # Translation
  log_questions(data=vignette_data, columns=vig_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=vignette_data, columns=vig_image_cols, suffix="QA", sheet="data")
)

## Patient Verification 
patient_tool <- read_excel(patient_tool_path, "survey", guess_max = 100000)
patient_audio_cols <- patient_tool %>% filter(type %in% c("audio")) %>% pull(name) 
patient_image_cols <- patient_tool %>% filter(type %in% c("image")) %>% pull(name)

patient_missing_log <- rbind(
  # Translation
  log_questions(data=patient_data_approved, columns=patient_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=patient_data_approved, columns=patient_image_cols, 
                columns_different = list("photo_of_service-related_document"="Services_data_Photo_QA", "Doortag1_Photo"="Doortag_Photo_QA"), suffix="QA", sheet="data")
)


## Log Missing Translation -------------------------------------------------------------------------
excluded_cols <- read_excel("input/translation_columns.xlsx") 
# excluded_cols <- c("Village")

missing_translation_log <- rbind(
  # ## QoC
  # missing_translation(data = qoc_data_approved$data, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="QoC"]) %>% mutate(Tool = "QoC"),
  # missing_translation(data = qoc_data_approved$Health_Worker_Interview_Ques..., KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="QoC"]) %>% mutate(Tool = "QoC"),
  # ## QQC
  # missing_translation(data = qqc_data_approved$data, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="QQC"]) %>% mutate(Tool = "QQC"),
  ## SP Attendances
  missing_translation(data = sp_data_approved$data, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="SP_data"]) %>% mutate(Tool = "SP_data"),
  missing_translation(data = sp_data_approved$Personnel, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="SP_Personnel"]) %>% mutate(Tool = "SP_Personnel"),
  missing_translation(data = sp_data_approved$Absent_Days, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="SP_Absent"]) %>% mutate(Tool = "SP_Absent"),
  ## Vignette
  missing_translation(data = vignette_data_approved, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="Vignette"]) %>% mutate(Tool = "Vignette"),
  ## Patient Verification
  missing_translation(data = patient_data_approved, KEY = "KEY", excluded_cols$question[excluded_cols$Tool=="Patient"]) %>% mutate(Tool = "Patient")
)

# ## HF Data Verification 
# for(sheet in names(HF_data_approved)){
#   missing_translation_log <- rbind(
#     missing_translation_log,
#     missing_translation(data = HF_data_approved[[sheet]], KEY = "KEY_Unique", excluded_cols$question[excluded_cols$Tool==paste0("HF_", sheet)]) %>% 
#       mutate(Tool = paste0("HF_", sheet))
#   )
# }

## HMIS Service assessment
for(sheet in names(hmis_data_approved)){
  missing_translation_log <- rbind(
    missing_translation_log,
    missing_translation(data = hmis_data_approved[[sheet]], KEY = "KEY_Unique", excluded_cols$question[excluded_cols$Tool==paste0("HMIS_", sheet)]) %>% 
      mutate(Tool = paste0("HMIS_", sheet))
  )
}

## Export List -------------------------------------------------------------------------------------
## Missing Translation and QA status
missing_translation_QA_log <- rbind(
  # HF_missing_log %>% mutate(Tool = "HF_Verification"),
  # QoC_missing_log %>% mutate(Tool = "QoC"),
  # qqc_missing_log %>% mutate(Tool = "QQC"),
  hmis_missing_log %>% mutate(Tool = "HMIS"),
  sp_missing_log %>% mutate(Tool = "SP_Attendance"),
  vig_missing_log %>% mutate(Tool = "Vignette"),
  patient_missing_log %>% mutate(Tool = "Patient_Verification")
) %>% 
  mutate(key = str_split_fixed(KEY, "/", 2)[,1], .after = KEY) %>%
  arrange(Tool, KEY)

## Separate translation and image logs
missing_translation_QA_log_sub <- missing_translation_QA_log %>% 
  filter(question_type == "Translation") %>% 
  filter(!(question %in% c("Final_Comments_Translation", "Surveyor_Comments_Translation") & issue %in% "Audio Translation is missing"))

# Export list
missing_translation_QA_log <- list(
  Image_log=filter(missing_translation_QA_log, question_type=="QA"),
  Audio_log=missing_translation_QA_log_sub
)

# remove extra objects -----------------------------------------------------------------------------
rm(HF_tool, HF_missing_log, HF_audio_cols, HF_image_cols, qoc_tool, qoc_audio_cols, qoc_image_cols, 
   QoC_missing_log, qqc_tool, qqc_missing_log, qqc_image_cols, qqc_audio_cols, hmis_tool, hmis_missing_log, 
   hmis_audio_cols, hmis_image_cols, sp_tool, sp_audio_cols, sp_image_cols, sp_missing_log, vig_tool, 
   vig_missing_log, vig_audio_cols, vig_image_cols, patient_tool, patient_missing_log, patient_audio_cols,
   patient_image_cols, excluded_cols)

