##### Data Processing Script #####
# Install/load required packages -------------------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(janitor)) install.packages("janitor")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")

source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Declaring Global Variables -----------------------------------------------------------------------
hf_tool_path <- "input/tools/HER+Re+-+HF+Level+Data+Verification+Tool+-+R3.xlsx"
qoc_tool_path <- "input/tools/HER+Re+-+QoC+-+Interview+with+Health+Workers+-+R3.xlsx"
qqc_tool_path <- "input/tools/HER+Re+-+QQC+-+R3.xlsx"
hmis_tool_path <- "input/tools/HER+Re+-+HMIS+and+Service+Assessment+-+R3.xlsx"
sp_tool_path <- "input/tools/HER+Re+-+SP+Personnel+Attendance+Check+-+R3.xlsx"
vignette_tool_path <- "input/tools/Vignette.xlsx"
patient_tool_path <- "input/tools/HER+Re+-+Patient+Verification+-+R3.xlsx"
# Survey CTO Download link extension
download_link <- "https://artftpm.surveycto.com/view/submission-attachment/"

# Check Tool versions against SCTO
print_tool_versions()

# Read data ----------------------------------------------------------------------------------------
HF_data = read_xlsx_sheets("input/raw_data/HER Re - HF Level Data Verification Tool - R3.xlsx")
qoc_data = read_xlsx_sheets("input/raw_data/HER Re - QoC - Interview with Health Workers R3.xlsx")
qqc_data = read_xlsx_sheets("input/raw_data/HER Re - QQC R3.xlsx")
hmis_data = read_xlsx_sheets("input/raw_data/HER Re - Service Assessment and Sampling Verification - R3.xlsx")
sp_data = read_xlsx_sheets("input/raw_data/HER Re - SP Personnel Attendance Check - R3.xlsx")
vignette_data = read_xlsx_sheets("input/raw_data/HER Re - Vignette R3.xlsx")
patient_data = read_excel("input/raw_data/HER Re Patient Verification - R3.xlsx", guess_max = 500000)

# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTdyi6ncX2kmDFajgSdvYepf8OC7fAJOeB67GqkEv5gvudykUQClQbm3sFxWrkaEcDJPfEZ_rsg3dUx/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=652263144&single=true&output=csv"), col_types = "c")
# qa_log2 <- readr::read_csv(paste0(url, "gid=32496178&single=true&output=csv"), col_types = "c")
detailed_check <- readr::read_csv(paste0(url, "gid=332627320&single=true&output=csv"), col_types = "c", guess_max = 500000)
correction_log <- readr::read_csv(paste0(url, "gid=357220174&single=true&output=csv"), col_types = "c")
rejection_log <- readr::read_csv(paste0(url, "gid=2093594510&single=true&output=csv"), col_types = "c")
# rejection_log <- data.frame()
# Translation log
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRSa4crz5VePsXjxKbykav4CWmkKhKCOcXWhQ7ATJYRo8fiHCbTOMAOoqbGLeuoATJzL0UcK9G4z1J6/pub?"
translation_log <- readr::read_csv(paste0(url, "gid=1083946710&single=true&output=csv"), col_types = "c") # 1101069904

# # merge
# names(qa_log1)[names(qa_log1) %notin% names(qa_log2)]
# names(qa_log2)[names(qa_log2) %notin% names(qa_log1)]
# qa_log <- rbind(qa_log1 %>% select(-c("data back check", "Back_Check_Remarks")),
#                 qa_log2) 
# rm(qa_log1, qa_log2)

# Join QA Status -----------------------------------------------------------------------------------
count(qa_log, Tool, QA_Status) # %>% View
qa_log_sub <- qa_log %>% 
  select(KEY=KEY_Unique, qa_status=QA_Status, Tool) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status
  )) %>% unique()

## Join
## HF Data Verification 
HF_data$data <- HF_data$data %>%
  left_join(filter(qa_log_sub, Tool=="Data Verification"), by="KEY") %>% select(-Tool) 
## QoC
qoc_data$data <- qoc_data$data %>%
  left_join(filter(qa_log_sub, Tool=="Interview with Health Workers (QoC)"), by="KEY") %>% select(-Tool) 
## QQC
qqc_data$data <- qqc_data$data %>%
  left_join(filter(qa_log_sub, Tool=="QQC"), by="KEY") %>% select(-Tool) 
## HMIS Service assessment
hmis_data$data <- hmis_data$data %>%
  left_join(filter(qa_log_sub, Tool=="Service Assessment & Sampling Verification Tool"), by="KEY") %>% select(-Tool) 
## SP Personnel 
sp_data$data <- sp_data$data %>%
  left_join(qa_log_sub, by="KEY") # %>% select(-Tool) # filter(qa_log_sub, Tool=="HER_HF_Study_Tool_1")
## Vignette 
vignette_data <- vignette_data %>%
  left_join(filter(qa_log_sub, Tool=="Vignette"), by="KEY") %>% select(-Tool) 
## Patient Verification 
patient_data <- patient_data %>%
  left_join(filter(qa_log_sub, Tool=="HER Re Patient Verification"), by="KEY") %>% select(-Tool) 

HF_data$data %>% count(qa_status)
qoc_data$data %>% count(qa_status)
qqc_data$data %>% count(qa_status)
hmis_data$data %>% count(qa_status)
sp_data$data %>% count(qa_status)
vignette_data %>% count(qa_status)
patient_data %>% count(qa_status)

# apply correction log -----------------------------------------------------------------------------
correction_log %>% count(Tool)
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R")  #### Check Red Flags Verification Tool
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Remove Rejected data ----------------------------------------------------------------------------
rejected_qa_status <- c("Rejected")
source("R/remove_rejected_data.R") # KEY_Unique should be used from this point onward for HF and HMIS data

# Relevancy check ----------------------------------------------------------------------------------
# file.edit("R/check_relevancy_rules.R")
source("R/check_relevancy_rules.R") # Check Is_There_A_Specific_Room_For_X_Ray
 
## Attach labels -----------------------------------------------------------------------------------
# file.edit("R/attach_labels.R")
source("R/attach_labels.R") 

# apply Translation log ----------------------------------------------------------------------------
translation_log %>% count(Tool, Tab_Name)
# file.edit("R/apply_translation_log.R")
source("R/apply_translation_log.R")
if(nrow(translation_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Recode ------------------------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R") # Note: Don't relabel numerics in analysis data
# Recode 8888 in HMIS patient sheet: Patient_Age_Years
# Which_Visit_Is_Selected_In_Our_Sample_ANC

# produce qa-backlog -------------------------------------------------------------------------------
# Check this part later
qa_log_sub <- qa_log %>% select(Tool, qa_status=QA_Status, KEY=KEY_Unique)
## Filter
QA_backlog_keys <- rbind(
  left_join(
    HF_data$data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="HF_Verification"),
  left_join(
    qoc_data$data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="QoC"),
  left_join(
    qqc_data$data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="QQC"),
  left_join(
    hmis_data$data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="HMIS"),
  left_join(
    sp_data$data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="SP"),
  left_join(
    vignette_data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="Vignette"),
  left_join(
    patient_data %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="Patient_Verification")) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>%
  filter(qa_status %notin% c("Approved", "Rejected")) # Filter Keys not yet QAed
# Count
QA_backlog <- QA_backlog_keys %>%
  group_by(SubmissionDate, Tool) %>% count(qa_status, name = "freq") %>%
  # mutate(percentage = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% arrange(SubmissionDate) %>%
  pivot_wider(names_from = "Tool", values_from = "freq")
# Print
print(knitr::kable(QA_backlog, format = "simple"))

## Filter Approved data ----------------------------------------------------------------------------
approved_qa_status <- c("Approved")#, "Pending", "Rejected", NA, "") #"Approved"
# file.edit("R/filter_approved_data.R")
source("R/filter_approved_data.R") # *** QQC filter sheets with no image

##Note: add constraint for number of times repeat groups are repeated
## Add "Checked & Verified" for all images (requested by QA) ---------------------------------------
# source("R/add_image_qa_status.R")

## Logic check -------------------------------------------------------------------------------------
# file.edit("R/logic_check.R")
source("R/logic_check.R") 

# Compare dataset responses with the Tools --------------------------------------------------------
# file.edit("R/dataset_responses_check.R")
source("R/dataset_responses_check.R")
response_log_list <- response_log_list %>% filter(question %notin% c("HF_Type", "q2_4_1"))

## Remove Extra columns ----------------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/remove_extra_columns.R") # Keeping PII temporarily for logic checks

# generate data with missing translations ----------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/check_missing_translation.R") # Check all the columns
# check Please_verify_why_you_could_not_locate_the_household_photo_QA
# Check untranslated data 

# Anyonimize Client Data ---------------------------------------------------------------------------
# file.edit("R/modify_client_data.R")
source("R/modify_client_data.R")

# Export -------------------------------------------------------------------------------------------
## QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)
qa_tracker_list <- list(
  qa_log=qa_log,
  correction_log=correction_log, 
  detailed_check=detailed_check,
  translation_log=translation_log,
  # addition_log=addition_log,
  rejection_log=rejection_log
)
log_issues <- list(Correction=correction_log_issues, Translation=translation_log_issues)

## create the output path
check_path("output/cleaned_data")
archive_datasets("output/cleaned_data") # Move previous datasets to Archive
## export cleaned datasets
openxlsx::write.xlsx(HF_data, paste0("output/cleaned_data/HER_HF_Level_Data_Verification_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates
openxlsx::write.xlsx(qoc_data, paste0("output/cleaned_data/HER_QoC_Interview_with_Health_Workers_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates
openxlsx::write.xlsx(qqc_data, paste0("output/cleaned_data/HER_QQC_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates
openxlsx::write.xlsx(hmis_data, paste0("output/cleaned_data/HER_Service_Assessment_Sampling_Verification_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates
openxlsx::write.xlsx(sp_data, paste0("output/cleaned_data/HER_SP_Personnel_Attendance_Check_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates
openxlsx::write.xlsx(vignette_data, paste0("output/cleaned_data/HER_Vignette_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates
openxlsx::write.xlsx(patient_data, paste0("output/cleaned_data/HER_Patient_Verification_R3_cleaned_", lubridate::today(), ".xlsx")) # writexl::write_xlsx misses up the dates

## export client datasets
check_path("output/client_data")
archive_datasets("output/client_data") # Move previous datasets to Archive
# header_color <- "#91CBD9"
export_datasets(HF_data_approved, paste0("output/client_data/HER_Ver_R3_HF_Level_Data_Verification_", lubridate::today(), ".xlsx")) # cleaned_approved
export_datasets(qoc_data_approved, paste0("output/client_data/HER_Ver_R3_QoC_Interview_", lubridate::today(), ".xlsx"))
export_datasets(qqc_data_approved, paste0("output/client_data/HER_Ver_R3_QQC_", lubridate::today(), ".xlsx"))
export_datasets(hmis_data_approved, paste0("output/client_data/HER_Ver_R3_Service_Assessment_Sampling_Verification_", lubridate::today(), ".xlsx"))
export_datasets(sp_data_approved, paste0("output/client_data/HER_Ver_R3_SP_Personnel_Attendance_Check_", lubridate::today(), ".xlsx"))
export_datasets(list(data=vignette_data_approved), paste0("output/client_data/HER_Ver_R3_Vignette_", lubridate::today(), ".xlsx"))
export_datasets(list(data=patient_data_approved), paste0("output/client_data/HER_Ver_R3_Patient_Verification_", lubridate::today(), ".xlsx"))

## export additional files
writexl::write_xlsx(qa_tracker_list, "output/QA_Tracker_logs.xlsx", format_headers = F) 
writexl::write_xlsx(log_issues, "output/correction_trans_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)
writexl::write_xlsx(relevancy_issues, "output/relevancy_issues.xlsx", format_headers = F)
writexl::write_xlsx(SM_issues, "output/Select_multiple_issues.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_QA_log, "output/Missing_audio_translation_&_image_QA.xlsx", format_headers = F)
writexl::write_xlsx(qa_backlog_list, "output/QA_backlog.xlsx", format_headers = F)
writexl::write_xlsx(response_log_list, "output/dataset_response_mismatch_with_tool.xlsx", format_headers = F)
writexl::write_xlsx(logical_issues_list, "output/Logical_issues.xlsx", format_headers = F) # Add later
writexl::write_xlsx(missing_data_list, paste0("output/missing_data/HER_Ver_HF_Missing_Data_", lubridate::today(), ".xlsx"))

