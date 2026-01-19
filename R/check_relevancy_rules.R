# Check Relevancy Rules ----------------------------------------------------------------------------
## Read
HF_tool_relevancy <- read_excel("input/tool_relevancy_rules/hf_verification_relevancy_rules.xlsx")
hmis_tool_relevancy <- read_excel("input/tool_relevancy_rules/HMIS_relevancy_rules.xlsx")
qoc_tool_relevancy <- read_excel("input/tool_relevancy_rules/QoC_relevancy_rules.xlsx")
qqc_tool_relevancy <- read_excel("input/tool_relevancy_rules/QQC_relevancy_rules.xlsx")
sp_tool_relevancy <- read_excel("input/tool_relevancy_rules/SP_relevancy_rules.xlsx")
vign_tool_relevancy <- read_excel("input/tool_relevancy_rules/Vignette_relevancy_rules.xlsx")
patient_tool_relevancy <- read_excel("input/tool_relevancy_rules/Patient_verification_relevancy_rules.xlsx")

# ### HF Level Data Verification ---------------------------------------------------------------------
# HF_tool_relevancy <- HF_tool_relevancy %>%
#   mutate(sheet=case_when(
#     sheet %in% "Drug_Availability_Reporting_Period" ~ "Drug_Availability_Reporting_...",
#     sheet %in% "Medicine_Stored_Systematic_Repeat" ~ "Medicine_Stored_Systematic_R...",
#     sheet %in% "Medicine_Stored_First_Expired_First_Repeat" ~ "Medicine_Stored_First_Expire...",
#     TRUE ~ sheet
#   ))
# HF_data_sub <- HF_data$data %>%
#   select(Consent, Interview_Type_SV, HF_Type, n_available_drug, qa_status,
#          Can_Speak_With_Two_Members, Control_Temperature, Windows_Can_Open,
#          Sunlight_cannot_enter, Area_Free_Moisture, Old_Storage_available, Filled_Tempreature_Chart,
#          Medicine_Not_Stored, Medicine_Stored_Systematic, Medicine_Stored_First_Expired_First,
#         Evidence_Of_Pests, Temperature_Chart, eq_rep, Register_Lab_Department_Available,
#         Is_There_A_Specific_Room_For_X_Ray,  KEY)
# # Join the subset with all sheets (new variable)
# HF_data_joined <- lapply(HF_data[-1], left_join, HF_data_sub, by = "KEY")
# 
# # Join specific columns
# HF_data_joined$Drug_Out_Of_Stock <- HF_data_joined$Drug_Out_Of_Stock %>%
#   left_join(
#     HF_data_joined$Drug_Availability_Reporting_... %>% select(How_Many_Times_Out_Of_Stock_RP, Was_Drug_Out_Of_Stock_RP, KEY_Unique),
#     by = c("PARENT_KEY" = "KEY_Unique")
#   )
# HF_data_joined$Consumable_Out_Of_Stock_RP <- HF_data_joined$Consumable_Out_Of_Stock_RP %>%
#   left_join(
#     HF_data_joined$List_Of_Consumables_RP %>% select(Was_Consumable_Out_Of_Stock_RP, Times_Were_Consumable_Out_Of_Stock_RP, KEY_Unique),
#     by = c("PARENT_KEY" = "KEY_Unique")
#   )
# HF_data_joined$Test_Component_Details <- HF_data_joined$Test_Component_Details %>%
#   left_join(
#     HF_data_joined$Lab %>% select(was_one_test_made, KEY_Unique),
#     by = c("PARENT_KEY" = "KEY_Unique")
#   )
# # Manually added in relevancy rule
# HF_data_joined$data <- HF_data$data %>%
#   left_join(
#     HF_data_joined$Personnel %>%
#       mutate(Is_Attendance=paste0(Date_Attendance, "_", Is_Attendance),
#              Mechanism_For_Attendance=paste0(Date_Attendance, "_", Mechanism_For_Attendance)) %>%
#       group_by(PARENT_KEY) %>%
#       reframe(Is_Attendance=paste0(Is_Attendance, collapse = ";"),
#               Mechanism_For_Attendance=paste0(Mechanism_For_Attendance, collapse = ";")),
#     by = c("KEY" = "PARENT_KEY")
#   )
# 
# # Check Relevancy
# HF_relevancy_issues <- check_relevancy_rules(HF_data_joined$data, HF_tool_relevancy, sheet_name="data")
# for(sheet in names(HF_data)[names(HF_data) != "data"]){
#   # Check
#   if(nrow(HF_data_joined[[sheet]])!=0){ # Temp condition
#     HF_relevancy_issues <- rbind(
#       HF_relevancy_issues,
#       check_relevancy_rules(HF_data_joined[[sheet]], HF_tool_relevancy, sheet_name=sheet, KEY="KEY_Unique")
#     )
#   }
# }
# 
# 
# ### QoC - Interview with Health Workers ------------------------------------------------------------
# qoc_tool_relevancy <- qoc_tool_relevancy %>% 
#   mutate(sheet=case_when(
#     sheet %in% "Health_Worker_Interview_Questions" ~ "Health_Worker_Interview_Ques...",
#     TRUE ~ sheet
#   ))
# 
# # Join the subset with all sheets (new variable)
# qoc_interview_joined <- qoc_data$Health_Worker_Interview_Ques... %>%
#   left_join(qoc_data$data %>%
#               select(Staff_member_interviewed_in_this, HF_Type, qa_status, KEY), by=c("PARENT_KEY"="KEY"))
# 
# # Check Relevancy Rules
# qoc_relevancy_issues <- rbind(
#   check_relevancy_rules(qoc_data$data, qoc_tool_relevancy, sheet_name="data"),
#   check_relevancy_rules(qoc_interview_joined, qoc_tool_relevancy, sheet_name="Health_Worker_Interview_Ques...")
#   
# )
# 
# ### QQC --------------------------------------------------------------------------------------------
# # Get relevancy related questions from main sheet to join with other sheets
# qqc_main_cols <- qqc_tool_relevancy %>% 
#   filter(sheet != "data") %>% 
#   pull(relevant_question) %>% 
#   str_split(" - ") %>% unlist() %>% 
#   unique() %>% .[. %in% names(qqc_data$data)]
# 
# qqc_data_sub <- qqc_data$data %>%
#   select(all_of(qqc_main_cols), KEY)
# 
# # Check Relevancy Rules
# qqc_relevancy_issues <- check_relevancy_rules(qqc_data$data, qqc_tool_relevancy, sheet_name="data")
# # Check relevancy rules of all the sheets
# for(sheet in names(qqc_data)[names(qqc_data) != "data"]){
#   
#   if(nrow(qqc_data[[sheet]])!=0){ # Temp condition
#     qqc_relevancy_issues <- rbind(
#       qqc_relevancy_issues,
#       qqc_data[[sheet]] %>% 
#         left_join(qqc_data_sub, by=c("PARENT_KEY"="KEY")) %>% # All Parent_KEYs are the same as KEY in main sheet
#         check_relevancy_rules(qqc_tool_relevancy, sheet_name=sheet, print=FALSE)
#     )
#   }
# 
# }
# 

### HMIS Service assessment ------------------------------------------------------------------------
hmis_tool_relevancy <- hmis_tool_relevancy %>% 
  mutate(sheet=case_when(
    sheet %in% "Health_Register_Photos_For_M1_M2_M3" ~ "Health_Register_Photos_For_M...",
    sheet %in% "Miar_Hmir_Hmis_Photos_For_M1_M2_M3" ~ "Miar_Hmir_Hmis_Photos_For_M1...",
    TRUE ~ sheet
  ))

# Join the subset with all sheets (new variable)
hmis_sub <- hmis_data$rep_service %>%
  left_join(hmis_data$data %>% select(Type_of_service_general, Survey_Language_Other, KEY), by="KEY") %>% 
  select(Consent, Register_Available, Hmis_Miar_Hmir_For_Available, Type_of_service_general, Survey_Language_Other, KEY)
hmis_data_joined <- lapply(hmis_data[-c(1:2)], left_join, hmis_sub, by = "KEY") # Join

# Check Relevancy Rules
hmis_relevancy_issues <- rbind(
  # check_relevancy_rules(hmis_data$data, hmis_tool_relevancy, sheet_name="data"), # No rules in data sheet
  check_relevancy_rules(hmis_data$rep_service, hmis_tool_relevancy, sheet_name="rep_service", KEY="KEY_Unique"),
  check_relevancy_rules(hmis_data_joined$rep_service_hf_register, hmis_tool_relevancy, sheet_name="rep_service_hf_register", KEY="KEY_Unique"),
  check_relevancy_rules(hmis_data_joined$Health_Register_Photos_For_M..., hmis_tool_relevancy, sheet_name="Health_Register_Photos_For_M...", KEY="KEY_Unique"),
  check_relevancy_rules(hmis_data_joined$rep_service_hmis, hmis_tool_relevancy, sheet_name="rep_service_hmis", KEY="KEY_Unique"),
  check_relevancy_rules(hmis_data_joined$Miar_Hmir_Hmis_Photos_For_M1..., hmis_tool_relevancy, sheet_name="Miar_Hmir_Hmis_Photos_For_M1...", KEY="KEY_Unique"),
  check_relevancy_rules(hmis_data_joined$Patient_Sampling_Verification, hmis_tool_relevancy, sheet_name="Patient_Sampling_Verification", KEY="KEY_Unique")
)

### SP Personal Attendance Check ------------------------------------------------------------------
# Check Relevancy Rules
sp_relevancy_issues <- rbind(
  check_relevancy_rules(sp_data$data, sp_tool_relevancy, sheet_name="data"),
  check_relevancy_rules(sp_data$Personnel %>% 
                          left_join(sp_data$data %>% select(Consent, SP_Coverage, KEY), by=c("PARENT_KEY"="KEY")), 
                        sp_tool_relevancy, sheet_name = "Personnel"),
  check_relevancy_rules(sp_data$Absent_Days %>%
                          left_join(sp_data$data %>% select(Consent, SP_Coverage, KEY), by=c("PARENT_KEY"="KEY")),
                        sp_tool_relevancy, sheet_name = "Absent_Days")
)

### Vignette ---------------------------------------------------------------------------------------
# Check Relevancy Rules
vignette_relevancy_issues <- check_relevancy_rules(vignette_data, vign_tool_relevancy, sheet_name="data")



### Patient Verification ---------------------------------------------------------------------------
# Check Relevancy Rules
patient_relevancy_issues <- check_relevancy_rules(patient_data, patient_tool_relevancy, sheet_name="data")




# Update Select_multiple series columns ------------------------------------------------------------
## HF Level Data Verification
# HF_SM_issues <- c()
# for(sheet in names(HF_data)){
#   HF_data[[sheet]] <- HF_data[[sheet]] %>%
#     update_series_cols(tool_path = hf_tool_path,
#                        question_separator="_")
#   # Check if updated correctly
#   HF_SM_issues <- rbind(
#     HF_SM_issues,
#     check_select_multiple(data=HF_data[[sheet]],
#                           tool_path = hf_tool_path,
#                           question_separator="_")
#   )
# }

# ## QoC
# qoc_data$data <- qoc_data$data %>% update_series_cols(tool_path = qoc_tool_path, question_separator="_")
# qoc_data$Health_Worker_Interview_Ques... <- qoc_data$Health_Worker_Interview_Ques... %>% update_series_cols(tool_path = qoc_tool_path, question_separator="_")
# # Check if updated correctly
# qoc_SM_issues <- rbind(
#   check_select_multiple(data=qoc_data$data, tool_path = qoc_tool_path, question_separator="_"),
#   check_select_multiple(data=qoc_data$Health_Worker_Interview_Ques..., tool_path = qoc_tool_path, question_separator="_")
# ) 
# 
# ### QQC
# qqc_data$data <- update_series_cols(data=qqc_data$data, tool_path = qqc_tool_path, question_separator="_")
# # Check if updated correctly
# qqc_SM_issues <- check_select_multiple(data=qqc_data$data, tool_path = qqc_tool_path, question_separator="_")


### HMIS Service assessment
HMIS_SM_issues <- c()
for(sheet in names(hmis_data)){
  hmis_data[[sheet]] <- hmis_data[[sheet]] %>%
    update_series_cols(tool_path = hmis_tool_path,
                       question_separator="_")
  # Check if updated correctly
  HMIS_SM_issues <- rbind(
    HMIS_SM_issues,
    check_select_multiple(data=hmis_data[[sheet]],
                          tool_path = hmis_tool_path,
                          question_separator="_")
  )
}
HMIS_SM_issues <- HMIS_SM_issues %>% filter(question %notin% "Service_Type")


### SP Personal Attendance Check
SP_SM_issues <- c()
for(sheet in names(sp_data)){
  sp_data[[sheet]] <- sp_data[[sheet]] %>%
    update_series_cols(tool_path = sp_tool_path,
                       question_separator="_")
  # Check if updated correctly
  SP_SM_issues <- rbind(
    SP_SM_issues,
    check_select_multiple(data=sp_data[[sheet]],
                          tool_path = sp_tool_path,
                          question_separator="_")
  )
}


### Vignette
vignette_data <- update_series_cols(data=vignette_data, tool_path = vignette_tool_path, question_separator="_")
# Check if updated correctly
Vig_SM_issues <- check_select_multiple(data=vignette_data, tool_path = vignette_tool_path, question_separator="_")


### Patient Verification
patient_data <- update_series_cols(data=patient_data, tool_path = patient_tool_path, question_separator="_")
# Check if updated correctly
patient_SM_issues <- check_select_multiple(data=patient_data, tool_path = patient_tool_path, question_separator="_")
                            


## Export List -------------------------------------------------------------------------------------
# Relevancy
relevancy_issues <- plyr::rbind.fill(
  # HF_relevancy_issues %>% mutate(Tool="HF_Verification"),
  # qoc_relevancy_issues %>% mutate(Tool="QoC"),
  # qqc_relevancy_issues %>% mutate(Tool="QQC"),
  hmis_relevancy_issues %>% mutate(Tool="HMIS"),
  sp_relevancy_issues %>% mutate(Tool="SP_Personnel"),
  vignette_relevancy_issues %>% mutate(Tool="Vignette"),
  patient_relevancy_issues %>% mutate(Tool="Patient_Verification")
) %>% 
  mutate(key = str_split_fixed(KEY, "/", 2)[,1], .after = KEY) %>%
  arrange(Tool, KEY)

relevancy_issues <- relevancy_issues %>% 
  left_join(select(qa_log, QA_Status, key=KEY_Unique))

# writexl::write_xlsx(relevancy_issues, "output/relevancy_issues.xlsx", format_headers = F)

## Select Multiple issues
SM_issues <- plyr::rbind.fill(
  # HF_SM_issues %>% mutate(Tool="HF_Verification"),
  # qoc_SM_issues %>% mutate(Tool="QoC"),
  # qqc_SM_issues %>% mutate(Tool="QQC"),
  HMIS_SM_issues %>% mutate(Tool="HMIS"),
  SP_SM_issues %>% mutate(Tool="SP_Personnel"),
  Vig_SM_issues %>% mutate(Tool="Vignette"),
  patient_SM_issues %>% mutate(Tool="Patient_Verification")
) %>% filter(!is.na(KEY))

# remove extra objects -----------------------------------------------------------------------------
rm(HF_tool_relevancy, hmis_tool_relevancy, qoc_tool_relevancy, qqc_tool_relevancy, sp_tool_relevancy, 
   vign_tool_relevancy, patient_tool_relevancy, HF_data_sub, HF_data_joined, qoc_interview_joined, 
   qqc_main_cols, qqc_data_sub, hmis_sub, hmis_data_joined, HF_SM_issues, qoc_SM_issues, qqc_SM_issues, 
   HMIS_SM_issues, SP_SM_issues, Vig_SM_issues, patient_SM_issues)

