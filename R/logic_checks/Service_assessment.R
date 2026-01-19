## Service Assessment and Sampling Verification ----------------------------------------------------
hmis_logical_issues <- plyr::rbind.fill(
  
  #### Data
  # Inconsistenceis
  hmis_data_approved$data %>%
    filter((HF_Type %in% "Regional Hospital (RH)" & HF_Type_based_on_sample %notin% c("RH", "Regional / National Hospital (H1)")) |
             (HF_Type %in% "Provincial Hospital (PH)" & HF_Type_based_on_sample %notin% c("PH", "Provincial Hospital (H2)")) |
             (HF_Type %in% "District hospital (DH)" & HF_Type_based_on_sample %notin% c("DH", c("District Hospital (H3)", "DH"))) |
             (HF_Type %in% c("Comprehensive Health Centre (CHC)", "Comprehensive Health Centre (CHC +)") & HF_Type_based_on_sample %notin% "CHC") |
             (HF_Type %in% "" & HF_Type_based_on_sample %notin% "") |
             (HF_Type %in% c("Basic Health Centre (BHC)", "Basic Health Centre (BHC+)") & HF_Type_based_on_sample %notin% "BHC") |
             (HF_Type %in% "Sub Health Centre (SHC)" & HF_Type_based_on_sample %notin% "SHC")) %>% 
    mutate(issue = "HF Type based on sample & observation are different, please double-check!",
           Questions = "HF_Type - HF_Type_based_on_sample",
           Values = paste0(HF_Type, " - ", HF_Type_based_on_sample)) %>%
    select(Questions, Values, issue, KEY),
  
  hmis_data_approved$data %>%
    mutate(Register_Type_English_calc=case_when(
      Type_of_service_general %in% "Ante-natal care (ANC)" ~ "MCH",
      Type_of_service_general %in% "Post-natal care (PNC)" ~ "MCH/PNC",
      Type_of_service_general %in% "Institutional Delivery"  ~ "delivery",
      Type_of_service_general %in% "Pentavalent vaccine (3rd dose)"  ~ "Expanded Programme on Immunization (EPI)",
      Type_of_service_general %in% "Couple-year protection (CYP)/Family Planning"  ~ "Family Planning (FP)",
      Type_of_service_general %in% "Toxoid Tetanus (DT+/TT+) vaccine (2nd dose and plus) for women of reproductive age"  ~ "Expanded Programme on Immunization (EPI)",
      Type_of_service_general %in% "Tuberculosis exam (TB smear+) case cured"  ~ "TB smear+(Microscopic exam, GeneXpert and Calture)",
      Type_of_service_general %in% "Growth monitoring of under 2 years"  ~ "growth monitoring of children Under-Two years (GM)",
      Type_of_service_general %in% "Under 5 children morbidities"  ~ "Outpatient",
      Type_of_service_general %in% "C-section" ~ "C-sections",
      Type_of_service_general %in% "Major surgery" ~ "surgical operations",
      TRUE ~ NA_character_
    )) %>% 
    filter(Register_Type_English != Register_Type_English_calc) %>% 
    mutate(issue = paste0("Register Type should be [", Register_Type_English_calc, "] for this Type of service: ", Type_of_service_general),
           Questions = "Register_Type_English - Type_of_service_general",
           Values = paste0(Register_Type_English, " - ", Type_of_service_general)) %>%
    select(Questions, Values, issue, KEY),
  
  hmis_data_approved$data %>%
    mutate(Register_Type_English_calc=case_when(
      Type_of_service_general %in% "Ante-natal care (ANC)" ~ "MCH",
      Type_of_service_general %in% "Post-natal care (PNC)" ~ "MCH/PNC",
      Type_of_service_general %in% "Institutional Delivery"  ~ "delivery",
      Type_of_service_general %in% "Pentavalent vaccine (3rd dose)"  ~ "Expanded Programme on Immunization (EPI)",
      Type_of_service_general %in% "Couple-year protection (CYP)/Family Planning"  ~ "Family Planning (FP)",
      Type_of_service_general %in% "Toxoid Tetanus (DT+/TT+) vaccine (2nd dose and plus) for women of reproductive age"  ~ "Expanded Programme on Immunization (EPI)",
      Type_of_service_general %in% "Tuberculosis exam (TB smear+) case cured"  ~ "TB smear+(Microscopic exam, GeneXpert and Calture)",
      Type_of_service_general %in% "Growth monitoring of under 2 years"  ~ "growth monitoring of children Under-Two years (GM)",
      Type_of_service_general %in% "Under 5 children morbidities"  ~ "Outpatient",
      Type_of_service_general %in% "C-section" ~ "C-sections",
      Type_of_service_general %in% "Major surgery" ~ "surgical operations",
      TRUE ~ NA_character_
    )) %>% 
    filter(Register_Type_English != Register_Type_English_calc) %>% 
    mutate(issue = paste0("Register Type should be [", Register_Type_English_calc, "] for this Type of service: ", Type_of_service_general),
           Questions = "Register_Type_English - Type_of_service_general",
           Values = paste0(Register_Type_English, " - ", Type_of_service_general)) %>%
    select(Questions, Values, issue, KEY),

  hmis_data_approved$data %>% 
    mutate(HMIS_HMIR_MIAR_English_calc=case_when(
      Type_of_service_general %in% c("Ante-natal care (ANC)", "Post-natal care (PNC)", "Institutional Delivery") ~ "HMIS (MIAR)",
      Type_of_service_general %in% "Pentavalent vaccine (3rd dose)"  ~ "Expanded Programme on Immunization (EPI) HMIS (MIAR)",
      Type_of_service_general %in% "Couple-year protection (CYP)/Family Planning"  ~ "Family Planning HMIS (MIAR/HMIR)",
      Type_of_service_general %in% "Toxoid Tetanus (DT+/TT+) vaccine (2nd dose and plus) for women of reproductive age"  ~ "Expanded Programme on Immunization (EPI) HMIS (MIAR)",
      Type_of_service_general %in% "Tuberculosis exam (TB smear+) case cured"  ~ "TB HMIS (MIAR)",
      Type_of_service_general %in% "Growth monitoring of under 2 years"  ~ "growth monitoring of children Under-Two years (GM) HMIS (MIAR)",
      Type_of_service_general %in% "Under 5 children morbidities"  ~ "Outpatient HMIS (MIAR)",
      Type_of_service_general %in% "C-section" ~ "HMIS (HMIR)",
      Type_of_service_general %in% "Major surgery" ~ "surgical HMIS (HMIR)",
      TRUE ~ NA_character_
    )) %>% 
    filter(HMIS_HMIR_MIAR_English != HMIS_HMIR_MIAR_English_calc) %>% 
    mutate(issue = paste0("HMIS Type should be [", HMIS_HMIR_MIAR_English_calc, "] for this Type of service: ", Type_of_service_general),
           Questions = "HMIS_HMIR_MIAR_English - Type_of_service_general",
           Values = paste0(HMIS_HMIR_MIAR_English, " - ", Type_of_service_general)) %>%
    select(Questions, Values, issue, KEY),
    
  hmis_data_approved$data %>% 
    filter((!is.na(Type_of_service_general) & is.na(rep_service_count)) | (is.na(Type_of_service_general) & !is.na(rep_service_count))) %>% 
    mutate(issue = "The question response and repeat sheet count does not match!",
           Questions = "Type_of_service_general - rep_service_count",
           Values = paste0(Type_of_service_general, " - ", rep_service_count)) %>%
    select(Questions, Values, issue, KEY),
  
  #### rep_service
  hmis_data_approved$rep_service %>% 
    filter((rep_service_hf_register_count < 1 & Register_Available %notin% c("No", NA)) | (rep_service_hf_register_count > 0 & Register_Available %notin% "Yes")) %>% 
    mutate(issue = "The question response and repeat sheet count does not match!",
           Questions = "Register_Available - rep_service_hf_register_count",
           Values = paste0(Register_Available, " - ", rep_service_hf_register_count),
           Tool="HMIS_rep_service") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$rep_service %>% 
    filter((Miar_Hmir_Hmis_Photos_For_M1_M2_M3_Count < 1 & Hmis_Miar_Hmir_For_Available %notin% c("No", NA)) | (Miar_Hmir_Hmis_Photos_For_M1_M2_M3_Count > 0 & Hmis_Miar_Hmir_For_Available %notin% "Yes")) %>% 
    mutate(issue = "The question response and repeat sheet count does not match!",
           Questions = "Hmis_Miar_Hmir_For_Available - Miar_Hmir_Hmis_Photos_For_M1_M2_M3_Count",
           Values = paste0(Hmis_Miar_Hmir_For_Available, " - ", Miar_Hmir_Hmis_Photos_For_M1_M2_M3_Count),
           Tool="HMIS_rep_service") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$rep_service %>% 
    filter((Health_Register_Photos_For_M1_M2_M3_Count < 1 & Register_Available %notin% c("No", NA)) | (Health_Register_Photos_For_M1_M2_M3_Count > 0 & Register_Available %notin% "Yes")) %>% 
    mutate(issue = "The question response and repeat sheet count does not match!",
           Questions = "Register_Available - Health_Register_Photos_For_M1_M2_M3_Count",
           Values = paste0(Register_Available, " - ", Health_Register_Photos_For_M1_M2_M3_Count),
           Tool="HMIS_rep_service") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$rep_service %>% 
    filter((str_count(Service_Type, ";")+1)!=rep_service_hf_register_count) %>% 
    mutate(issue = "The question response and repeat sheet count does not match!",
           Questions = "Service_Type - rep_service_hf_register_count",
           Values = paste0(Service_Type, " - ", rep_service_hf_register_count),
           Tool="HMIS_rep_service") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$rep_service %>% 
    filter((as.numeric(Number_Of_Patients_Based_On_The_Specified_Target) %/% as.numeric(Number_Of_Samples))!=as.numeric(interval_sampling)) %>% 
    mutate(issue = "Interval calculation does not match the division of the two values, plz double-check!",
           Questions = "Number_Of_Patients_Based_On_The_Specified_Target - Number_Of_Samples - interval_sampling",
           Values = paste0(Number_Of_Patients_Based_On_The_Specified_Target, " - ", Number_Of_Samples, " - ", interval_sampling),
           Tool="HMIS_rep_service") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$rep_service %>% 
    filter(Number_Of_Samples!=Patient_Sampling_Verification_count) %>% 
    mutate(issue = "Inconsistent values!",
           Questions = "Number_Of_Samples - Patient_Sampling_Verification_count",
           Values = paste0(Number_Of_Samples, " - ", Patient_Sampling_Verification_count),
           Tool="HMIS_rep_service") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  #### rep_service_hf_register
  hmis_data_approved$rep_service_hf_register %>% 
    filter(Total_Verified_Visits!=(as.numeric(Number_Visits_In_The_Health_Register_Month1)+as.numeric(Number_Visits_In_The_Health_Register_Month2)+as.numeric(Number_Visits_In_The_Health_Register_Month3))) %>% 
    mutate(issue = "Total doesn't match the sum of parts!",
           Questions = "Total_Verified_Visits - Number_Visits_In_The_Health_Register_Month1 -	Number_Visits_In_The_Health_Register_Month2 -	Number_Visits_In_The_Health_Register_Month3",
           Values = paste0(Total_Verified_Visits, " - ", Number_Visits_In_The_Health_Register_Month1, " - ", Number_Visits_In_The_Health_Register_Month2, " - ", Number_Visits_In_The_Health_Register_Month3),
           Tool="HMIS_rep_service_hf_register") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  #### Patient_Sampling_Verification
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(((as.numeric(Patient_Age_Years)*12)+as.numeric(Patient_Age_Months) != as.numeric(Total_Months)) & Total_Months %notin% 8888) %>%
    mutate(issue="Age total does not match years+months!",
           Questions = "Patient_Age_Years - Patient_Age_Months - Total_Months",
           Values = paste0(Patient_Age_Years, " - ", Patient_Age_Months, " - ", Total_Months),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  ## Service Type vs Patient Age checks
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(Type_of_service_general %in% "Under 5 children morbidities" & as.numeric(Total_Months) >= 66) %>% # 60 + 6 months since reporting period
    mutate(issue="Visited HF for under 5 morbities but the patient age is more than 5 years old",
           Questions = "Type_of_service_general - Total_Months - Age_in_Years",
           Values = paste0(Type_of_service_general, " - ", Total_Months, " - ", as.numeric(Total_Months)/12),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),

  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(Type_of_service_general %in% "Pentavalent vaccine (3rd dose)" & as.numeric(Total_Months) >= 10) %>% # 4 + 6 months since reporting period
    mutate(issue="The Pentavalent vaccine is only administered to children of no more than 4 months old",
           Questions = "Type_of_service_general - Total_Months - Age_in_Years",
           Values = paste0(Type_of_service_general, " - ", Total_Months, " - ", as.numeric(Total_Months)/12),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(Type_of_service_general %in% "Growth monitoring of under 2 years" & as.numeric(Total_Months) >= 30) %>% # 24 + 6 months since reporting period
    mutate(issue="Visited HF for children below 2 years but age is reported above 2 years",
           Questions = "Type_of_service_general - Total_Months - Age_in_Years",
           Values = paste0(Type_of_service_general, " - ", Total_Months, " - ", as.numeric(Total_Months)/12),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(Type_of_service_general %in% c("Ante-natal care (ANC)",
                                          "Post-natal care (PNC)",
                                          "Institutional Delivery",
                                          "Couple-year protection (CYP)/Family Planning",
                                          "C-section") & as.numeric(Total_Months) < 180) %>% 
    mutate(issue="Visited HF for pregnancy related services but the patient age is too small, plz double-check",
           Questions = "Type_of_service_general - Total_Months - Age_in_Years",
           Values = paste0(Type_of_service_general, " - ", Total_Months, " - ", as.numeric(Total_Months)/12),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  
  # Pregnancy related services but patient is not Female
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(Type_of_service_general %in% c("Ante-natal care (ANC)",
                                          "Post-natal care (PNC)",
                                          "Institutional Delivery",
                                          "Couple-year protection (CYP)/Family Planning",
                                          "C-section") & Patient_Gender %notin% "Female") %>% 
    mutate(issue="Pregnancy related services but patient is not Female",
           Questions = "Type_of_service_general - Patient_Gender",
           Values = paste0(Type_of_service_general, " - ", Patient_Gender),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),

  # Outlier age
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(as.numeric(Total_Months) > 1176 & Total_Months %notin% 8888) %>% 
    mutate(issue="Outlier age, plz double-check",
           Questions = "Total_Months - Age_in_Years",
           Values = paste0(Total_Months, " - ", as.numeric(Total_Months)/12),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool),
  # The Male Patient has Husband name
  hmis_data_approved$Patient_Sampling_Verification %>%
    filter(Patient_Gender %in% "Male" & !is.na(Husband_Name)) %>%
    mutate(issue = "The Male Patient has 'Husband name'",
           Questions = "Patient_Gender - Husband_Name",
           Values = paste0(Patient_Gender, " - ", Husband_Name),
           Tool="HMIS_Patient_Sampling") %>%
    select(Questions, Values, issue, KEY=KEY_Unique, Tool)
)  %>% mutate(Tool=ifelse(is.na(Tool), "HMIS", Tool))

#### Flag Numeric values in Other/Numeric Questions
hmis_other_num_issues <- c()
for(sheet in names(hmis_data_approved)){
  # Log
  hmis_other_num_issues = rbind(
    hmis_other_num_issues,
    flag_numeric_values(hmis_data_approved[[sheet]], hmis_tool_path, Tool="HMIS")
  )
}


# ## Check Constraint Issues 
# constraint_rules <- read_excel("input/tool_relevancy_rules/HER_Verification_constraint_rules.xlsx", sheet="HMIS")
# 
# check_constraints <- function(data, constraint_rules){
#   
#   constraint_rules <- constraint_rules %>% filter(Question %in% names(data))
#   # print(constraint_rules$Question)
#   constraint_issues <- c()
#   for(question in constraint_rules$Question){
#     xls_formula <- constraint_rules$XLS_Constraint[constraint_rules$Question %in% question]
#     const_formula <- constraint_rules$Corrected_rule[constraint_rules$Question %in% question]
#     
#     constraint_issues <- rbind(
#       constraint_issues,
#       data %>% 
#         filter(eval(parse(text=const_formula))) %>% 
#         mutate(issue="Value is inconsistent with the constraint rule!",
#                Questions = question,
#                Values = get(question),
#                Constraint_Rule=xls_formula) %>%
#         select(Questions, Values, issue, Constraint_Rule, KEY)
#     )
#   }
#   return(constraint_issues)
# }
# constraint_issues <- c()
# for(sheet in names(hmis_data_approved)){
#   constraint_issues <- rbind(
#     constraint_issues,
#     check_constraints(hmis_data_approved[[sheet]], constraint_rules)
#   )
# }


## HF Level checks ---------------------------------------------------------------------------------
service_type <- c("Ante-natal care (ANC)",
                  "Post-natal care (PNC)",
                  "Institutional Delivery",
                  "Pentavalent vaccine (3rd dose)",
                  "Toxoid Tetanus (DT+/TT+) vaccine (2nd dose and plus) for women of reproductive age",
                  "Tuberculosis exam (TB smear+) case cured",
                  "Growth monitoring of under 2 years",
                  "Under 5 children morbidities",
                  "C-section"
                  # Relevancy for Service Type excludes these two services
                  # "Couple-year protection (CYP)/Family Planning",
                  # "Major surgery"
                  )

hmis_service_type <- hmis_data_approved$data %>% 
  group_by(HF_Name_based_on_Sample, HF_Code_based_on_sample) %>% 
  reframe(Total_Service_Types = length(unique(Type_of_service_general)),
          Collected_data = paste0(Type_of_service_general, collapse = " & "),
          Missing_data = paste0(service_type[service_type %notin% Type_of_service_general], collapse = " & "),
          # flag interview types collected more than once
          Duplicate_flag = any(duplicated(Type_of_service_general)),
          # list which ones were duplicated
          Duplicated_types = paste0(unique(Type_of_service_general[duplicated(Type_of_service_general)]), collapse = " - ")
          
  ) %>% ungroup() %>% unique() 

## Patient CHeck
# Duplicate Patient IDs
hmis_duplicate_patients <- hmis_data_approved$Patient_Sampling_Verification %>% 
  janitor::get_dupes(MA_PatientID) %>% 
  select(MA_PatientID, dupe_count, Site_Visit_ID, Province, District, HF_Code_based_on_sample,
         HF_Name_based_on_Sample, Name_Of_Patient, PARENT_KEY, KEY_Unique)

# Chech for repeat sheet mismatches ----------------------------------------------------------------
hmis_count_mismatch <- rbind(
  # rep_service (Main question included in Logic check)
  hmis_data_approved$rep_service %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="rep_service") %>%
    full_join(hmis_data_approved$data %>% select(main_sheet_count=rep_service_count, KEY) %>%
                mutate(Sheet="rep_service", Question="rep_service_count"), by=c("KEY", "Sheet")),
  # rep_service_hf_register (Main question included in Logic check)
  hmis_data_approved$rep_service_hf_register %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="rep_service_hf_register") %>%
    full_join(hmis_data_approved$rep_service %>% select(main_sheet_count=rep_service_hf_register_count, KEY=KEY_Unique) %>%
                mutate(Sheet="rep_service_hf_register", Question="rep_service_hf_register_count"), by=c("KEY", "Sheet")),
  # Health_Register_Photos_For_M... (Main question included in Logic check)
  hmis_data_approved$Health_Register_Photos_For_M... %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Health_Register_Photos_For_M...") %>%
    full_join(hmis_data_approved$rep_service %>% select(main_sheet_count=Health_Register_Photos_For_M1_M2_M3_Count, KEY=KEY_Unique) %>%
                mutate(Sheet="Health_Register_Photos_For_M...", Question="Health_Register_Photos_For_M1_M2_M3_Count"), by=c("KEY", "Sheet")),
  # rep_service_hmis (Main question included in Logic check)
  hmis_data_approved$rep_service_hmis %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="rep_service_hmis") %>%
    full_join(hmis_data_approved$rep_service %>% select(main_sheet_count=rep_service_hmis_count, KEY=KEY_Unique) %>%
                mutate(Sheet="rep_service_hmis", Question="rep_service_hmis_count"), by=c("KEY", "Sheet")),
  # Miar_Hmir_Hmis_Photos_For_M1... (Main question included in Logic check)
  hmis_data_approved$Miar_Hmir_Hmis_Photos_For_M1... %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Miar_Hmir_Hmis_Photos_For_M1...") %>%
    full_join(hmis_data_approved$rep_service %>% select(main_sheet_count=Miar_Hmir_Hmis_Photos_For_M1_M2_M3_Count, KEY=KEY_Unique) %>%
                mutate(Sheet="Miar_Hmir_Hmis_Photos_For_M1...", Question="Miar_Hmir_Hmis_Photos_For_M1_M2_M3_Count"), by=c("KEY", "Sheet")),
  # Patient_Sampling_Verification (Main question included in Logic check)
  hmis_data_approved$Patient_Sampling_Verification %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Patient_Sampling_Verification") %>%
    full_join(hmis_data_approved$rep_service %>% select(main_sheet_count=Patient_Sampling_Verification_count, KEY=KEY_Unique) %>%
                mutate(Sheet="Patient_Sampling_Verification", Question="Patient_Sampling_Verification_count"), by=c("KEY", "Sheet"))
) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & !(is.na(repeat_sheet_count) & main_sheet_count == 0)) %>% 
  mutate(Tool="HMIS")

