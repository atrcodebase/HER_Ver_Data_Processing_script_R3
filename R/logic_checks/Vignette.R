### Vignette ---------------------------------------------------------------------------------------

vignette_logical_issues <- bind_rows(
  #### Exit Interview 1
  vignette_data_approved %>% 
    # mutate(HF_Code=str_remove_all(HF_Code, "str_remove(df$code, '^0+')")) %>% 
    filter(HF_Code_based_on_sample!=as.numeric(HF_Code)) %>% 
    mutate(issue = "HF_Code does not match the sample",
           Questions = "HF_Code_based_on_sample - HF_Code",
           Values = paste0(HF_Code_based_on_sample, " - ", as.numeric(HF_Code))) %>%
    select(Questions, Values, issue, KEY),
    
  vignette_data_approved %>% 
    filter((HF_Type %in% c("Region Hospital (RH)", "Provincial Hospital (PH)") & Staff_Range_Per_HF != 20) | 
             (HF_Type %in% "District hospital (DH)" & Staff_Range_Per_HF != 8) | 
             (HF_Type %in% c("Comprehensive Health Centre (CHC)", "Comprehensive Health Centre (CHC +)") & Staff_Range_Per_HF != 3) | 
             (HF_Type %in% c("Basic Health Centre (BHC)", "Basic Health Centre (BHC+)", "Sub Health Centre (SHC)") & Staff_Range_Per_HF != 2)) %>% 
    mutate(issue = case_when(
      HF_Type %in% c("Basic Health Centre (BHC)", "Basic Health Centre (BHC+)", "Sub Health Centre (SHC)") ~ "Staff Range for BHC-BHC+-SHC is: 2",
      HF_Type %in%  c("Comprehensive Health Centre (CHC)", "Comprehensive Health Centre (CHC +)") ~ "Staff Range for CHC-CHC+ is: 3",
      HF_Type %in% "District hospital (DH)" ~ "Staff Range for DH is: 8",
      HF_Type %in% c("Region Hospital (RH)", "Provincial Hospital (PH)") ~ "Staff Range for RH-PH is: 20"
    ),
    Questions = "HF_Type - Staff_Range_Per_HF",
    Values = paste0(HF_Type, " - ", Staff_Range_Per_HF)) %>% 
    select(Questions, Values, issue, KEY),
  
  vignette_data_approved %>% 
    filter(Respondent_Position %in% "Midwife" & Gender_Of_Interviewee %in% "Male") %>% 
    mutate(issue = "It's highly unlikely for men to be midwifes, please double-check!",
           Questions = "Respondent_Position - Gender_Of_Interviewee",
           Values = paste0(Respondent_Position, " - ", Gender_Of_Interviewee)) %>%
    select(Questions, Values, issue, KEY),
  
  vignette_data_approved %>% 
    filter(child_age_months > 59 | child_age_months < 0) %>% 
    mutate(issue = "Child age should be under 59 months!",
           Questions = "child_age_months",
           Values = paste0(child_age_months)) %>%
    select(Questions, Values, issue, KEY),
  
  #### Exit Interview 2
  vignette_data_approved %>% 
    filter(child_age_months2 < 18) %>% 
    mutate(issue = "Respondent should be 18 or older!",
           Questions = "child_age_months2",
           Values = paste0(child_age_months2)) %>%
    select(Questions, Values, issue, KEY)
)


#### Flag Numeric values in Other/Numeric Questions
vign_other_num_issues = rbind(
  flag_numeric_values(vignette_data_approved, vignette_tool_path, Tool="Vignette")
)



## HF Level checks ---------------------------------------------------------------------------------
vign_int_types <- c("Exit Interview 1",
                  "Exit Interview 2",
                  "Vignette [1] Respiratory Infection: Pneumonia",
                  "Vignette [2]: Diarrhea with Cat B dehydration",
                  "Vignette [3] Growth Monitoring and Promotion",
                  "Vignette [4]: The management of presumptive TB patient",
                  "Vignette [5] Labor",
                  "Vignette [6] PNC pre Discharge",
                  "Vignette [7] First ANC Visit",
                  "Vignette [8] Birth Spacing/ Family Planning : FP Counselling",
                  "Vignette [9] Birth Spacing: Change FP method to Injectable",
                  "Vignette [10] Post Partum Haemorrhage - Atonic Uterus at Health Centre")
exit_ints <- c("Exit Interview 1", "Exit Interview 2")
vign_1 <- c("Vignette [1] Respiratory Infection: Pneumonia", 
            "Vignette [2]: Diarrhea with Cat B dehydration",
            "Vignette [3] Growth Monitoring and Promotion",
            "Vignette [4]: The management of presumptive TB patient")
vign_2 <- c("Vignette [5] Labor",
            "Vignette [6] PNC pre Discharge",
            "Vignette [7] First ANC Visit",
            "Vignette [8] Birth Spacing/ Family Planning : FP Counselling",
            "Vignette [9] Birth Spacing: Change FP method to Injectable",
            "Vignette [10] Post Partum Haemorrhage - Atonic Uterus at Health Centre")

# Project team Targets: 2 exit interviews, 2 from Vignettes 1 to 4, and 2 from Vignettes 5 to 10
vign_interview_type <- vignette_data_approved %>% 
  group_by(HF_Name_based_on_Sample, HF_Code_based_on_sample, HF_Type_based_on_sample) %>% 
  reframe(Total_Interview_Types = length(unique(Interview_Type)),
          Collected_data = paste0(Interview_Type, collapse = " & "),
          
          #### The following targets were explained by project team (Sahebi)
          ## Exit Interviews: Each HF should have both Exit Interviews
          exit_collected = length(unique(Interview_Type[Interview_Type %in% exit_ints])),
          exit_missing=paste0(exit_ints[exit_ints %notin% Interview_Type], collapse = " & "),
          
          ## Vignette 1 (1-4): Each HF should have at least 2 interviews from Vignettes 1-4
          Vign1_collected = length(unique(Interview_Type[Interview_Type %in% vign_1])),
          Vign1_missing=ifelse(Vign1_collected<2, 
                               paste0(vign_1[vign_1 %notin% Interview_Type], collapse = " & "), # TRUE
                               NA_character_ # FALSE
                               ),
          ## Vignette 2 (5-10): Each HF should have at least 2 interviews from Vignettes 5-10
          # Vign2_missing=paste0(vign_2[vign_2 %notin% Interview_Type], collapse = " & "),
          Vign2_collected = length(unique(Interview_Type[Interview_Type %in% vign_2])),
          Vign2_missing=ifelse(Vign2_collected<2, 
                               paste0(vign_2[vign_2 %notin% Interview_Type], collapse = " & "), # TRUE
                               NA_character_ # FALSE
          ),
          # Missing_data = paste0(hf_int_types[hf_int_types %notin% Interview_Type], collapse = " & "),
          # flag interview types collected more than once
          Duplicate_flag = any(duplicated(Interview_Type)),
          # list which ones were duplicated
          Duplicated_types = paste0(unique(Interview_Type[duplicated(Interview_Type)]), collapse = " - ")
          
  ) %>% ungroup() %>% unique() 
