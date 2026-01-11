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
