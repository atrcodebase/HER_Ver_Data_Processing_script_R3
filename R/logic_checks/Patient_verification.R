## Patient Verification ----------------------------------------------------------------------------
patient_logical_issues <- rbind(
  patient_data_approved %>%
    mutate(visited_service_cal=case_when(
      Type_of_service %in% "1"  ~ "to receive ante-natal care?",
      Type_of_service %in% "2"  ~ "to receive post-natal care?",
      Type_of_service %in% "3"  ~ "for a delivery?",
      Type_of_service %in% "4"  ~ "to receive a pentavalent vaccine for your child?",
      Type_of_service %in% "5"  ~ "to receive family planning services?",
      Type_of_service %in% "6"  ~ "to receive a tetanus vaccine (TT+)?",
      Type_of_service %in% "7"  ~ "to receive a sputum exam for Tuberculosis?",
      Type_of_service %in% "8"  ~ "to get your child treatment for Under 5 (U5) Children Morbidities?",
      Type_of_service %in% "9"  ~ "to have your child’s nutrition status checked through measuring their weight, height, and upper arm circumference?",
      Type_of_service %in% "10" ~ "to receive a C-section?",
      Type_of_service %in% "11" ~ "to receive a major surgery?",
    )) %>% 
    filter(Visited_Service != visited_service_cal) %>%
    mutate(issue="The value is not accurate!",
           Questions = "Visited_Service - visited_service_cal",
           Values = paste0(Visited_Service, " - ", visited_service_cal)) %>%
    select(Questions, Values, issue, KEY, qa_status),
    
  patient_data_approved %>%
    filter(grepl("They did not have the medicine I needed", Can_you_explain_why) &
             Were_you_provided_with_any_medicine_in_the_health_facility %in% 
             c("Yes, I was provided medicine in the health facility", 
             "Yes, I was partially provided with medicines in the health facility but I had to buy the remaining medicines from a private pharmacy")
           ) %>%
    mutate(issue="The respondent says the HF didn't have the medicine and yet later on says Yes I was provided with medicine",
           Questions = "Can_you_explain_why - Were_you_provided_with_any_medicine_in_the_health_facility",
           Values = paste0(Can_you_explain_why, " - ", Were_you_provided_with_any_medicine_in_the_health_facility)) %>%
    select(Questions, Values, issue, KEY),
  
  patient_data_approved %>%
    filter(grepl("To receive a prescription", What_were_you_asked_to_pay_for) &
             Did_you_pay_anything_to_receive_the_medicines_from_the_health_facility %in% "No") %>%
    mutate(issue="The respondent says they were asked to pay for prescription but in also says they didn't pay anything to receive the medicine from HF",
           Questions = "What_were_you_asked_to_pay_for - Did_you_pay_anything_to_receive_the_medicines_from_the_health_facility",
           Values = paste0(What_were_you_asked_to_pay_for, " - ", Did_you_pay_anything_to_receive_the_medicines_from_the_health_facility)) %>%
    select(Questions, Values, issue, KEY),
  # QA Comment: After reviewing the forms, it appears that the sample itself contains issues that cannot be corrected. For example, in one form, the case belongs to ANC and should refer to a woman, but the age was recorded as 0 in the sampling. In another case, based on the vaccination card, TT+ was recorded — which also applies to a woman — yet the age in the sampling plan was recorded as 0. In conclusion, all data were reviewed by the QA team, and none of the interviews were conducted with a child.
  patient_data_approved %>%
    filter(resp_type %in% "Responded by patient themselves" & as.numeric(Patient_Age_Sample) < 10) %>%
    mutate(issue="Based on the age of the patient, it doesn't seem like the interview was done with the patient itself",
           Questions = "resp_type - Patient_Age_Sample",
           Values = paste0(resp_type, " - ", Patient_Age_Sample)) %>%
    select(Questions, Values, issue, KEY)
)


## HF Level checks ---------------------------------------------------------------------------------
duplicate_patients_ver <- patient_data_approved %>% 
  janitor::get_dupes(MA_PatientID) %>%
  select(MA_PatientID, KEY)

## Checking patients across two tools
meta_cols <- c("Site_Visit_ID", "Province", "District", "HF_Type_based_on_sample",
               "HF_Code_based_on_sample", "HF_Name_based_on_Sample", "MA_PatientID")

patient_cross_check <- full_join(
  hmis_data_approved$Patient_Sampling_Verification %>% 
    mutate(HMIS_Data="HMIS") %>% 
    select(all_of(meta_cols), HMIS_Data) %>% unique(),
  patient_data %>% 
    filter(!is.na(MA_PatientID)) %>% 
    mutate(Patient_Data="Patient") %>% 
    select(all_of(meta_cols), Patient_Data, Patient_QA_Status=qa_status) %>% unique(),
  by=meta_cols
) %>% 
  mutate(Visit_Status=case_when(
    !is.na(HMIS_Data) & !is.na(Patient_Data) ~ "Completed Visit",
    TRUE ~ "Pending"
  ),
  issue=case_when(
    !is.na(HMIS_Data) & is.na(Patient_Data) ~ "Patient Not Visited yet!",
    is.na(HMIS_Data) & !is.na(Patient_Data) ~ "Patient ID Not found in HMIS data, please double-check!",
  )
  ) # %>% filter(issue!="Patient in both data!")


patient_per_service <- patient_data %>% 
  filter(!is.na(MA_PatientID)) %>% 
  mutate(Interview_status=case_when(
    Did_you_locate_the_household_of_the_patient %in% "No" | Is_Anyone_Home %in% "No" | Consent %in% "No" ~ "Incomplete_Interview",
    Consent %in% "Yes" ~ "Complete_Interview"
  )) %>% 
  count(Site_Visit_ID, Province, District, HF_Type_based_on_sample, HF_Code_based_on_sample, 
        HF_Name_based_on_Sample, Type_of_service, Interview_status) %>% 
  pivot_wider(Site_Visit_ID:Type_of_service,  names_from = Interview_status, values_from = n) %>% 
  rowwise() %>% 
  mutate(Total=sum(Complete_Interview, Incomplete_Interview, na.rm = T), 
         Issues="There should be a minimum of 4 patient interviews per service per HF") %>% ungroup() %>% 
  filter(Complete_Interview<4)


# Note: cross check all patient info with HMIS
# ## Check with HMIS data (data source for patients info)

# hmis_data_approved$Patient_Sampling_Verification %>% 
#   mutate(Patient_Age_Sample=round(Total_Months/12)) %>% 
#   select(all_of(meta_cols), Patient_Name_Sample=Name_Of_Patient, Patient_Gender_Sample=Patient_Gender, 
#          Patient_Address_Sample=Patient_Address, Patient_DoV_Sample=Patient_Date_of_Visit, )