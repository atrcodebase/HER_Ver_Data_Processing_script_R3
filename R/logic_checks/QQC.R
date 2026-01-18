## QQC ---------------------------------------------------------------------------------------------
QQC_logical_issues <- plyr::rbind.fill(
  ##### 1. GENERAL MANAGEMENT
  qqc_data_approved$data %>%
    filter(Gender_Of_Interviewee %in% "Male" & provider_function %in% c("Midwife")) %>%
    mutate(
      issue = "Highly unlikely for midwife to be male, plz check!",
      Questions = "Gender_Of_Interviewee - provider_function",
      Values = paste0(Gender_Of_Interviewee, " - ", provider_function)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  
  
  # Flagging if the interview type and respondent type are inconsistent
  qqc_data_approved$data %>%
    filter(Interview_Type_SV %in% "1. General Management" & provider_function %in% c("Nurse", "Driver", "Cleaner/Guard")) %>%
    mutate(
      issue = "The respondent type and the interview type are inconsistent!",
      Questions = "Interview_Type_SV - provider_function",
      Values = paste0(Interview_Type_SV, " - ", provider_function)
    ) %>%
    select(Questions, Values, issue, KEY),
   
  # # Flagging if the HF lacks clinic health committee meeting minutes and at the same time the enumerator has checked it
  # qqc_data_approved$data %>%
  #   filter(grepl("None of the above", q1_2_5) & !is.na(q1_4) & q1_4 %notin% "None of the above") %>%
  #   mutate(
  #     issue = paste0("The HF lacks Health Committee Meeting Minutes but there is: ", q1_4),
  #     Questions = "q1_2_5 - q1_4",
  #     Values = paste0(q1_2_5, " - ", q1_4)
  #   ) %>%
  # #   select(Questions, Values, issue, KEY),
  # qqc_data_approved$data %>%
  #   filter(q1_9 %in% "Yes" & q1_2_5 %in% "NO (criteria NOT met]") %>%
  #   mutate(
  #     issue = "The HF lacks Health Committee Meeting Minutes but the enumerator has observed it in question q1_9!",
  #     Questions = "q1_9 - q1_2_5",
  #     Values = paste0(q1_9, " - ", q1_2_5)
  #   ) %>%
  #   select(Questions, Values, issue, KEY),

  # # Flagging if there is no list of staff and their phone numbers but at the same time there is a phone which includes the proper list of staffs and their phone numbers
  qqc_data_approved$data %>%
    filter(q1_6 %in% "Yes (all criteria met)" & q1_3 %in% "No") %>%
    mutate(
      issue = "There is no list of HF's staff and their phone numbers, but there is a dedicated mobile which includes these information!",
      Questions = "q1_6 - q1_3",
      Values = paste0(q1_6, " - ", q1_3)
    ) %>%
    select(Questions, Values, issue, KEY),

  # # Flagging if there is no documents related to HMIS but at the same time the HMIS reports are sent monthly
  qqc_data_approved$data %>%
    filter(q1_7 %in% "Yes" & grepl("None of the above", q1_2_2)) %>%
    mutate(
      issue = "There is no documents related to HMIS in q1_2_2 but the HMIS reports are sent monthly in q1_7!",
      Questions = "q1_7 - q1_2_2",
      Values = paste0(q1_7, " - ", q1_2_2)
    ) %>%
    select(Questions, Values, issue, KEY),
  

  
  ##### 2. HYGIENE
  # Flagging if any HF with no toilet reported
  # qqc_data_approved$data %>%
  #   filter(q2_3_1 %in% "No") %>%
  #   mutate(
  #     issue = "The HF doesn't have any toilet, investigation required!",
  #     Questions = "q2_3_1",
  #     Values = q2_3_1
  #   ) %>%
  #   select(Questions, Values, issue, KEY),
  
  # Flagging if the HF has no toilet but at the same time reported that they are well maintained
  qqc_data_approved$data %>%
    filter((q2_3_1 %in% "Yes" | q2_3_2_hg %in% "Yes") & q2_3_3 %in% "No") %>%
    mutate(
      issue = "The HF have at least two toilets (staff or patient), but the next question says No toilet facility!",
      Questions = "q2_3_1 - q2_3_2_hg - q2_3_3",
      Values = paste0(q2_3_1, " - ", q2_3_2_hg, " - ", q2_3_3)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if the HF has no toilet but at the same time reported that they are well maintained
  qqc_data_approved$data %>%
    filter((q2_3_1 %in% "No" & q2_3_2_hg %in% "No" & q2_3_3 %in% "No") & q2_3_4 %in% "Yes") %>%
    mutate(
      issue = "The HF doesn't have any toilet, but they are well-maintained!",
      Questions = "q2_3_1 - q2_3_2_hg - q2_3_3 - q2_3_4",
      Values = paste0(q2_3_1, " - ", q2_3_2_hg, " - ", q2_3_3, " - ", q2_3_4)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  qqc_data_approved$data %>%
    filter((q2_3_1 %in% "No" & q2_3_2_hg %in% "No" & q2_3_3 %in% "No") & q2_3_5 %in% "Yes") %>%
    mutate(
      issue = "The HF doesn't have any toilet, but they are well-maintained!",
      Questions = "q2_3_1 - q2_3_2_hg - q2_3_3 - q2_3_5",
      Values = paste0(q2_3_1, " - ", q2_3_2_hg, " - ", q2_3_3, " - ", q2_3_5)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  qqc_data_approved$data %>%
    filter((q2_3_1 %in% "No" & q2_3_2_hg %in% "No" & q2_3_3 %in% "No") & q2_3_6 %in% "Yes") %>%
    mutate(
      issue = "The HF doesn't have any toilet, but they are well-maintained!",
      Questions = "q2_3_1 - q2_3_2_hg - q2_3_3 - q2_3_6",
      Values = paste0(q2_3_1, " - ", q2_3_2_hg, " - ", q2_3_3, " - ", q2_3_6)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  ###### 3. CURATIVE CONSULTATIONS
  # Flagging if number of treated patients are less than 10 in 3 months
  qqc_data_approved$data %>%
    filter(q3_15_1 < 10) %>%
    mutate(
      issue = "Number of treated patients is less than 10 in 3 months, plz double-check!",
      Questions = "q3_15_1",
      Values = q3_15_1
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if number of 30 sampled treated patients exceed 30
  qqc_data_approved$data %>%
    filter(q3_15_2 > 30) %>%
    mutate(
      issue = "Number of 30 sampled treated patients exceed 30!",
      Questions = "q3_15_2",
      Values = q3_15_2
    ) %>%
    select(Questions, Values, issue, KEY),
  
  ###### 4. FAMILY PLANNING
  
  ###### 5. LABORATORY
  
  ##### 6. ESSENTIAL DRUGS MANAGEMENT
  
  # Flagging if there is no separate room for stock and pharmacy but reported that they any of the both measures have been meet
  qqc_data_approved$data %>%
    filter(q6_1_1 != "None of the above" & !is.na(q6_1_1) & q6_1 %in% "No") %>%
    mutate(
      issue = "The HF doesn't have sparate room for stock and pharmacy but reported in q6_1_1 any of both measures has been fulfilled!",
      Questions = "q6_1_1 - q6_1",
      Values = paste0(q6_1_1, " - ", q6_1)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if any of the 3 medicine names is duplicated
  qqc_data_approved$data %>%
    filter(Medicine1_name == Medicine2_name) %>%
    mutate(
      issue = "The medicine name is duplicate!",
      Questions = "Medicine1_name - Medicine2_name",
      Values = paste0(Medicine1_name, " - ", Medicine2_name)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if any of the 3 medicine names is duplicated
  qqc_data_approved$data %>%
    filter(Medicine1_name == Medicine3_name) %>%
    mutate(
      issue = "The medicine name is duplicate!",
      Questions = "Medicine1_name - Medicine3_name",
      Values = paste0(Medicine1_name, " - ", Medicine3_name)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if any of the 3 medicine names is duplicated
  qqc_data_approved$data %>%
    filter(Medicine2_name == Medicine3_name) %>%
    mutate(
      issue = "The medicine name is duplicate!",
      Questions = "Medicine2_name - Medicine3_name",
      Values = paste0(Medicine2_name, " - ", Medicine3_name)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if any of the count of medicine is the same but reported different in q6_3_1
  qqc_data_approved$data %>%
    filter(
      (Medicine1_Balance_in_the_stock_card == Medicine1_PC &
         Medicine2_Balance_in_the_stock_card == Medicine2_PC &
         Medicine3_Balance_in_the_stock_card == Medicine3_PC) &
        q6_3_1 != "No"
    ) %>%
    mutate(
      issue = "The medicine counts match the stock, but there is reported discrepancy in at least one of them according to q6_3_1",
      Questions = "Medicine1_PC/Balance - Medicine2_PC/Balance - Medicine3_PC/Balance - q6_3_1",
      Values = paste0(Medicine1_PC,"/", Medicine1_Balance_in_the_stock_card, " - ", Medicine2_PC,"/", Medicine2_Balance_in_the_stock_card, " - ", Medicine3_PC,"/", Medicine3_Balance_in_the_stock_card, " - ", q6_3_1)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if two of the count of medicine is the same but reported all different in q6_3_1
  qqc_data_approved$data %>%
    rowwise() %>% 
    filter(
      ((Medicine1_Balance_in_the_stock_card %in% Medicine1_PC &
          Medicine2_Balance_in_the_stock_card %in% Medicine2_PC) |
         
         (Medicine1_Balance_in_the_stock_card %in% Medicine1_PC &
            Medicine3_Balance_in_the_stock_card %in% Medicine3_PC) |
         
         (Medicine2_Balance_in_the_stock_card %in% Medicine2_PC &
            Medicine3_Balance_in_the_stock_card %in% Medicine3_PC)) &
        q6_3_1 %in% "Yes, there is discrepancy in all three selected medicines" # Fixed
    ) %>%
    mutate(
      issue = "At least two of the medicine counts are the same, but it has been reported that all of them are discrepant!",
      Questions = "Medicine1_PC/Balance - Medicine2_PC/Balance - Medicine3_PC/Balance - q6_3_1",
      Values = paste0(Medicine1_PC,"/", Medicine1_Balance_in_the_stock_card, " - ", Medicine2_PC,"/", Medicine2_Balance_in_the_stock_card, " - ", Medicine3_PC,"/", Medicine3_Balance_in_the_stock_card, " - ", q6_3_1)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if all of the counts are different but later selected that only one or two of them are discrepant
  qqc_data_approved$data %>%
    filter(
      Medicine1_Balance_in_the_stock_card != Medicine1_PC &
        Medicine2_Balance_in_the_stock_card != Medicine2_PC &
        Medicine3_Balance_in_the_stock_card != Medicine3_PC &
        q6_3_1 %notin% "Yes, there is discrepancy in all three selected medicines" # Fixed
    ) %>%
    mutate(
      issue = "All medicine counts are not matching, but it has been reported that only one or two of them are discrepant!",
      Questions = "Medicine1_PC/Balance - Medicine2_PC/Balance - Medicine3_PC/Balance - q6_3_1",
      Values = paste0(Medicine1_PC,"/", Medicine1_Balance_in_the_stock_card, " - ", Medicine2_PC,"/", Medicine2_Balance_in_the_stock_card, " - ", Medicine3_PC,"/", Medicine3_Balance_in_the_stock_card, " - ", q6_3_1)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if only one count is discrepant but later reported two or more are. 
  qqc_data_approved$data %>%
    rowwise() %>% 
    filter(
      ((Medicine1_Balance_in_the_stock_card != Medicine1_PC & Medicine2_Balance_in_the_stock_card %in% Medicine2_PC & Medicine3_Balance_in_the_stock_card %in% Medicine3_PC) |
         (Medicine1_Balance_in_the_stock_card %in% Medicine1_PC & Medicine2_Balance_in_the_stock_card != Medicine2_PC & Medicine3_Balance_in_the_stock_card %in% Medicine3_PC) |
         (Medicine1_Balance_in_the_stock_card %in% Medicine1_PC & Medicine2_Balance_in_the_stock_card %in% Medicine2_PC & Medicine3_Balance_in_the_stock_card != Medicine3_PC)) &
        q6_3_1 %notin% "Yes, there is discrepancy in one out of three selected medicines" # Fixed (Mergred 3 checks in one)  
    ) %>%
    mutate(
      issue = "Only one of this medicine count is not matching, but it has been reported that two or more of them are discrepant!",
      Questions = "Medicine1_PC/Balance - Medicine2_PC/Balance - Medicine3_PC/Balance - q6_3_1",
      Values = paste0(Medicine1_PC,"/", Medicine1_Balance_in_the_stock_card, " - ", Medicine2_PC,"/", Medicine2_Balance_in_the_stock_card, " - ", Medicine3_PC,"/", Medicine3_Balance_in_the_stock_card, " - ", q6_3_1)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  ###### 7. TRACER DRUGS
  
  ###### 8. MATERNITY
  # Flagging if soap or liquid soap is available but no water
  qqc_data_approved$data %>%
    filter(
      grepl("Soap AND hand cleanser available in the delivery room.", q8_1) &
        !grepl("A functioning water source or at least 20L available in the delivery room", q8_1) & !is.na(q8_1)
    ) %>%
    mutate(
      issue = "soap is available but there is no water source/water available!",
      Questions = "q8_1",
      Values = q8_1
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if water is not available in q8_1 but reported available in q8_3
  qqc_data_approved$data %>%
    filter(
      !grepl("A functioning water source or at least 20L available in the delivery room", q8_1) & !is.na(q8_1) &
        grepl("AND there is running water OR a container min 20L filled with water with a laddle", q8_3)
    ) %>%
    mutate(
      issue = "It's reported that Water is not available in q8_1 but reported available in q8_3!",
      Questions = "q8_1 - q8_3",
      Values = paste0(q8_1, " - ", q8_3)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if water is not available in q8_1 but reported available in q8_3
  qqc_data_approved$data %>%
    filter(q8_4_2 %in% "Yes" & !grepl("This separate area has a curtain between the bed and the door", q8_4_3) & !is.na(q8_4_3)) %>%
    mutate(
      issue = "In q8_4_2 it's confirmed that the delivery rooms are separated by curtain, but the same choices is not selected in q8_4_3!",
      Questions = "q8_4_2 - q8_4_3",
      Values = paste0(q8_4_2, " - ", q8_4_3)
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if confirmed the delivery bed has clean bed sheet but later reported that there are blood marks on it (not clean)
  qqc_data_approved$data %>%
    filter(
      grepl("This separate area - different from the delivery room has a separate space for admission, triage, and initial assessment of all women attending - contains a bed with a plasticized mattress \\(washable material\\) and a clean bed cover", q8_4_3) &
        !grepl("Delivery bed matrass has no signs of blood \\(clean\\)", q8_9) & !is.na(q8_9)
    ) %>%
    mutate(
      issue = "In q8_4_3 the delivery bed has clean cover but later in q8_9 it's not reported clean!",
      Questions = "q8_4_3 - q8_9",
      Values = paste0(q8_4_3, " - ", q8_9)
    ) %>%
    select(Questions, Values, issue, KEY),

  ###### 9. EPI AND PRE-SCHOOL CONSULTATION
  
  # Flagging if the thermometer is not functioning but the temperature is registered in the form.
  qqc_data_approved$data %>%
    filter(
      !grepl("The thermometer is tested and is functioning", q9_2) & !is.na(q9_2) &
        grepl("Presence of a fridge - temp form available, filled twice a day including the day of the visit", q9_2)
    ) %>%
    mutate(
      issue = "Temp form is filled twice but the response 'Thermometer is functioning tested and functioning' is not selected!", # Fixed
      Questions = "q9_2",
      Values = q9_2
    ) %>%
    select(Questions, Values, issue, KEY),
  
  # Flagging if the thermometer is not functioning but the temperature is set according on thermometer.
  qqc_data_approved$data %>%
    filter(
      !grepl("The thermometer is tested and is functioning", q9_2) & !is.na(q9_2) &
        grepl("Temperature between 2 and 8C also according to the thermometer", q9_2)
    ) %>%
    mutate(
      issue = "Thermometer is not functioning but the temperature is set according on thermometer!",
      Questions = "q9_2",
      Values = q9_2
    ) %>%
    select(Questions, Values, issue, KEY)
  
  ###### 10. ANTENATAL CARE
)


#### Flag Numeric values in Other/Numeric Questions
qqc_other_num_issues = rbind(
  flag_numeric_values(qqc_data_approved$data, qqc_tool_path, Tool="QQC")
)



## HF Level checks ---------------------------------------------------------------------------------
qqc_int_type <- c("1. General Management", "2. Hygiene and Sterilization", "3. Curative Consultation (OPD)", 
                  "4. Family Planning", "5. Laboratory", "6. Essential Medicine Management", 
                  "7. Tracer Medicine", "8. Maternity", "9. EPI and Pre-school Consultation", 
                  "10. Antenatal Care (ANC)", "11. Additional Questions")

qqc_interview_type <- qqc_data_approved$data %>% 
  group_by(HF_Name_based_on_Sample, HF_Code_based_on_sample) %>% 
  reframe(Total_Interview_Types = length(unique(Interview_Type_SV)),
          Collected_data = paste0(Interview_Type_SV, collapse = " & "),
          Missing_data = paste0(qqc_int_type[qqc_int_type %notin% Interview_Type_SV], collapse = " & "),
          # flag interview types collected more than once
          Duplicate_flag = any(duplicated(Interview_Type_SV)),
          # list which ones were duplicated
          Duplicated_types = paste0(unique(Interview_Type_SV[duplicated(Interview_Type_SV)]), collapse = " - ")
          ) %>% ungroup() %>% unique() 

# Chech for repeat sheet mismatches ----------------------------------------------------------------
QQC_count_mismatch <- rbind(
  # q1_4_1
  qqc_data_approved$q1_4_1 %>% count(KEY=PARENT_KEY, Sheet="q1_4_1", name="repeat_sheet_count", main_sheet_count="6", Question="Fixed_repeat_count"),
  # q1_4_2
    qqc_data_approved$q1_4_2 %>% count(KEY=PARENT_KEY, Sheet="q1_4_2", name="repeat_sheet_count", main_sheet_count="4", Question="Fixed_repeat_count"),
  # q1_4_3
    qqc_data_approved$q1_4_3 %>% count(KEY=PARENT_KEY, Sheet="q1_4_3", name="repeat_sheet_count", main_sheet_count="2", Question="Fixed_repeat_count"),
  # q1_4_4
    qqc_data_approved$q1_4_4 %>% count(KEY=PARENT_KEY, Sheet="q1_4_4", name="repeat_sheet_count", main_sheet_count="2", Question="Fixed_repeat_count"),
  # q1_9_1
    qqc_data_approved$q1_9_1 %>% count(KEY=PARENT_KEY, Sheet="q1_9_1", name="repeat_sheet_count", main_sheet_count="6", Question="Fixed_repeat_count"),
  # q1_9_2
  qqc_data_approved$q1_9_2 %>% count(KEY=PARENT_KEY, Sheet="q1_9_2", name="repeat_sheet_count", main_sheet_count="4", Question="Fixed_repeat_count"),
  # q1_9_3
  qqc_data_approved$q1_9_3 %>% count(KEY=PARENT_KEY, Sheet="q1_9_3", name="repeat_sheet_count", main_sheet_count="2", Question="Fixed_repeat_count"),
  # q3_15_1_Repeat
  qqc_data_approved$q3_15_1_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="q3_15_1_Repeat") %>%
    full_join(qqc_data_approved$data %>% select(main_sheet_count=q3_15_2, KEY) %>% 
                mutate(Sheet="q3_15_1_Repeat", Question="q3_15_2"), by=c("KEY", "Sheet"))
) #%>% 
  # relocate(repeat_sheet_count, .before = main_sheet_count) %>% 
  # rowwise() %>% 
  # filter(repeat_sheet_count %notin% main_sheet_count & 
  #          !(is.na(repeat_sheet_count) & main_sheet_count == 0) & 
  #          !(is.na(main_sheet_count) & is.na(repeat_sheet_count))) %>% 
  # mutate(Tool="QQC") 

no_photo_count <- c("data", "q1_2_1_Photos", "q1_2_2_Photos", "q1_2_3_Photos", 
                    "q1_2_4_Photos", "q1_2_5_Photos", "q2_3_Photos", "q2_3_2_hg_Photos", 
                    "q2_3_3_Photos", "q2_3_4_Photos", "q2_3_5_Photo_Re", #"q2_3_7_Photos", 
                    "q2_3_8_photos", "q2_5_1_Photos", "q3_23_Photos")

for(sheet in names(qqc_data_approved)[names(qqc_data_approved) %notin% no_photo_count]){
  # if(sheet %in% c("q1_4_1", "q1_4_2",	"q1_4_3", "q1_4_4",
  #                 "q1_9_1",	"q1_9_2", "q1_9_3")){
  #   col_count <- paste0(sheet, "_count")
  # } else {
  #   col_count <- paste0(sheet, "_Count")
  # }
  
  if(paste0(sheet, "_count") %in% names(qqc_data_approved$data)){
    col_count <- paste0(sheet, "_count")
  } else if(paste0(sheet, "_Count") %in% names(qqc_data_approved$data)) {
    col_count <- paste0(sheet, "_Count")
  } else{ 
    message("No repeat sheet count found for: ", sheet)
    break
  }
  
  QQC_count_mismatch <- rbind(
    QQC_count_mismatch,
    # sheet
    qqc_data_approved[[sheet]] %>%
      count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet=sheet) %>%
      full_join(qqc_data_approved$data %>% select(main_sheet_count=col_count, KEY) %>% 
                  mutate(Sheet=sheet, Question=col_count), by=c("KEY", "Sheet"))
  )
  
}

QQC_count_mismatch <- QQC_count_mismatch %>% 
  relocate(repeat_sheet_count, .before = main_sheet_count) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & 
           !(is.na(repeat_sheet_count) & main_sheet_count == 0) & 
           !(is.na(main_sheet_count) & is.na(repeat_sheet_count))) %>% 
  mutate(Tool="QQC") 
