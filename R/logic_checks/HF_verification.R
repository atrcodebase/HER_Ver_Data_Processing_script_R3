# double-check these
reporting_date_start <- "2025-04-01" # Equal to 12 Hamal 1404
reporting_date_end <- "2025-06-30" # Equal to 9 Saratan 1404


### HF Level Data Verification ---------------------------------------------------------------------
## Sheet Main 
hf_logical_issues <- bind_rows(
  #### Main Sheet 
  # # QA Comment: Missing days are recorded based on the number of Missing days in the attendance sheet, so the Total Missing Days can be more than the number of Working days .
  # HF_data_approved$data %>% 
  #   mutate(Total_Working_Days = as.numeric(Total_working_days_Month1)+as.numeric(Total_working_days_Month2)+as.numeric(Total_working_days_Month3),
  #          Total_Missing_Days = as.numeric(Total_Missing_days_Month1)+as.numeric(Total_Missing_days_Month2)+as.numeric(Total_Missing_days_Month3)) %>% 
  #   filter((Total_Working_Days %in% 0 & Total_Missing_Days %in% 0) | Total_Missing_Days > Total_Working_Days) %>% 
  #   mutate(issue = "Mismatch between Total working days and Missing days, please double check it.",
  #          Questions = "Total_Working_Days - Total_Missing_Days",
  #          Values = paste0(Total_Working_Days, " - ", Total_Missing_Days)) %>%
  #   select(Questions, Values, issue, KEY),
  
  # QA Comment from Dr. Sadat: Working days does not refer to working days of the specific employee but rather the total working days of that particular month and then how many days the employee missed
  # HF_data_approved$data %>% 
  #   mutate(working_missing_M1=as.numeric(Total_working_days_Month1)+as.numeric(Total_Missing_days_Month1),
  #          working_missing_M2=as.numeric(Total_working_days_Month2)+as.numeric(Total_Missing_days_Month2),
  #          working_missing_M3=as.numeric(Total_working_days_Month3)+as.numeric(Total_Missing_days_Month3)) %>% 
  #   filter(working_missing_M1 > 30 | working_missing_M2 > 31 | working_missing_M3 > 30) %>% 
  #   mutate(issue = "Total working & Missing days shouldn't exceed the total days in the month (Apr-June 30 & May 31)!",
  #          Questions = "working_missing_M1 - working_missing_M2 - working_missing_M3",
  #          Values = paste0(working_missing_M1, " - ", working_missing_M2, " - ", working_missing_M3)) %>%
  #   select(Questions, Values, issue, KEY),
  # QA Comment: Since the number of missing days is calculated based on each person who did not sign or blank, it can exceed total months.
  # HF_data_approved$data %>%
  #   filter(as.numeric(Total_Missing_days_Month1) > as.numeric(Total_working_days_Month1) | 
  #            as.numeric(Total_Missing_days_Month2) > as.numeric(Total_working_days_Month2) | 
  #            as.numeric(Total_Missing_days_Month3) > as.numeric(Total_working_days_Month3)) %>%
  #   mutate(issue = "Total working & Missing days shouldn't exceed the total days in the month (Apr-June 30 & May 31)!",
  #          Questions = "Total_Missing_days_Month1;Total_working_days_Month1 - Total_Missing_days_Month2;Total_working_days_Month2 - Total_Missing_days_Month3;Total_working_days_Month3",
  #          Values = paste0(Total_Missing_days_Month1, ";", Total_working_days_Month1, " - ", Total_Missing_days_Month2, ";", Total_working_days_Month2, " - ", Total_Missing_days_Month3, ";", Total_working_days_Month3)) %>%
  #   select(Questions, Values, issue, KEY),
  
  HF_data_approved$data %>% 
    filter(as.numeric(Total_working_days_Month1) > 30 | as.numeric(Total_working_days_Month3) > 30 | as.numeric(Total_working_days_Month2) > 31) %>% 
    mutate(issue = "April & June were 30 days & May was 31, please double-check!",
           Questions = "Total_working_days_Month1 - Total_working_days_Month2 - Total_working_days_Month3",
           Values = paste0(Total_working_days_Month1, " - ", Total_working_days_Month2, " - ", Total_working_days_Month3)) %>%
    select(Questions, Values, issue, KEY),
  
  HF_data_approved$data %>% 
    filter(as.numeric(Total_working_days_Month1) < 20 | as.numeric(Total_working_days_Month3) < 20 | as.numeric(Total_working_days_Month2) < 20) %>% 
    mutate(issue = "Working days for one of the months is below 20, please double-check!",
           Questions = "Total_working_days_Month1 - Total_working_days_Month2 - Total_working_days_Month3",
           Values = paste0(Total_working_days_Month1, " - ", Total_working_days_Month2, " - ", Total_working_days_Month3)) %>%
    select(Questions, Values, issue, KEY),

  HF_data_approved$data %>% 
    filter(Any_Staff_Scheduled_Overnight %in% "Yes" & How_Many_Nights_Schedule %in% 0) %>% 
    mutate(issue = "It is reported that at least 1 staff scheduled for at least 1 night for the past 10 days, but also reported 0 day for how many night scheduled Questions",
           Questions = "Any_Staff_Scheduled_Overnight",
           Values = paste0(Any_Staff_Scheduled_Overnight)) %>%
    select(Questions, Values, issue, KEY),
  
  HF_data_approved$data %>% 
    filter(How_Many_Members_On_Site > Hospital_Community_Board_Sum) %>% 
    mutate(issue = "The number of present board member is reported greater than total board members for the hospital",
           Questions = "How_Many_Members_On_Site - Hospital_Community_Board_Sum",
           Values = paste0(How_Many_Members_On_Site, " - ", Hospital_Community_Board_Sum)) %>%
    select(Questions, Values, issue, KEY),

  HF_data_approved$data %>% 
    filter((One_Member_On_Site_Today %in% "No" & How_Many_Members_On_Site > 0) |
             (One_Member_On_Site_Today %in% "Yes" & How_Many_Members_On_Site <= 0))%>% 
    mutate(issue = "Mismatch between members on site and total number of members!",
           Questions = "One_Member_On_Site_Today - How_Many_Members_On_Site",
           Values = paste0(One_Member_On_Site_Today, " - ", How_Many_Members_On_Site)) %>%
    select(Questions, Values, issue, KEY),
  
  HF_data_approved$data %>% 
    filter((as.numeric(total_personnel_male)+as.numeric(total_personnel_female)) != as.numeric(total_personnel_total)) %>% 
    mutate(issue = "Total Personnel does not match Male+Female Personnels!",
           Questions = "total_personnel_male - total_personnel_female - total_personnel_total",
           Values = paste0(total_personnel_male, " - ", total_personnel_female, " - ", total_personnel_total)) %>%
    select(Questions, Values, issue, KEY),
  
  		
  
  # Tool calculation not matching
  HF_data_approved$data %>% 
    filter((eq_rep %in% 203 & HF_Type %notin% c("Regional Hospital (RH)", "Provincial Hospital (PH)") | HF_Type %in% c("Regional Hospital (RH)", "Provincial Hospital (PH)") & eq_rep != 203) |
             (eq_rep %in% 79 & HF_Type %notin% c("District hospital (DH)", "Comprehensive Health Centre (CHC)",
                                                 "Comprehensive Health Centre (CHC +)", "Basic Health Centre (BHC)",
                                                 "Basic Health Centre (BHC+)", "Sub Health Centre (SHC)") |
                HF_Type %in% c("District hospital (DH)", "Comprehensive Health Centre (CHC)",
                               "Comprehensive Health Centre (CHC +)", "Basic Health Centre (BHC)",
                               "Basic Health Centre (BHC+)", "Sub Health Centre (SHC)") & eq_rep != 79)) %>% 
    mutate(issue = "eq_rep calculation: if(${HF_Type}='RH',203,if(${HF_Type}='PH',203,79))",
           Questions = "eq_rep - HF_Type",
           Values = paste0(eq_rep, " - ", HF_Type)) %>% 
    select(Questions, Values, issue, KEY),
  HF_data_approved$data %>% 
    filter((eq_indx %in% 79 & HF_Type %notin% c("Regional Hospital (RH)", "Provincial Hospital (PH)") | HF_Type %in% c("Regional Hospital (RH)", "Provincial Hospital (PH)") & eq_indx != 79) |
             (eq_indx %in% 0 & HF_Type %notin% c("District hospital (DH)", "Comprehensive Health Centre (CHC)",
                                                 "Comprehensive Health Centre (CHC +)", "Basic Health Centre (BHC)",
                                                 "Basic Health Centre (BHC+)", "Sub Health Centre (SHC)") |
                HF_Type %in% c("District hospital (DH)", "Comprehensive Health Centre (CHC)",
                               "Comprehensive Health Centre (CHC +)", "Basic Health Centre (BHC)",
                               "Basic Health Centre (BHC+)", "Sub Health Centre (SHC)") & eq_indx != 0)) %>% 
    mutate(issue = "eq_indx calculation: if(${HF_Type}='RH',79,if(${HF_Type}='PH',79,0))",
           Questions = "eq_indx - HF_Type",
           Values = paste0(eq_indx, " - ", HF_Type)) %>% 
    select(Questions, Values, issue, KEY),

  ##### Personnel
  HF_data_approved$Personnel %>% 
    left_join(
      HF_data_approved$data %>% 
        mutate(Total_Working_Days = as.numeric(Total_working_days_Month1)+as.numeric(Total_working_days_Month2)+as.numeric(Total_working_days_Month3)) %>% 
        select(Total_Working_Days, KEY), by="KEY") %>% 
    filter(Attendance_Complete %in% "Yes" & Total_Working_Days %in% 0) %>% 
    mutate(issue = "It is reported that attendance sheet is complete but the total number of working days is repoted 0",
           Questions = "Attendance_Complete - Total_Working_Days",
           Values = paste0(Attendance_Complete, " - ", Total_Working_Days)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  #### Absent_Days
  HF_data_approved$Absent_Days %>% 
    filter(Staff_N != Staff_Absentees_count) %>% 
    mutate(issue = "They should be the same",
           Questions = "Staff_N - Staff_Absentees_count",
           Values = paste0(Staff_N, " - ", Staff_Absentees_count)) %>% 
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  #### Staff_Absentees
  HF_data_approved$Staff_Absentees %>% 
    filter(Absent_Days_Month1 == 31 | Absent_Days_Month3 == 31) %>% 
    mutate(issue = "April and June were 30 days!",
           Questions = "Absent_Days_Month1 - Absent_Days_Month3",
           Values = paste0(Absent_Days_Month1, " - ", Absent_Days_Month3)) %>% 
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  #### Drugs
  HF_data_approved$Drugs %>%
    group_by(KEY=PARENT_KEY) %>% 
    reframe(all_medicines_calc=paste0(Drug_Name, collapse = ";")) %>% 
    left_join(
      HF_data_approved$data %>% select(all_medicines, KEY), by="KEY"
    ) %>% 
    filter(all_medicines_calc!=all_medicines) %>% 
    mutate(issue="Drugs difference in the main vs Drugs sheet!",
           Questions = "all_medicines_calc - all_medicines",
           Values = paste0(all_medicines_calc, " - ", all_medicines)) %>%
    select(Questions, Values, issue, KEY),
  
  HF_data_approved$Drugs %>%
    filter(Course_Of_Drug_Available %in% "Yes") %>% 
    group_by(KEY=PARENT_KEY) %>% 
    reframe(available_medicines_calc=paste0(Drug_Name, collapse = ", ")) %>% 
    left_join(
      HF_data_approved$data %>% select(available_medicines, KEY), by="KEY"
    ) %>% filter(available_medicines_calc!=available_medicines) %>% 
    mutate(issue="Drugs difference in the main vs Drugs sheet!",
           Questions = "available_medicines_calc - available_medicines",
           Values = paste0(available_medicines_calc, " - ", available_medicines)) %>%
    select(Questions, Values, issue, KEY),
  
  ## Medicine_Expiration
  HF_data_approved$Medicine_Expiration %>% 
    filter(Medicine_Expired %in% 0 & Medicine_Not_Expired %in% 0 & Medicine_Unclear %in% 0) %>% 
    mutate(issue = paste0("It is reported that one of drug ", Drug_Name_Available ," course is available but all Expired, Not Expired and Unclear Expiration Date Drug number are reported 0"),
           Questions = "Medicine_Expired - Medicine_Not_Expired - Medicine_Unclear",
           Values = paste0(Medicine_Expired, " - ", Medicine_Not_Expired, " - ", Medicine_Unclear)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  #### Drug Availability Reporting
  HF_data_approved$Drug_Availability_Reporting_... %>% 
    filter(Was_Drug_Out_Of_Stock_RP %in% "Yes" & (How_Many_Times_Out_Of_Stock_RP %in% 0 | is.na(How_Many_Times_Out_Of_Stock_RP))) %>% 
    mutate(issue = "It is reported that the Drug was out of stock during reporting period but also the number of times out of stock is reported 0",
           Questions = "Was_Drug_Out_Of_Stock_RP - How_Many_Times_Out_Of_Stock_RP",
           Values = paste0(Was_Drug_Out_Of_Stock_RP, " - ", How_Many_Times_Out_Of_Stock_RP)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  HF_data_approved$Drug_Availability_Reporting_... %>%
    filter(How_Many_Times_Out_Of_Stock_RP != Drug_Out_Of_Stock_count) %>%
    mutate(issue = "They should be the same",
           Questions = "How_Many_Times_Out_Of_Stock_RP - Drug_Out_Of_Stock_count",
           Values = paste0(How_Many_Times_Out_Of_Stock_RP, " - ", Drug_Out_Of_Stock_count)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  #### Drug_Out_Of_Stock
  HF_data_approved$Drug_Out_Of_Stock %>% 
    mutate(Days_Out_Of_Stock_cal = as.numeric(as.Date(Date_In_Stock_RP)-as.Date(Last_Date_Out_Of_Stock_RP))) %>% 
    rowwise() %>% 
    filter(Days_Out_Of_Stock_cal %notin% How_Many_Days_Out_Of_Stock_RP) %>% 
    mutate(issue="Number of days out of stock is not correct!",
           Questions = "Days_Out_Of_Stock_cal - How_Many_Days_Out_Of_Stock_RP",
           Values = paste0(Days_Out_Of_Stock_cal, " - ", How_Many_Days_Out_Of_Stock_RP)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  #### List of Consumable RP
  HF_data_approved$List_Of_Consumables_RP %>% 
    filter(Was_Consumable_Out_Of_Stock_RP %in% "Yes" & Times_Were_Consumable_Out_Of_Stock_RP %in% 0) %>% 
    mutate(issue = "It is reported that Consumable RP was out of stock during Reporting period, but the number of times reported 0",
           Questions = "Was_Consumable_Out_Of_Stock_RP - Times_Were_Consumable_Out_Of_Stock_RP",
           Values = paste0(Was_Consumable_Out_Of_Stock_RP, " - ", Times_Were_Consumable_Out_Of_Stock_RP)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  HF_data_approved$List_Of_Consumables_RP %>%
    filter(Times_Were_Consumable_Out_Of_Stock_RP != Consumable_Out_Of_Stock_RP_count) %>%
    mutate(issue = "They should be the same",
           Questions = "Times_Were_Consumable_Out_Of_Stock_RP - Consumable_Out_Of_Stock_RP_count",
           Values = paste0(Times_Were_Consumable_Out_Of_Stock_RP, " - ", Consumable_Out_Of_Stock_RP_count)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  		
  #### Consumable_Out_Of_Stock_RP
  HF_data_approved$Consumable_Out_Of_Stock_RP %>% 
    mutate(Days_Out_Of_Stock_cal = as.numeric(as.Date(Date_Were_Consumable_In_Stock_RP)-as.Date(Last_Date_Were_Consumable_Out_Of_Stock_RP))) %>% 
    rowwise() %>% 
    filter(Days_Out_Of_Stock_cal %notin% Days_Were_Consumable_Out_Of_Stock_RP) %>%
    mutate(issue="Number of days out of stock is not correct!",
           Questions = "Days_Out_Of_Stock_cal - Days_Were_Consumable_Out_Of_Stock_RP",
           Values = paste0(Days_Out_Of_Stock_cal, " - ", Days_Were_Consumable_Out_Of_Stock_RP)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  ## List of Equipment
  HF_data_approved$List_Of_Equipment %>% 
    filter(At_Least_One_Equipment_Available %in% "Yes" & How_Many_Available %in% 0) %>% 
    mutate(issue = "Is is reported that at least one Equipment of the type is available, but for how many available Questions it is reported 0",
           Questions = "At_Least_One_Equipment_Available - How_Many_Available",
           Values = paste0(At_Least_One_Equipment_Available, " - ", How_Many_Available)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  HF_data_approved$List_Of_Equipment %>% 
    filter(as.numeric(How_Many_Equipment_Not_Functional) > as.numeric(How_Many_Available)) %>% 
    mutate(issue = "The number of non-funcational equipment is greather than total available",
           Questions = "How_Many_Equipment_Not_Functional - How_Many_Available",
           Values = paste0(How_Many_Equipment_Not_Functional, " - ", How_Many_Available)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique),
  
  ## Hospital Governanace
  HF_data_approved$Hospital_Governance %>% 
    filter(as.numeric(Number_Of_Meeting_Minutes_In_Month) > as.numeric(Number_Of_Meetings_Conducted_In_Month)) %>% 
    mutate(issue = "The number of meeting minutes is reported greater than conducted meetings in the month",
           Questions = "Number_Of_Meeting_Minutes_In_Month - Number_Of_Meetings_Conducted_In_Month",
           Values = paste0(Number_Of_Meeting_Minutes_In_Month, " - ", Number_Of_Meetings_Conducted_In_Month)) %>%
    select(Questions, Values, issue, KEY=KEY_Unique)
)
  
# Check the repeat sheet counts vs main values
sub_cols <- c("Control_Temperature", "Windows_Can_Open", "Sunlight_cannot_enter", 
  "Area_Free_Moisture", "Old_Storage_available", "Filled_Tempreature_Chart", 
  "Medicine_Not_Stored", "Medicine_Stored_Systematic", "Medicine_Stored_First_Expired_First", 
  "Evidence_Of_Pests", "Temperature_Chart") #"Register_Lab_Department_Available") # Count not added yet in the data
# Add checks for these two as well once data is added
# Register_Lab_Department_Available_Count
# Is_There_A_Specific_Room_For_X_Ray

for(col_i in sub_cols){
  hf_logical_issues <- rbind(
    hf_logical_issues,
    HF_data_approved$data %>% 
      filter((get(col_i) %in% "Yes" & get(paste0(col_i, "_Count")) < 1) | (get(col_i) %in% "No" & get(paste0(col_i, "_Count")) > 0)) %>% 
      mutate(issue = "The question response and repeat sheet count does not match!",
             Questions = paste0(col_i, " - ", paste0(col_i, "_Count")),
             Values = paste0(get(col_i), " - ", get(paste0(col_i, "_Count")))) %>%
      select(Questions, Values, issue, KEY=KEY_Unique)
  )
}

## HF Level checks ---------------------------------------------------------------------------------
hf_int_types <- c("Personnel (section 1)", "Medicine (section 2)", "Consumable (section 3)", 
                  "Equipments (section 4)", "Lab Test (section 5)", "Other than P4P (section 6)", 
                  "Hospital governance (section 7)")

hf_interview_type <- HF_data_approved$data %>% 
  group_by(HF_Name_based_on_Sample, HF_Code_based_on_sample) %>% 
  reframe(Total_Interview_Types = length(unique(Interview_Type_SV)),
          Collected_data = paste0(Interview_Type_SV, collapse = " & "),
          Missing_data = paste0(hf_int_types[hf_int_types %notin% Interview_Type_SV], collapse = " & "),
          # flag interview types collected more than once
          Duplicate_flag = any(duplicated(Interview_Type_SV)),
          # list which ones were duplicated
          Duplicated_types = paste0(unique(Interview_Type_SV[duplicated(Interview_Type_SV)]), collapse = " - ")
          
          ) %>% ungroup() %>% unique() 

### Staff Number
shc_data <- read.csv("input/sample/shc_data.csv")
shc_data <- shc_data %>% 
  select(Staff_Type=Staff, RH:SHC) %>% 
  filter(!is.na(Staff_Type)) %>% 
  pivot_longer(-Staff_Type, names_to = "HF_Type", values_to = "Staff_N_sample")

staff_inconsistency <- HF_data_approved$Absent_Days %>% 
  mutate(HF_Type = case_when(
    HF_Type %in% "Regional Hospital (RH)" ~ "RH",
    HF_Type %in% "Provincial Hospital (PH)" ~ "PH",
    HF_Type %in% "District hospital (DH)" ~ "DH",
    HF_Type %in% "Comprehensive Health Centre (CHC)" ~ "CHC",
    HF_Type %in% "Comprehensive Health Centre (CHC +)" ~ "CHC+",
    HF_Type %in% "Basic Health Centre (BHC)" ~ "BHC",
    HF_Type %in% "Basic Health Centre (BHC +)" ~ "BHC+",
    HF_Type %in% "Sub Health Centre (SHC)" ~ "SHC",
    TRUE ~ HF_Type
  )) %>% 
  select(HF_Type, Staff_Type, Staff_N_data=Staff_N) %>% unique() %>% 
  full_join(shc_data) %>% 
  filter(Staff_N_data!=Staff_N_sample) %>% 
  mutate(Issue="Number of staff is different from the sample for this HF_Type!")

# Chech for repeat sheet mismatches ----------------------------------------------------------------
HF_count_mismatch <- rbind(
  # Personnel
  HF_data_approved$Personnel %>% count(KEY=PARENT_KEY, Sheet="Personnel", name="repeat_sheet_count", main_sheet_count="4", Question="Fixed_repeat_count"),
  HF_data_approved$Personnel %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Personnel") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Personnel_count, KEY) %>% 
                mutate(Sheet="Personnel", Question="Personnel_count"), by=c("KEY", "Sheet")),
  
  # Absent_Days
  HF_data_approved$Absent_Days %>% count(KEY=PARENT_KEY, Sheet="Absent_Days", name="repeat_sheet_count", main_sheet_count="30", Question="Fixed_repeat_count"),
  HF_data_approved$Absent_Days %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Absent_Days") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Absent_Days_count, KEY) %>% 
                mutate(Sheet="Absent_Days", Question="Absent_Days_count"), by=c("KEY", "Sheet")),
  
  # Staff_Absentees
  HF_data_approved$Staff_Absentees %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Staff_Absentees") %>%
    full_join(HF_data_approved$Absent_Days %>% select(main_sheet_count=Staff_N, KEY=KEY_Unique) %>% 
                mutate(Sheet="Staff_Absentees", Question="Staff_N"), by=c("KEY", "Sheet")),
  
  # Drugs
  HF_data_approved$Drugs %>% count(KEY=PARENT_KEY, Sheet="Drugs", name="repeat_sheet_count", main_sheet_count="20", Question="Fixed_repeat_count"),
  HF_data_approved$Drugs %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Drugs") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Drugs_count, KEY) %>% 
                mutate(Sheet="Drugs", Question="Drugs_count"), by=c("KEY", "Sheet")),
  
  # Drug_Expiration
  HF_data_approved$Medicine_Expiration %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Drug_Expiration") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=n_available_drug, KEY) %>% 
                mutate(Sheet="Drug_Expiration", Question="n_available_drug"), by=c("KEY", "Sheet")),
  
  # Drug_Availability_Reporting_...
  HF_data_approved$Drug_Availability_Reporting_... %>% count(KEY=PARENT_KEY, Sheet="Drug_Availability_Reporting_...", name="repeat_sheet_count", main_sheet_count="20", Question="Fixed_repeat_count"),
  HF_data_approved$Drug_Availability_Reporting_... %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Drug_Availability_Reporting_...") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Drug_Availability_Reporting_Period_count, KEY) %>% 
                mutate(Sheet="Drug_Availability_Reporting_...", Question="Drugs_count"), by=c("KEY", "Sheet")),
  
  # Drug_Out_Of_Stock
  HF_data_approved$Drug_Out_Of_Stock %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Drug_Out_Of_Stock") %>%
    full_join(HF_data_approved$Drug_Availability_Reporting_... %>% select(main_sheet_count=How_Many_Times_Out_Of_Stock_RP, KEY=KEY_Unique) %>% 
                mutate(Sheet="Drug_Out_Of_Stock", Question="How_Many_Times_Out_Of_Stock_RP"), by=c("KEY", "Sheet")),
  
  # List_Of_Consumables_DV
  HF_data_approved$List_Of_Consumables_DV %>% count(KEY=PARENT_KEY, Sheet="List_Of_Consumables_DV", name="repeat_sheet_count", main_sheet_count="46", Question="Fixed_repeat_count"),
  HF_data_approved$List_Of_Consumables_DV %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="List_Of_Consumables_DV") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=List_Of_Consumables_DV_count, KEY) %>% 
                mutate(Sheet="List_Of_Consumables_DV", Question="Drugs_count"), by=c("KEY", "Sheet")),
  
  # List_Of_Consumables_RP
  HF_data_approved$List_Of_Consumables_RP %>% count(KEY=PARENT_KEY, Sheet="List_Of_Consumables_RP", name="repeat_sheet_count", main_sheet_count="46", Question="Fixed_repeat_count"),
  HF_data_approved$List_Of_Consumables_RP %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="List_Of_Consumables_RP") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=List_Of_Consumables_RP_count, KEY) %>% 
                mutate(Sheet="List_Of_Consumables_RP", Question="Drugs_count"), by=c("KEY", "Sheet")),
  
  # Consumable_Out_Of_Stock_RP
  HF_data_approved$Consumable_Out_Of_Stock_RP %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Consumable_Out_Of_Stock_RP") %>%
    full_join(HF_data_approved$List_Of_Consumables_RP %>% select(main_sheet_count=Times_Were_Consumable_Out_Of_Stock_RP, KEY=KEY_Unique) %>% 
                mutate(Sheet="Consumable_Out_Of_Stock_RP", Question="Times_Were_Consumable_Out_Of_Stock_RP"), by=c("KEY", "Sheet")),
  
  # Control_Temperature_Repeat
  HF_data_approved$Control_Temperature_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Control_Temperature_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Control_Temperature_Count, KEY) %>% 
                mutate(Sheet="Control_Temperature_Repeat", Question="Control_Temperature_Count"), by=c("KEY", "Sheet")),
  
  # Windows_Can_Open_Repeat
  HF_data_approved$Windows_Can_Open_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Windows_Can_Open_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Windows_Can_Open_Count, KEY) %>% 
                mutate(Sheet="Windows_Can_Open_Repeat", Question="Windows_Can_Open_Count"), by=c("KEY", "Sheet")),
  
  # Sunlight_cannot_enter_Repeat
  HF_data_approved$Sunlight_cannot_enter_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Sunlight_cannot_enter_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Sunlight_cannot_enter_Count, KEY) %>% 
                mutate(Sheet="Sunlight_cannot_enter_Repeat", Question="Sunlight_cannot_enter_Count"), by=c("KEY", "Sheet")),
  
  # Area_Free_Moisture_Repeat
  HF_data_approved$Area_Free_Moisture_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Area_Free_Moisture_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Area_Free_Moisture_Count, KEY) %>% 
                mutate(Sheet="Area_Free_Moisture_Repeat", Question="Area_Free_Moisture_Count"), by=c("KEY", "Sheet")),
  
  # Old_Storage_available_Repeat
  HF_data_approved$Old_Storage_available_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Old_Storage_available_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Old_Storage_available_Count, KEY) %>% 
                mutate(Sheet="Old_Storage_available_Repeat", Question="Old_Storage_available_Count"), by=c("KEY", "Sheet")),

  # Filled_Tempreature_Chart_Repeat
  HF_data_approved$Filled_Tempreature_Chart_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Filled_Tempreature_Chart_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Filled_Tempreature_Chart_Count, KEY) %>% 
                mutate(Sheet="Filled_Tempreature_Chart_Repeat", Question="Filled_Tempreature_Chart_Count"), by=c("KEY", "Sheet")),
  
  # Medicine_Not_Stored_Repeat
  HF_data_approved$Medicine_Not_Stored_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Medicine_Not_Stored_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Medicine_Not_Stored_Count, KEY) %>% 
                mutate(Sheet="Medicine_Not_Stored_Repeat", Question="Medicine_Not_Stored_Count"), by=c("KEY", "Sheet")),
  
  # Medicine_Stored_Systematic_R...
  HF_data_approved$Medicine_Stored_Systematic_R... %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Medicine_Stored_Systematic_R...") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Medicine_Stored_Systematic_Count, KEY) %>% 
                mutate(Sheet="Medicine_Stored_Systematic_R...", Question="Medicine_Stored_Systematic_Count"), by=c("KEY", "Sheet")),

  # Medicine_Stored_First_Expire...
  HF_data_approved$Medicine_Stored_First_Expire... %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Medicine_Stored_First_Expire...") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Medicine_Stored_First_Expired_First_Count, KEY) %>% 
                mutate(Sheet="Medicine_Stored_First_Expire...", Question="Medicine_Stored_First_Expired_First_Count"), by=c("KEY", "Sheet")),
  
  # Evidence_Of_Pests_Repeat
  HF_data_approved$Evidence_Of_Pests_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Evidence_Of_Pests_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Evidence_Of_Pests_Count, KEY) %>%
                mutate(Sheet="Evidence_Of_Pests_Repeat", Question="Evidence_Of_Pests_Count"), by=c("KEY", "Sheet")),
  
  # Temperature_Chart_Repeat
  HF_data_approved$Temperature_Chart_Repeat %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Temperature_Chart_Repeat") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Temperature_Chart_Count, KEY) %>% 
                mutate(Sheet="Temperature_Chart_Repeat", Question="Temperature_Chart_Count"), by=c("KEY", "Sheet")),
  
  # List_Of_Equipment
  HF_data_approved$List_Of_Equipment %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="List_Of_Equipment") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=eq_rep, KEY) %>% 
                mutate(Sheet="List_Of_Equipment", Question="eq_rep"), by=c("KEY", "Sheet")),
  # Lab_Register_Repeat
  # HF_data_approved$Lab_Register_Repeat %>%
  #   count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Lab_Register_Repeat") %>%
  #   full_join(HF_data_approved$data %>% select(main_sheet_count=Register_Lab_Department_Available_Count, KEY) %>%
  #               mutate(Sheet="Lab_Register_Repeat", Question="Register_Lab_Department_Available_Count"), by=c("KEY", "Sheet")),
  # Lab
  HF_data_approved$Lab %>% count(KEY=PARENT_KEY, Sheet="Lab", name="repeat_sheet_count", main_sheet_count="44", Question="Fixed_repeat_count"),
  HF_data_approved$Lab %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Lab") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Lab_count, KEY) %>%
                mutate(Sheet="Lab", Question="Lab_count"), by=c("KEY", "Sheet")),
  
  # Test_Component_Details
  HF_data_approved$Test_Component_Details %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Test_Component_Details") %>%
    full_join(HF_data_approved$Lab %>% select(main_sheet_count=Test_Component_Details_count, KEY=KEY_Unique) %>%
                mutate(Sheet="Test_Component_Details", Question="Test_Component_Details_count"), by=c("KEY", "Sheet")),
  HF_data_approved$Test_Component_Details %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Test_Component_Details") %>%
    full_join(HF_data_approved$Lab %>% select(main_sheet_count=subtests, KEY=KEY_Unique) %>%
                mutate(Sheet="Test_Component_Details", Question="subtests"), by=c("KEY", "Sheet")),
  
  # x_ray_room
  # HF_data_approved$x_ray_room %>%
  #   count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="x_ray_room") %>%
  #   full_join(HF_data_approved$data %>% select(main_sheet_count=x_ray_room_Count, KEY) %>% 
  #               mutate(Sheet="x_ray_room", Question="x_ray_room_Count"), by=c("KEY", "Sheet")),
  # 
  # Hospital_Governance
  HF_data_approved$Hospital_Governance %>% count(KEY=PARENT_KEY, Sheet="Hospital_Governance", name="repeat_sheet_count", main_sheet_count="3", Question="Fixed_repeat_count"),
  HF_data_approved$Hospital_Governance %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Hospital_Governance") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=Hospital_Governance_count, KEY) %>% 
                mutate(Sheet="Hospital_Governance", Question="Hospital_Governance_count"), by=c("KEY", "Sheet")),
  
  # HCB_Members
  HF_data_approved$HCB_Members %>% count(KEY=PARENT_KEY, Sheet="HCB_Members", name="repeat_sheet_count", main_sheet_count="2", Question="Fixed_repeat_count"),
  HF_data_approved$HCB_Members %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="HCB_Members") %>%
    full_join(HF_data_approved$data %>% select(main_sheet_count=HCB_Members_count, KEY) %>% 
                mutate(Sheet="HCB_Members", Question="HCB_Members_count"), by=c("KEY", "Sheet"))
  
  ) %>% 
  relocate(repeat_sheet_count, .before = main_sheet_count) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & 
           !(is.na(repeat_sheet_count) & main_sheet_count == 0) & 
           !(is.na(main_sheet_count) & is.na(repeat_sheet_count))) %>% 
  mutate(Tool="HF_Verification") 


