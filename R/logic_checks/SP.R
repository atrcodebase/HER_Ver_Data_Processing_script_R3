sp_logical_issues <- rbind(
  # 
  sp_data_approved$Personnel %>% 
    filter(as.numeric(Total_Working_Days) > 31) %>% 
    mutate(issue = "Incorrect working days",
           Questions = "Total_Working_Days",
           Values = Total_Working_Days) %>% 
    select(Questions, Values, issue, KEY),
  # 
  sp_data_approved$Personnel %>% 
    filter(as.numeric(Missing_Days) > as.numeric(Total_Working_Days)) %>% 
    mutate(issue = "Missing days shouldn't be more than working days",
           Questions = "Missing_Days - Total_Working_Days",
           Values = paste0(Missing_Days, " - ", Total_Working_Days)) %>% 
    select(Questions, Values, issue, KEY),
  #
  sp_data_approved$Absent_Days %>% 
    filter(as.numeric(Absent_Days_Month1) > 30 | as.numeric(Absent_Days_Month2) > 31 | as.numeric(Absent_Days_Month3) > 30) %>% 
    mutate(issue = "Absentees shouldn't be more than 31",
           Questions = "Absent_Days_Month1 - Absent_Days_Month2 - Absent_Days_Month3",
           Values = paste0(Absent_Days_Month1, " - ", Absent_Days_Month2, " - ", Absent_Days_Month3)) %>% 
    select(Questions, Values, issue, KEY)
)

#### Flag Numeric values in Other/Numeric Questions
sp_other_num_issues = rbind(
  flag_numeric_values(sp_data_approved$data, sp_tool_path, Tool="SP"),
  flag_numeric_values(sp_data_approved$Personnel, sp_tool_path, Tool="SP"),
  flag_numeric_values(sp_data_approved$Absent_Days, sp_tool_path, Tool="SP")
)

## HF Level checks ---------------------------------------------------------------------------------
sampled_sps <- read_excel("input/sample/Sampled_SPs_vs_UNICEF_list.xlsx")
# sampled_sps <- sampled_sps$Sampled_SPs %>% unique()
# sp_data_approved$data 
  

sp_interviews <- sp_data_approved$data %>% 
  mutate(
    SP_Name_based_on_sample = case_when(
      SP_Name_Sample_backup %in%  "AGA KHAN FOUNDATION AFGHANISTAN (AKF)" ~ "AKF", 
      SP_Name_Sample_backup %in% "BAKHTAR DEVELOPMENT NETWORK (BDN)" ~ "BDN", 
      SP_Name_Sample_backup %in% "CARE OF AFGHAN FAMILIES (CAF)" ~ "CAF", 
      SP_Name_Sample_backup %in% "AGENCY FOR ASSISTANCE AND DEVELOPMENT OF AFGHANISTAN AADA (AADA)" ~ "AADA", 
      SP_Name_Sample_backup %in% "SOLIDARITY FOR AFGHAN FAMILIES (SAF)" ~ "SAF", 
      SP_Name_Sample_backup %in% "ORGANIZATION FOR HEALTH PROMOTION AND MANAGEMENT (OHPM)" ~ "OHPM", 
      SP_Name_Sample_backup %in% "HEALTH NET TPO (HEALTH NET)" ~ "HEALTH NET", 
      SP_Name_Sample_backup %in% "JUST FOR AFGHAN CAPACITY AND KNOWLEDGE (JACK)" ~ "JACK", 
      SP_Name_Sample_backup %in% "SWEDISH COMMITTEE FOR AFGHANISTAN (SCA)" ~ "ARSDO", 
      SP_Name_Sample_backup %in% "RELIEF INTERNATIONAL-MEDICAL REFRESHER COURSES FOR AFGHANS (RI-MRCA)" ~ "RI-MRCA", 
      SP_Name_Sample_backup %in% "MEDICAL MANAGEMENT AND RESEARCH COURSES FOR AFGHANISTAN (MMRCA)" ~ "MMRCA", 
      TRUE ~ SP_Name_Sample_backup
    )) %>% 
  group_by(Province, #SP_Name_Updated=SP_Name_based_on_sample,
           SP_Name_based_on_sample) %>% 
  reframe(Collected_data = paste0(SP_Name_based_on_sample, collapse = " & "),
          # Missing_data = paste0(sampled_sps[service_type %notin% SP_Name_based_on_sample], collapse = " & "),
          # flag interview types collected more than once
          Duplicate_flag = any(duplicated(SP_Name_based_on_sample)),
          # list which ones were duplicated
          Duplicated_types = paste0(unique(SP_Name_based_on_sample[duplicated(SP_Name_based_on_sample)]), collapse = " - ")) %>% 
  ungroup() %>% 
  mutate(Missing_data = paste0(sampled_sps$Sampled_SPs[sampled_sps$Sampled_SPs %notin% SP_Name_based_on_sample], collapse = " & "))
          
  # 
  # reframe(Total_Service_Types = length(unique(Type_of_service_general)),
  #         Collected_data = paste0(Type_of_service_general, collapse = " & "),
  #         Missing_data = paste0(service_type[service_type %notin% Type_of_service_general], collapse = " & "),
  #         # flag interview types collected more than once
  #         Duplicate_flag = any(duplicated(Type_of_service_general)),
  #         # list which ones were duplicated
  #         Duplicated_types = paste0(unique(Type_of_service_general[duplicated(Type_of_service_general)]), collapse = " - ")
  #         
  # ) %>% ungroup() %>% unique() 


# Chech for repeat sheet mismatches ----------------------------------------------------------------
sp_count_mismatch <- rbind(
  # Personnel
  sp_data_approved$Personnel %>% count(KEY=PARENT_KEY, Sheet="Personnel", name="repeat_sheet_count", main_sheet_count="4", Question="Fixed_repeat_count"),
  sp_data_approved$Personnel %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Personnel") %>%
    full_join(sp_data_approved$data %>% select(main_sheet_count=Personnel_count, KEY) %>% 
                mutate(Sheet="Personnel", Question="Personnel_count"), by=c("KEY", "Sheet")),
  
  # Absent_Days
  sp_data_approved$Absent_Days %>% count(KEY=PARENT_KEY, Sheet="Absent_Days", name="repeat_sheet_count", main_sheet_count="9", Question="Fixed_repeat_count"),
  sp_data_approved$Absent_Days %>%
    count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Absent_Days") %>%
    full_join(sp_data_approved$data %>% select(main_sheet_count=Absent_Days_count, KEY) %>% 
                mutate(Sheet="Absent_Days", Question="Absent_Days_count"), by=c("KEY", "Sheet"))
) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & !(is.na(repeat_sheet_count) & main_sheet_count == 0)) %>% 
  mutate(Tool="SP")

