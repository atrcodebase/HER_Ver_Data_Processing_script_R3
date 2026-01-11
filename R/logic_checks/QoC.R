## QoC - Interview with Health Workers -------------------------------------------------------------
qoc_logical_issues <- plyr::rbind.fill(
  #### Data
  # Type of visit issue
  qoc_data_approved$data %>% 
    filter(Type_Of_Visit %in% "Follow up visit" & is.na(If_not_a_first_Site_Visit_state_Original_Site_Visit_ID) |
             Type_Of_Visit %in% "First visit" & !is.na(If_not_a_first_Site_Visit_state_Original_Site_Visit_ID)) %>% 
    mutate(issue = "Inconsistent!",
           Questions = "Type_Of_Visit - If_not_a_first_Site_Visit_state_Original_Site_Visit_ID",
           Values = paste0(Type_Of_Visit, " - ", If_not_a_first_Site_Visit_state_Original_Site_Visit_ID)) %>% 
    select(Questions, Values, issue, KEY),
  
  # HF Type and Staff Range doesn't match!
  qoc_data_approved$data %>% 
    filter((HF_Type %in% c("Region Hospital (RH)", "Provincial Hospital (PH)") & Staff_Range_Per_HF != 20) | 
             (HF_Type %in% "District hospital (DH)" & Staff_Range_Per_HF != 8) | 
             (HF_Type %in% "Comprehensive Health Centre (CHC)" & Staff_Range_Per_HF != 3) | 
             (HF_Type %in% c("Basic Health Centre (BHC)", "Sub Health Centre (SHC)") & Staff_Range_Per_HF != 2)) %>% 
    mutate(issue = "HF Type and Staff Range doesn't match!",
           Questions = "HF_Type - Staff_Range_Per_HF",
           Values = paste0(HF_Type, " - ", Staff_Range_Per_HF)) %>% 
    select(Questions, Values, issue, KEY),
  
  # Number of present members is only asked when it is higher than Staff Range
  qoc_data_approved$data %>% 
    filter(Staff_Range_Per_HF == Number_of_present_members) %>% 
    mutate(issue = "These two questions shouldn't be the same!",
           Questions = "Staff_Range_Per_HF - Number_of_present_members",
           Values = paste0(Staff_Range_Per_HF, " - ", Number_of_present_members)) %>% 
    select(Questions, Values, issue, KEY),
  
  qoc_data_approved$data %>% 
    filter(Number_of_present_members > Staff_Range_Per_HF) %>% 
    mutate(issue = "Number of present members should not be more than the staff range, plz double-check!",
           Questions = "Staff_Range_Per_HF - Number_of_present_members",
           Values = paste0(Staff_Range_Per_HF, " - ", Number_of_present_members)) %>% 
    select(Questions, Values, issue, KEY),
  
  qoc_data_approved$data %>%
    filter(Staff_member_interviewed_in_this != Health_Worker_Interview_Questions_count) %>%
    mutate(issue = "These two questions should be the same!",
           Questions = "Health_Worker_Interview_Questions_count - Staff_member_interviewed_in_this",
           Values = paste0(Health_Worker_Interview_Questions_count, " - ", Staff_member_interviewed_in_this)) %>%
    select(Questions, Values, issue, KEY),
  
  qoc_data_approved$data %>% 
    mutate(HF_Type_recoded = case_when(
      HF_Type %in% "Basic Health Centre (BHC)" ~ "BHC",
      HF_Type %in% "Comprehensive Health Centre (CHC)" ~ "CHC",
      # HF_Type %in% "Comprehensive Health Centre (CHC)" & CHC_Types %in% "Comprehensive Health Centre (CHC+)" ~ "CHC+",
      HF_Type %in% "District hospital (DH)" ~  "DH",
      HF_Type %in% "Provincial Hospital" ~ "PH", 
      HF_Type %in% "Sub Health Centre (SHC)" ~ "SHC",
      HF_Type %in% "Region Hospital" ~ "RH"
    )) %>% 
    filter(HF_Type_based_on_sample != HF_Type_recoded) %>% 
    mutate(issue = "The HF Type doesn't match the sample, plz double-check!",
           Questions = "HF_Type_based_on_sample - HF_Type",
           Values = paste0(HF_Type_based_on_sample, " - ", HF_Type)) %>% 
    select(Questions, Values, issue, KEY),
  
  #### Health_Worker_Interview
  qoc_data_approved$Health_Worker_Interview_Ques... %>% 
    filter(Interviewee_Respondent_Type %in% c("Midwife", "Gynecologist") & Gender_Of_Interviewee %notin% "Female") %>%
    mutate(issue = "Gender and Respondent type doesn't match",
           Questions = "Interviewee_Respondent_Type - Staff_member_interviewed_in_this",
           Values = paste0(Interviewee_Respondent_Type, " - ", Gender_Of_Interviewee)) %>%
    select(Questions, Values, issue, KEY)
)	


# agree_list <- c("Strongly Agree", "Agree")
# disagree_list <- c("Disagree", "Strongly Disagree")
# # Check whether issues like this should be reported
# inconsistent_Responses <- rbind(
#   # Inconsistent Responses
#   qoc_data_approved$Health_Worker_Interview_Ques... %>% 
#     filter(My_Supervisor_Is_Unfair_To_Me %in% agree_list & I_Can_Get_Help_From_My_Supervisor_When_I_Need_It %in% agree_list | 
#              (My_Supervisor_Is_Unfair_To_Me %in% disagree_list & I_Can_Get_Help_From_My_Supervisor_When_I_Need_It %in% disagree_list)) %>%
#     mutate(issue = "If supervisor is unfair then it's not likely to get help from him and vice versa",
#            Questions = "My_Supervisor_Is_Unfair_To_Me - I_Can_Get_Help_From_My_Supervisor_When_I_Need_It",
#            Values = paste0(My_Supervisor_Is_Unfair_To_Me, " - ", I_Can_Get_Help_From_My_Supervisor_When_I_Need_It)) %>%
#     select(Questions, Values, issue, KEY),
#   qoc_data_approved$Health_Worker_Interview_Ques... %>% 
#     filter(My_Supervisor_Is_Unfair_To_Me %in% agree_list & When_I_Do_A_Good_Job_I_Receive_The_Recognition_From_My_Supervisor %in% agree_list | 
#              (My_Supervisor_Is_Unfair_To_Me %in% disagree_list & When_I_Do_A_Good_Job_I_Receive_The_Recognition_From_My_Supervisor %in% disagree_list)) %>%
#     mutate(issue = "If supervisor is unfair then it's not likely to receive recognition from him and vice verse",
#            Questions = "My_Supervisor_Is_Unfair_To_Me - When_I_Do_A_Good_Job_I_Receive_The_Recognition_From_My_Supervisor",
#            Values = paste0(My_Supervisor_Is_Unfair_To_Me, " - ", When_I_Do_A_Good_Job_I_Receive_The_Recognition_From_My_Supervisor)) %>%
#     select(Questions, Values, issue, KEY),
#   qoc_data_approved$Health_Worker_Interview_Ques... %>% 
#     filter(My_Supervisor_Is_Unfair_To_Me %in% agree_list & My_Supervisor_Never_Gives_Me_Any_Feedback_About_How_Well_I_Am_Doing_In_My_Job %in% disagree_list | 
#              (My_Supervisor_Is_Unfair_To_Me %in% disagree_list & My_Supervisor_Never_Gives_Me_Any_Feedback_About_How_Well_I_Am_Doing_In_My_Job %in% agree_list)) %>%
#     mutate(issue = "If supervisor is unfair then he will not give any feedbacks and vice versa",
#            Questions = "My_Supervisor_Is_Unfair_To_Me - My_Supervisor_Never_Gives_Me_Any_Feedback_About_How_Well_I_Am_Doing_In_My_Job",
#            Values = paste0(My_Supervisor_Is_Unfair_To_Me, " - ", My_Supervisor_Never_Gives_Me_Any_Feedback_About_How_Well_I_Am_Doing_In_My_Job)) %>%
#     select(Questions, Values, issue, KEY)
# )

## Merge
qoc_logical_issues <- bind_rows(
  qoc_logical_issues
  # inconsistent_Responses
)

## HF Level checks ---------------------------------------------------------------------------------
# Total staff interviewed in an HF is less than the requirement
qoc_staff_interview <- qoc_data_approved$data %>%
  group_by(Site_Visit_ID, HF_Code_based_on_sample, HF_Name_based_on_Sample, HF_Type, Staff_Range_Per_HF) %>%
  summarise(total_staff_interviewed = sum(Staff_member_interviewed_in_this)) %>%
  filter(total_staff_interviewed < Staff_Range_Per_HF) %>% ungroup() %>% 
  mutate(issue = case_when(
    HF_Type %in% c("Basic Health Centre (BHC)", "Sub Health Centre (SHC)") ~ "The target interviews for BHC-SHC is 2",
    HF_Type %in% "Comprehensive Health Centre (CHC)" ~ "The target interviews for CHC is 3",
    HF_Type %in% "District hospital (DH)" ~ "The target interviews for DH is 8",
    HF_Type %in% c("Region Hospital (RH)", "Provincial Hospital (PH)") ~ "The target interviews for RH-PH is 20"
  )) 
qoc_duplicate_staff <- qoc_data_approved$Health_Worker_Interview_Ques... %>% janitor::get_dupes(Interviewee_Respondent_Type, Contact_number_of_Respondent, HF_Code_based_on_sample)


# # HF Type requirement is not met (QA: The FR might interview other health workers if the requirement is not met)
# qoc_data_approved$Health_Worker_Interview_Ques... %>%
#   # left_join(qoc_data_approved$data %>% select(HF_Type, KEY), by=c("PARENT_KEY"="KEY")) %>%
#   count(HF_Type, HF_Code_based_on_sample, Interviewee_Respondent_Type, KEY="") %>%
#   filter((HF_Type %in% "District hospital (DH)" & ((Interviewee_Respondent_Type %in% "Doctor/general practitioner" & n != 3) |
#                                                      (Interviewee_Respondent_Type %in% "Midwife" & n != 2) |
#                                                      (Interviewee_Respondent_Type %in% "Nurse" & n != 3))) |
#            (HF_Type %in% c("Region Hospital (RH)", 
#                            "Provincial Hospital (PH)") & ((Interviewee_Respondent_Type %in% "Doctor/general practitioner" & n != 10) |
#                                                             (Interviewee_Respondent_Type %in% "Midwife" & n != 4) |
#                                                             (Interviewee_Respondent_Type %in% "Nurse" & n != 6))) |
#            (HF_Type %in% "Comprehensive Health Centre (CHC)" & ((Interviewee_Respondent_Type %in% "Administrator" & n != 1) |
#                                                                   (Interviewee_Respondent_Type %in% "Midwife" & n != 1) |
#                                                                   (Interviewee_Respondent_Type %in% "Nurse" & n != 1))) | 
#            (HF_Type %in% c("Basic Health Centre (BHC)", "Sub Health Centre (SHC)") & ((Interviewee_Respondent_Type %in% "Administrator" & n != 1) |
#                                                                                         (Interviewee_Respondent_Type %in% "Midwife" & n != 1)))) %>%
#   mutate(issue = case_when(
#     HF_Type %in% c("Basic Health Centre (BHC)", "Sub Health Centre (SHC)") ~ "The target interviews for BHC-SHC is 2",
#     HF_Type %in% "Comprehensive Health Centre (CHC)" ~ "The target interviews for CHC is 3",
#     HF_Type %in% "District hospital (DH)" ~ "The target interviews for DH is 8",
#     HF_Type %in% c("Region Hospital (RH)", "Provincial Hospital (PH)") ~ "The target interviews for RH-PH is 20"
#   ),
#   Questions = "HF_Type - HF_Code_based_on_sample - Interviewee_Respondent_Type - Total_Interviews",
#   Values = paste0(HF_Type, " - ", HF_Code_based_on_sample, " - ", Interviewee_Respondent_Type, " - ", n)) %>%
#   select(Questions, Values, issue, KEY)


# Chech for repeat sheet mismatches ----------------------------------------------------------------
qoc_count_mismatch <- qoc_data_approved$Health_Worker_Interview_Ques... %>%
  count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="Health_Worker_Interview") %>%
  full_join(qoc_data_approved$data %>% select(main_sheet_count=Staff_member_interviewed_in_this, KEY) %>% 
              mutate(Sheet="Health_Worker_Interview", Question="Staff_member_interviewed_in_this"), by=c("KEY", "Sheet")) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count) %>% mutate(Tool="QoC")



