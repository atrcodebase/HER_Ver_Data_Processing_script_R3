sp_logical_issues <- rbind(
  # 
  sp_data_approved$Personnel %>% 
    filter(Total_Working_Days > 31) %>% 
    mutate(issue = "Incorrect working days",
           Questions = "Total_Working_Days",
           Values = Total_Working_Days) %>% 
    select(Questions, Values, issue, KEY),
  # 
  sp_data_approved$Personnel %>% 
    filter(Missing_Days > Total_Working_Days) %>% 
    mutate(issue = "Missing days shouldn't be more than working days",
           Questions = "Missing_Days - Total_Working_Days",
           Values = paste0(Missing_Days, " - ", Total_Working_Days)) %>% 
    select(Questions, Values, issue, KEY),
  #
  sp_data_approved$Absent_Days %>% 
    filter(Absent_Days_Month1 > 31 | Absent_Days_Month2 > 31 | Absent_Days_Month3 > 31) %>% 
    mutate(issue = "Absentees shouldn't be more than 31",
           Questions = "Absent_Days_Month1 - Absent_Days_Month2 - Absent_Days_Month3",
           Values = paste0(Absent_Days_Month1, " - ", Absent_Days_Month2, " - ", Absent_Days_Month3)) %>% 
    select(Questions, Values, issue, KEY)
)
# Chech for repeat sheet mismatches
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
