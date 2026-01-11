# Change/Recode variables
relabel_98_99 <- function(x) {
  x = case_when(
    x %in% c(9999, "9999") ~ "No stock card",
    TRUE ~ x
  )}

# Numeric cols
HF_numeric_cols <- read_excel(hf_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 
qoc_numeric_cols <- read_excel(qoc_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 
qqc_numeric_cols <- read_excel(qqc_tool_path, "survey", guess_max = 50000) %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 
hmis_numeric_cols <- read_excel(hmis_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 
sp_numeric_cols <- read_excel(sp_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 
vig_numeric_cols <- read_excel(vignette_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 
patient_numeric_cols <- read_excel(patient_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name) 

qqc_no_stock_card <- read_excel(qqc_tool_path, sheet = "survey", guess_max = 50000) %>% 
  filter(hint %in% c("[if there is no stock card put 999]", "[if there is no stock card put 9999]")) %>% pull(name)
qqc_no_stock_card_amc <- read_excel(qqc_tool_path, sheet = "survey", guess_max = 50000) %>% 
  filter(hint %in% c("[if AMC is not recorded in the stock card/or there is no stock card put 999]",
                     "[if AMC is not recorded in the stock card/or there is no stock card put 9999]")) %>% pull(name)

## HF Level Data Verification ----------------------------------------------------------------------
HF_data$data <- HF_data$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")) %>% 
  mutate(HF_Type = case_when(
    HF_Type %in% "RH" ~ "Regional Hospital (RH)",
    HF_Type %in% "PH" ~ "Provincial Hospital (PH)",
    HF_Type %in% "DH" ~ "District hospital (DH)",
    HF_Type %in% "CHC" ~ "Comprehensive Health Centre (CHC)",
    HF_Type %in% "CHC+" ~ "Comprehensive Health Centre (CHC +)",
    HF_Type %in% "BHC" ~ "Basic Health Centre (BHC)",
    HF_Type %in% "BHC+" ~ "Basic Health Centre (BHC +)",
    HF_Type %in% "SHC" ~ "Sub Health Centre (SHC)",
    TRUE ~ HF_Type
  )) 
  
# Subset & Join main sheet columns
HF_data_sub <- HF_data$data %>% 
  select(Site_Visit_ID, Province, District, Interview_Type_SV, Region, HF_Type_based_on_sample, 
         HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, HF_Type, KEY)

# Apply changes on all sheets
for(sheet in names(HF_data)){
  # Relabel numeric cols
  HF_data[[sheet]] <- HF_data[[sheet]] %>%
    mutate(across(any_of(HF_numeric_cols), as.character)) %>% 
    mutate(across(any_of(HF_numeric_cols), relabel_98_99))
  
  # Update links
  key_col <- "KEY" # ifelse(sheet %in% "data", "KEY", "PARENT_KEY")
  HF_data[[sheet]] <- update_media_links(data=HF_data[[sheet]], 
                                       tool_path = hf_tool_path,  
                                       download_link=download_link,
                                       key_col) # No need if data is downloaded from SCTO website
  
  # Join main Sheet cols
  if(sheet!="data"){
    HF_data[[sheet]] <- HF_data[[sheet]] %>% 
      left_join(HF_data_sub, by=c("KEY")) %>% # New key
      relocate(Site_Visit_ID:SP_Name_based_on_sample, .before = 1)
  }
}


# Join Extra Columns
# HF_data$Drug_Expiration <- HF_data$Drug_Expiration %>% 
#   left_join(HF_data$data %>% select(n_available_drug, KEY), by="KEY") %>% 
#   relocate(n_available_drug, .before=Drug_Name_Available)
HF_data$Drug_Out_Of_Stock <- HF_data$Drug_Out_Of_Stock %>%
  left_join(HF_data$Drug_Availability_Reporting_... %>% select(Drug_Name_Available_RP, Was_Drug_Out_Of_Stock_RP, KEY_Unique), by=c("PARENT_KEY"="KEY_Unique")) %>% 
  relocate(Drug_Name_Available_RP:Was_Drug_Out_Of_Stock_RP, .before = indx6) %>% 
  mutate(across(all_of("Date_In_Stock_RP"), function(x){
    x = case_when(
      x %in% "9999" ~ "Not in stock",
      TRUE ~ x
    )}))
HF_data$Consumable_Out_Of_Stock_RP <- HF_data$Consumable_Out_Of_Stock_RP %>%
  left_join(HF_data$List_Of_Consumables_RP %>% select(Consumable_Name_RP, Was_Consumable_Out_Of_Stock_RP, KEY_Unique), by=c("PARENT_KEY"="KEY_Unique")) %>% 
  relocate(Consumable_Name_RP:Was_Consumable_Out_Of_Stock_RP, .before = indx3) %>% 
  mutate(across(all_of("Date_Were_Consumable_In_Stock_RP"), function(x){
    x = case_when(
      x %in% "9999" ~ "Not in stock",
      TRUE ~ x
    )}))


## QoC - Interview with Health Workers -------------------------------------------------------------
qoc_data$data <- qoc_data$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))
         # Confirm_the_number_of_HF_staff_present=str_replace_all(Confirm_the_number_of_HF_staff_present, "StaffRangePerHF", "Staff_Range_Per_HF"))

# Subset & Join main sheet columns
qoc_data_sub <- qoc_data$data %>% 
  select(Site_Visit_ID, Province, District, HF_Type, Region, Area_Type, HF_Type_based_on_sample, 
         HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, KEY)
qoc_data$Health_Worker_Interview_Ques... <- qoc_data$Health_Worker_Interview_Ques... %>%
  left_join(qoc_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:SP_Name_based_on_sample, .before = 1)

# Update links
qoc_data$data <- update_media_links(data=qoc_data$data, tool_path = qoc_tool_path, download_link=download_link) # No need if data is downloaded from SCTO website

qoc_data$Health_Worker_Interview_Ques... <- update_media_links(data=qoc_data$Health_Worker_Interview_Ques..., 
                                          tool_path = qoc_tool_path,
                                          download_link=download_link, 
                                          key_col="PARENT_KEY") # No need if data is downloaded from SCTO website

## QQC ---------------------------------------------------------------------------------------------
qqc_data$data <- qqc_data$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")) %>% 
  mutate(across(all_of(c(qqc_no_stock_card, qqc_no_stock_card_amc)), as.character)) %>% 
  mutate(across(all_of(qqc_no_stock_card), function(x){
    x = case_when(
      x %in% c(999, 9999, 99999) ~ "No stock card",
      TRUE ~ x
    )}
    )) %>% 
  mutate(across(all_of(qqc_no_stock_card_amc), function(x){
    x = case_when(
      x %in% c(999, 9999, 99999) ~ "No stock card/AMC not recorded",
      TRUE ~ x
    )}
    )) %>% 
  mutate(HF_Type = case_when(
    HF_Type %in% "DH" ~ "District hospital (DH)",
    HF_Type %in% "CHC" ~ "Comprehensive Health Centre (CHC / CHC +)",
    HF_Type %in% "BHC" ~ "Basic Health Centre (BHC/BHC+)",
    TRUE ~ HF_Type
  ))

# Update URL links in all sheets
for(sheet in names(qqc_data)){
  # Update links
  key_col <- ifelse(sheet %in% "data", "KEY", "PARENT_KEY")
  qqc_data[[sheet]] <- update_media_links(data=qqc_data[[sheet]], 
                                         tool_path = qqc_tool_path,  
                                         download_link=download_link,
                                         key_col) # No need if data is downloaded from SCTO website
}

## HMIS Service assessment -------------------------------------------------------------------------
hmis_data$data <- hmis_data$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")) 
# Labesl
source("R/labeling/hmis_labels.R") 



# Subset & Join main sheet columns
hmis_sub <- hmis_data$data %>% 
  select(Site_Visit_ID, Province, District, HF_Type, Region, HF_Type_based_on_sample, 
         HF_Code_based_on_sample, HF_Name_based_on_Sample, SP_Name_based_on_sample, Quarter, 
         Type_of_service_general, KEY)

# Apply changes on all sheets
for(sheet in names(hmis_data)){
  # Relabel numeric cols
  hmis_data[[sheet]] <- hmis_data[[sheet]] %>%
    mutate(across(any_of(hmis_numeric_cols), as.character)) %>% 
    mutate(across(any_of(hmis_numeric_cols), relabel_98_99))
  
  # Update links
  key_col <- "KEY" # ifelse(sheet %in% "data", "KEY", "PARENT_KEY")
  hmis_data[[sheet]] <- update_media_links(data=hmis_data[[sheet]], 
                                         tool_path = hmis_tool_path,  
                                         download_link=download_link,
                                         key_col) # No need if data is downloaded from SCTO website
  
  # Join main Sheet cols
  if(sheet!="data"){
    hmis_data[[sheet]] <- hmis_data[[sheet]] %>% 
      left_join(hmis_sub, by=c("KEY")) %>% # New key
      relocate(Site_Visit_ID:Type_of_service_general, .before = 1)
  }
}

## SP Personal Attendance Check --------------------------------------------------------------------
sp_data$data <- sp_data$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))

# Subset & Join main sheet columns
sp_data_sub <- sp_data$data %>% 
  select(Site_Visit_ID, Province, District, SP_Name_based_on_sample, Region, Area_Type, KEY)

# Apply changes on all sheets
for(sheet in names(sp_data)){
  # Relabel numeric cols
  sp_data[[sheet]] <- sp_data[[sheet]] %>%
    mutate(across(any_of(sp_numeric_cols), as.character)) %>% 
    mutate(across(any_of(sp_numeric_cols), relabel_98_99))
  
  # Update links
  key_col <- ifelse(sheet %in% "data", "KEY", "PARENT_KEY")
  sp_data[[sheet]] <- update_media_links(data=sp_data[[sheet]], 
                                           tool_path = sp_tool_path,  
                                           download_link=download_link,
                                           key_col) # No need if data is downloaded from SCTO website
  
  # Join main Sheet cols
  if(sheet!="data"){
    sp_data[[sheet]] <- sp_data[[sheet]] %>% 
      left_join(sp_data_sub, by=c("PARENT_KEY"="KEY")) %>% 
      relocate(Site_Visit_ID:Area_Type, .before = 1)
  }
}

## Vignette ----------------------------------------------------------------------------------------
vignette_data <- vignette_data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))

# Update links
vignette_data <- update_media_links(data=vignette_data, tool_path = vignette_tool_path, download_link=download_link) # No need if data is downloaded from SCTO website

## Patient Verification ----------------------------------------------------------------------------
patient_data <- patient_data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")) %>% 
  mutate(HF_Type = case_when(
    HF_Type %in% "RH" ~ "Regional Hospital (RH)",
    HF_Type %in% "PH" ~ "Provincial Hospital (PH)",
    HF_Type %in% "DH" ~ "District hospital (DH)",
    HF_Type %in% "CHC" ~ "Comprehensive Health Centre (CHC)",
    HF_Type %in% "CHC+" ~ "Comprehensive Health Centre (CHC +)",
    HF_Type %in% "BHC" ~ "Basic Health Centre (BHC)",
    HF_Type %in% "BHC+" ~ "Basic Health Centre (BHC +)",
    HF_Type %in% "SHC" ~ "Sub Health Centre (SHC)",
    TRUE ~ HF_Type
  ),
  Type_of_service=case_when(
    Type_of_service %in% "1" ~ "Ante-natal Care (ANC)",
    Type_of_service %in% "2" ~ "Post-natal Care (PNC)",
    Type_of_service %in% "3" ~ "Deliveries",
    Type_of_service %in% "4" ~ "Pentavalent Vaccine (3rd dose)",
    Type_of_service %in% "5" ~ "Family Planning",
    Type_of_service %in% "6" ~ "Toxoid Tetanus TT+/TD+ vaccine",
    Type_of_service %in% "7" ~ "Tuberculosis Exams (TB Smear + Cured Cases)",
    Type_of_service %in% "8" ~ "Under 5 (U5) Children Morbidities",
    Type_of_service %in% "9" ~ "Growth Monitoring of Children Under 2 years",
    Type_of_service %in% "10" ~ "C-Section",
    Type_of_service %in% "11" ~ "Major Surgery",
    TRUE ~ as.character(Type_of_service)
    ))

# Update links
patient_data <- update_media_links(data=patient_data, tool_path = patient_tool_path, download_link=download_link) # No need if data is downloaded from SCTO website


# remove extra objects -----------------------------------------------------------------------------
rm()

