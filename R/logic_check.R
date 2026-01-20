### Logic Checks 
# Completed Logic checks: HF_Verification, QoC, QQC, HMIS, Patient, Vignette

### HF Data Verification ---------------------------------------------------------------------------
# source("R/logic_checks/HF_verification.R") # Check Commented Sections
# Note: some sections did not have data while adding these checks, might requrie review later

# ## QoC - Interview with Health Workers -------------------------------------------------------------*
# source("R/logic_checks/QoC.R")
# 
# ## QQC ---------------------------------------------------------------------------------------------*
# source("R/logic_checks/QQC.R") 
# # Note: Add constraint checks

## HMIS Service Assessment -------------------------------------------------------------------------*
source("R/logic_checks/Service_assessment.R") # Needs work
# Note: check if all service types are visited for each HF
# Patient_Date_of_Visit requires manual review

### SP Personnel -----------------------------------------------------------------------------------*
source("R/logic_checks/SP.R") # No data yet

## Vignette ----------------------------------------------------------------------------------------
# source("R/logic_checks/Vignette.R")

## Patient Verification ----------------------------------------------------------------------------*
source("R/logic_checks/Patient_verification.R")
# Exclude comented issues under: resp_type - Patient_Age_Sample

# Rejected and approved data -----------------------------------------------------------------------
rejec_approved <- rbind(
  HF_data$data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "HF_validation"),
  qoc_data$data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "QoC"),
  qqc_data$data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "QQC"),
  hmis_data$data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "Service_assessment"),
  # sp_data$data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
  #   select(KEY, review_status, qa_status) %>% mutate(Tool = "SP"),
  vignette_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "Vignette"),
  patient_data %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
    select(KEY, review_status, qa_status) %>% mutate(Tool = "Patient")
)


# Health Facility Level Checks ---------------------------------------------------------------------
#### Sample check
ver_sample <- read_excel("input/sample/HER_Verification_October_2025_Resample_Final - Site Visits_unprotected.xlsx", sheet = "Sample")
ver_sample <- ver_sample %>% 
  select(Site_Visit_ID, Province=`Province Name`, District=`District Name`, Region=Zone,
         HF_Code_based_on_sample=FacilityID, HF_Name_based_on_Sample=`Facility Name (DHIS2)`,
         SP_Name_based_on_sample=SPs, Sample_Type)


#### Missing Tools
meta_cols <- c("Site_Visit_ID", "Province", "District", "HF_Type_based_on_sample",
          "HF_Code_based_on_sample", "HF_Name_based_on_Sample")
tool_names <- c("HF_Verification", "QoC", "QQC", "HMIS", "Vignette", "Patient_Verification")

# Merge Sample cols
sample_info <- plyr::rbind.fill(
  HF_data_approved$data %>% select(all_of(meta_cols)) %>% mutate(tool = "HF_Verification"),
  qoc_data_approved$data %>% select(all_of(meta_cols)) %>% mutate(tool = "QoC"),
  qqc_data_approved$data %>% select(all_of(meta_cols)) %>% mutate(tool = "QQC"),
  hmis_data_approved$data %>% select(all_of(meta_cols)) %>% mutate(tool = "HMIS"),
  # sp_data_approved$data %>% select(all_of(meta_cols)) %>% mutate(tool = "SP"), # Service Provider Level not HF
  vignette_data_approved %>% select(all_of(meta_cols)) %>% mutate(tool = "Vignette"),
  patient_data_approved %>% select(all_of(meta_cols)) %>% mutate(tool = "Patient_Verification")
) %>% unique() %>% 
  group_by(HF_Code_based_on_sample, HF_Name_based_on_Sample) %>%
  mutate(Missing_data = paste0(tool_names[tool_names %notin% tool], collapse = " & "),
         Collected_data = paste0(tool, collapse = " & "),
         Total_collected_tools = length(Collected_data), tool=NULL) %>% 
  ungroup() %>% unique() %>% 
  filter(!is.na(Site_Visit_ID)) # %>% select(Site_Visit_ID, Collected_data, Missing_data, Total_collected_tools)

missing_HFs <- ver_sample %>% #filter(Site_Visit_ID %in% "HER-BDS-HF2507-Q1-2023-595-01")
  full_join(sample_info, 
            by = join_by("Site_Visit_ID", "Province", "District", "HF_Code_based_on_sample", "HF_Name_based_on_Sample")) %>% 
  mutate( Total_collected_tools=case_when(
    is.na(Total_collected_tools) ~ 0,
    TRUE ~ Total_collected_tools
  ),
  Remarks=case_when(
    (duplicated(HF_Code_based_on_sample) | duplicated(HF_Code_based_on_sample, fromLast = TRUE)) & 
      Sample_Type %notin% c("Replaced", "Replacement") ~ "One or more Health Facility related information is different across tools",
    Total_collected_tools == 0 ~ "Sampled HF Not visited yet!",
    is.na(SP_Name_based_on_sample) ~ "Site Visit not found in the sample!",
    Total_collected_tools == 6 | (Missing_data %in% "QQC" & HF_Type_based_on_sample %notin% c("BHC", "CHC", "DH")) ~ "Complete (QQC not collected if HF not BHC/CHC/DH)!",
    Sample_Type %in% c("Replaced") & !is.na(Collected_data) ~ "Data in Replaced Health Facility!",
    # "QQC only collected for BHC, CHC & DH"
  ),
  Missing_data=case_when(
    is.na(Missing_data) & is.na(Collected_data) ~ paste0(tool_names, collapse = " & "),
    (grepl("QQC", Missing_data) & HF_Type_based_on_sample %notin% c("BHC", "CHC", "DH")) ~ str_remove(Missing_data, "QQC"),
    TRUE ~ Missing_data
  )) 
# Check if same HF has different data across tools
# missing_HFs %>% janitor::get_dupes(HF_Code_based_on_sample) %>% View

# Check which HFs are missing in which Tool
# missing_HFs <- missing_HFs %>% filter(Total_collected_tools %notin% 6)

#### Manual Checks
sample_info %>% janitor::get_dupes(HF_Code_based_on_sample)# %>% View # Same HF having inconsistent info across tools
sample_info %>% janitor::get_dupes(Site_Visit_ID) #%>% View # Same Site Visit having inconsistent info across tools



#### Check if any of the Sample columns are null
meta_cols <- c("SubmissionDate", "Starttime", "Endtime", "duration", "Date_And_Time", 
  "Surveyor_Id", "Surveyor_Gender_Paused", "Site_Visit_ID", "Site_Visit_Subcategory_ID", "TPMA_Location_Name", 
  "TPMA_Location_ID", "Province", "District", "Region", "Sector", "Sampling_Proposed_By", 
  "Type_Of_Sampling", "Planned_data_collection_period", "type_of_interview", 
  "HF_Type_based_on_sample", "HF_Code_based_on_sample", "HF_Name_based_on_Sample", "SP_Name_based_on_sample", 
  "Line_Ministry_Name", "Line_Ministry_Project_Id", "Line_Ministry_SubProject_Id", 
  "Line_Ministry_Sub_Project_Name_And_Description", "Type_Of_Implementing_Partner", 
  "Type_Of_Site_Visit", "Type_Of_Visit",  "Reporting_Period", "KEY"
  #"Area_Type", "If_not_a_first_Site_Visit_state_Original_Site_Visit_ID", # Mostly NA
)

missing_sample_data <- plyr::rbind.fill(
  HF_data_approved$data %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "HF_Verification"),
  qoc_data_approved$data %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "QoC"),
  qqc_data_approved$data %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "QQC"),
  hmis_data_approved$data %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "HMIS"),
  sp_data_approved$data %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "SP"),
  vignette_data_approved %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "Vignette"),
  patient_data_approved %>% select(any_of(meta_cols)) %>% 
    pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
    mutate(tool = "Patient_Verification")
) %>%     #pivot_longer(-KEY, names_to = "Questions", values_to = "Values", values_transform = as.character) %>% 
  filter(Values %in% c("", NA, "NA")) %>% 
  mutate(issue="Data is missing!", qa_status="Approved") %>%
  select(Questions, Values, issue, KEY, qa_status, tool)

## Check keys
if(any(duplicated(HF_data_approved$data$KEY)) | 
   any(duplicated(qoc_data_approved$data$KEY)) |
   any(duplicated(qqc_data_approved$data$KEY)) | 
   any(duplicated(hmis_data_approved$data$KEY)) |
   any(duplicated(sp_data_approved$data$KEY)) |
   any(duplicated(vignette_data_approved$KEY)) |
   any(duplicated(patient_data_approved$KEY))
   ){
  message("Duplicate UUID found, plz double-check!")
}

# Other Checks -------------------------------------------------------------------------------------


# Export list --------------------------------------------------------------------------------------
logical_issues <- plyr::rbind.fill(
  # hf_logical_issues %>% mutate(Tool="HF_data"),
  # qoc_logical_issues %>% mutate(Tool="QoC"),
  # QQC_logical_issues %>% mutate(Tool="QQC"),
  hmis_logical_issues, # Tool name added in check
  sp_logical_issues %>% mutate(Tool="SP"),
  # vignette_logical_issues %>% mutate(Tool="Vignette"),
  patient_logical_issues %>% mutate(Tool="Patient_Verification")
  # 
) 

# Repeat sheet mismatches
repeatsheet_count_mismatch <- plyr::rbind.fill(
  # HF_count_mismatch,
  # qoc_count_mismatch,
  # QQC_count_mismatch,
  hmis_count_mismatch,
  sp_count_mismatch
)

# Potential Numeric Codes 
numeric_issues_list <- plyr::rbind.fill(
  # hf_other_num_issues,
  # qoc_other_num_issues,
  # qqc_other_num_issues,
  hmis_other_num_issues,
  # sp_count_mismatch,
  # vign_other_num_issues,
  patient_other_num_issues,
  sp_other_num_issues
)


# Final Export List
logical_issues_list <- list(
  logical_issues=logical_issues,
  numeric_issues=numeric_issues_list,
  missing_sample_data=missing_sample_data,
  repeat_sheet_issues=repeatsheet_count_mismatch,
  rejec_approved=rejec_approved
)

## Missing Data List
missing_data_list <- list(
  # HF_level=missing_HFs,
  # HF_Verification=hf_interview_type,
  # HF_Ver_staff=staff_inconsistency, # Check where to add
  # QoC_Staff_Interview=qoc_staff_interview, # QA: Not needed
  # QoC_duplicate_staff=qoc_duplicate_staff,
  # QQC=qqc_interview_type,
  HMIS=hmis_service_type,
  HMIS_Patient_Dup=hmis_duplicate_patients,
  Patient_Ver_Dup=duplicate_patients_ver,
  Patient_missing=patient_cross_check,
  Patient_per_Service=patient_per_service
  # Vignette_interviews=vign_interview_type
)


# Remove extra objects -----------------------------------------------------------------------------
# rm(cols, lc_HER_RE_QQC.general_management, lc_HER_RE_QQC.hygiene, lc_HER_RE_QQC.curative_consultations,
#    lc_HER_RE_QQC.family_planning, lc_HER_RE_QQC.laboratory, lc_HER_RE_QQC.essential_drugs_management,
#    lc_HER_RE_QQC.tracer_drugs, lc_HER_RE_QQC.maternity, lc_HER_RE_QQC.epi_pre_school_consultation,
#    lc_HER_RE_QQC.antenatal_care, HF_data.lc, HF_personnel_lc, HF_drugs_lc, HF_drug_expiration_lc,
#    HF_drug_avail_report_lc, HF_drug_outof_stock_lc, data.consumable_rp_lc, HF_equipment_lc, 
#    HF_hospital_gov_lc)

