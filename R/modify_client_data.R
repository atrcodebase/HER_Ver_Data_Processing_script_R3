# Anonymize PII fields -----------------------------------------------------------------------------
anyonymize_PII <- function(data, tool_path){
  tool <- read_excel(tool_path, "survey", guess_max = 100000)
  url_cols <- tool %>% filter(type %in% c("audio", "image")) %>% pull(name)
  
  data <- data %>% mutate(across(any_of(url_cols), function(x){
    x=case_when(
      grepl(download_link, x) ~ "REDACTED",
      TRUE ~ x
    )
  }))
  
  return(data)
}

## HF Data Verification 
for(sheet in names(HF_data_approved)){
  HF_data_approved[[sheet]] <- anyonymize_PII(HF_data_approved[[sheet]], hf_tool_path)
}

## QoC
qoc_data_approved$data <- anyonymize_PII(qoc_data_approved$data, qoc_tool_path)
qoc_data_approved$Health_Worker_Interview_Ques... <- anyonymize_PII(qoc_data_approved$Health_Worker_Interview_Ques..., qoc_tool_path)

## QQC
for(sheet in names(qqc_data_approved)){
  qqc_data_approved[[sheet]] <- anyonymize_PII(qqc_data_approved[[sheet]], qqc_tool_path)
}

## HMIS Service assessment
for(sheet in names(hmis_data_approved)){
  hmis_data_approved[[sheet]] <- anyonymize_PII(hmis_data_approved[[sheet]], hmis_tool_path)
}

## SP Personnel 
for(sheet in names(sp_data_approved)){
  sp_data_approved[[sheet]] <- anyonymize_PII(sp_data_approved[[sheet]], sp_tool_path)
}

## Vignette 
vignette_data_approved <- anyonymize_PII(vignette_data_approved, vignette_tool_path)

## Patient Verification 
patient_data_approved <- anyonymize_PII(patient_data_approved, patient_tool_path)



# Remove extra objects
rm(anyonymize_PII)

