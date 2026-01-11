# QQC
qqc_data_filtered$data <- update_image_qa_status(data=qqc_data_filtered$data,
                                                 tool_path = qqc_tool_path, 
                                                 different_cols = list("q10_7_photo"="q10_7_QA"),
                                                 excluded_cols="Signature_Of_Respondent")
for(sheet in qqc_sheets[qqc_sheets %notin% "data"]){
  qqc_data_filtered[[sheet]] <- update_image_qa_status(data=qqc_data_filtered[[sheet]], 
                                                       tool_path = qqc_tool_path)
}

# HF Validation 
for(sheet in HF_verf_sheets){
  HF_data_filtered[[sheet]] <- update_image_qa_status(data= HF_data_filtered[[sheet]], 
                                                      tool_path=hf_tool_path)
}

# QoC (No Image columns)

# SP
sp_data_filtered <- update_image_qa_status(data=sp_data_filtered, tool_path = sp_tool_path)
sp_absent_filtered <- update_image_qa_status(data=sp_absent_filtered, tool_path = sp_tool_path)
sp_personnel_filtered <- update_image_qa_status(data=sp_personnel_filtered, tool_path = sp_tool_path)
