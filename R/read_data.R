### Read Data 
guess_max <- 5000000
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA

## QoC - Interview with Health Workers
qoc_path <- "input/raw_data/HER Re - R2 QoC - Interview with Health Workers.xlsx"
qoc_data <- read_excel(qoc_path, sheet = "data", guess_max = guess_max, na = convert_to_na)
qoc_interview <- read_excel(qoc_path, sheet = "Health_Worker_Interview_Ques...", guess_max = guess_max, na = convert_to_na)

## QQC
qqc_path <- "input/raw_data/HER Re QQC R2.xlsx"
qqc_sheets <- excel_sheets(qqc_path)
# Read as list
qqc_data <- purrr::map2(qqc_path, getSheetNames(qqc_path), ~ readxl::read_excel(.x, sheet = .y, guess_max=guess_max, na=convert_to_na), .progress = TRUE)  %>% setNames(qqc_sheets)

## HF Level Data Verification
HF_verf_path <- "input/raw_data/HER Re HF Level Data Verification Tool R2.xlsx"
HF_verf_sheets <- excel_sheets(HF_verf_path)
HF_data <- purrr::map2(HF_verf_path, getSheetNames(HF_verf_path), ~ readxl::read_excel(.x, sheet = .y, guess_max=guess_max, na=convert_to_na), .progress = TRUE)  %>% setNames(HF_verf_sheets)

## SP Personal Attendance Check
sp_path <- "input/raw_data/HER Re - R2 SP Personnel Attendance Check.xlsx"
sp_data <- read_excel(sp_path, sheet = "data", guess_max = guess_max, na = convert_to_na)
sp_personnel <- read_excel(sp_path, sheet = "Personnel", guess_max = guess_max, na = convert_to_na)
sp_absent <- read_excel(sp_path, sheet = "Absent_Days", guess_max = guess_max, na = convert_to_na)


# Remove Extra Objects -----------------------------------------------------------------------------
rm(convert_to_na, guess_max, qoc_path, qqc_path, HF_verf_path, sp_path
   # sheet, variable, service_path
 )
