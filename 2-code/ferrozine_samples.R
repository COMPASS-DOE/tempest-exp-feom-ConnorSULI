# processing data from the ferrozine assay
# kfp, 2024-03-23

library(tidyverse)
options(scipen = 999999)

# step 1: import data -----------------------------------------------------

# a. plate map/layout
ferrozine_map = read.csv("1-data/ferrozine_MAP_2024-03-08.csv", na = "") %>% janitor::clean_names()

# b. sample key
ferrozine_key = read.csv("1-data/ferrozine-sampleID_kfp.csv") %>% janitor::clean_names() %>% dplyr::select(id, ferrozine_id)

# c. import data files
import_iron = function(FILEPATH){
  
  # import data files (plate reader)
  filePaths_ferrozine <- list.files(path = FILEPATH, pattern = ".csv", full.names = TRUE, recursive = TRUE)
  ferrozine_data <- do.call(bind_rows, lapply(filePaths_ferrozine, function(path) {
    df <- read.csv(path, skip = 24) %>% mutate_all(as.character) %>% janitor::clean_names()
    df = df %>% mutate(source = basename(path))
    df}))
  
  ferrozine_data
}
ferrozine_data = import_iron(FILEPATH = "1-data/ferrozine_samples")


# step 2: process data ----------------------------------------------------

# a. clean the map ----
map_processed = 
  ferrozine_map %>% 
  mutate(date = mdy(date)) %>% 
  fill(date, tray, analysis) %>% 
  pivot_longer(-c(date, tray, analysis, dilution, letter, notes), names_to = "number", values_to = "sample_label") %>% 
  filter(!is.na(sample_label)) %>% 
  mutate(number = parse_number(number)) %>% 
  mutate_at(vars(c(dilution, number)), as.numeric) %>% 
  mutate(well_position = paste0(letter, number),
         dilution = if_else(is.na(dilution), 1, dilution)) %>% 
  arrange(date, tray, number, letter) %>% 
  mutate(sample_type = case_when(grepl("mM", sample_label) ~ "standard",
                                 TRUE ~ "sample")) %>% 
  dplyr::select(date, tray, analysis, dilution, well_position, sample_label, sample_type)

#
# b. clean the data ----

# first, get the dataframe into a proper format
data_columns = 
  ferrozine_data %>% 
  mutate_all(na_if,"") %>% 
  janitor::remove_empty(which = "cols") %>% 
  dplyr::select(source, everything())

# rename the columns to what we need
colnames(data_columns) = c("source", "letter", 1,2,3,4,5,6,7,8,9,10,11,12, "wavelength")

# process the dataframe
data_processed = 
  data_columns %>% 
  fill(letter) %>% 
  filter(wavelength == "562") %>% 
  pivot_longer(-c(source, wavelength, letter), values_to = "absorbance_562", names_to = "number") %>% 
  mutate(name = as.numeric(number),
         well_position = paste0(letter, number),
         date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
         date = ymd(date),
         tray = str_extract(source, "plate[1-9][a-z]?"),
         tray = parse_number(tray, "plate"),
         absorbance_562 = as.numeric(absorbance_562)) %>% 
  dplyr::select(date, tray, well_position, absorbance_562) %>% 
  left_join(map_processed, by = c("date", "tray", "well_position")) %>% 
  filter(!grepl("skip", sample_label)) %>% 
  filter(!is.na(sample_label))

#
# c. calibrate the ferrozine data ----

calibrate_ferrozine_data = function(data_processed){
  # now do the calibrations
  # standards are in mM
  # molecular formula for FAS = (NH₄)₂Fe(SO₄)₂·6H₂O
  # so 1 M FAS = 1M Fe
  # 1 mM FAS = 1 * 55.85 mg Fe in 1 L solution = 55.85 mg Fe in 1 L solution
  # therefore 1 mM = 55.85 mg/L or 55.85 ppm
  
  standards = 
    data_processed %>% 
    filter(sample_type == "standard") %>% 
    mutate(standard_mM = parse_number(sample_label),
           standard_type = str_extract(sample_label, "FAS|FeCl3"),
           standard_ppm =  case_when(standard_type == "FAS" ~ standard_mM * 55.85)) %>% 
    #dplyr::select(date, tray, absorbance_562, standard_ppm) %>% 
    mutate(standard_ppm = as.numeric(standard_ppm))
  
  # reduction efficiency
  reduction_efficiency = 95
  
  gg_calibration = 
    standards %>% 
    filter(standard_mM < 2.5) %>% 
    ggplot(aes(x = standard_ppm, y = absorbance_562, color = as.character(tray)))+
    geom_point()+
    geom_smooth(method = "lm", se = F)+
    facet_wrap(~date + tray)
  
  calibration_coef = 
    standards %>% 
    filter(standard_mM < 2.5) %>% 
    drop_na() %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarize(slope = lm(absorbance_562 ~ standard_ppm)$coefficients["standard_ppm"], 
                     intercept = lm(absorbance_562 ~ standard_ppm)$coefficients["(Intercept)"])
  
  # y = mx + c
  # abs = m*ppm + c
  # ppm = abs-c/m
  
  data_calibrated = 
    data_processed %>% 
    left_join(calibration_coef, by = c("date")) %>% 
    mutate(ppm_calculated = ((absorbance_562 - intercept) / slope))
  
  list(calibration_coef = calibration_coef,
       data_calibrated = data_calibrated,
       gg_calibration = gg_calibration,
       reduction_efficiency = reduction_efficiency)
}

calibration_curves = calibrate_ferrozine_data(data_processed)$gg_calibration
reduction = calibrate_ferrozine_data(data_processed)$reduction_efficiency

samples = 
  calibrate_ferrozine_data(data_processed)$data_calibrated %>% 
  filter(sample_type == "sample") %>% 
  mutate(ppm_calculated = ppm_calculated * dilution) %>% 
  dplyr::select(date, sample_label, analysis, ppm_calculated) %>% 
  filter(!is.na(ppm_calculated)) %>% 
  filter(ppm_calculated >= 0) %>% 
  mutate(flag = case_when(ppm_calculated < 5 ~ "below detection"),
         ppm_calculated = round(ppm_calculated, 2)) %>% 
  group_by(date, sample_label, analysis) %>% 
  dplyr::mutate(mean = mean(ppm_calculated),
                sd = sd(ppm_calculated),
                cv = 100 * sd/mean) %>% 
  # filter samples as needed
  force()

samples_summary = 
  samples %>% 
  group_by(date, sample_label, analysis) %>% 
  dplyr::summarize(ppm_calculated = mean(ppm_calculated)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "analysis", values_from = "ppm_calculated") %>% 
 # mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(Fe_total = Fe_total * 100/reduction,  # correct for 99.6% reduction efficiency of ascorbic acid
         Fe3 = Fe_total - Fe2) %>% 
  rename(Fe2_ppm = Fe2,
         Fe3_ppm = Fe3,
         FeTotal_ppm = Fe_total,
         ferrozine_id = sample_label) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  ungroup() %>% 
  right_join(ferrozine_key) %>% 
  dplyr::select(id, ferrozine_id, everything())
