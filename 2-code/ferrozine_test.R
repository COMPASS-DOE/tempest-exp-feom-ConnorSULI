# processing data from the ferrozine assay for test samples
# kfp, 2024-03-01

library(tidyverse)

import_iron = function(FILEPATH = "1-data/ferrozine_test"){
  
  # import map
  ferrozine_map = read_csv("1-data/ferrozine_test/ferrozine_test_MAP.csv") %>% janitor::clean_names()
  
  # import data files (plate reader)
  filePaths_ferrozine <- list.files(path = FILEPATH, pattern = ".xlsx", full.names = TRUE, recursive = TRUE)
  ferrozine_data <- do.call(bind_rows, lapply(filePaths_ferrozine, function(path) {
    df <- readxl::read_excel(path, skip = 25) %>% mutate_all(as.character) %>% janitor::clean_names()
    df = df %>% mutate(source = basename(path))
    df}))
  
  list(ferrozine_map = ferrozine_map,
       ferrozine_data = ferrozine_data)
  
}
process_iron = function(ferrozine_map, ferrozine_data){
  
  # clean the map
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
  
  # clean the data
  data_processed = 
    ferrozine_data %>% 
    mutate_all(na_if,"") %>% 
    fill(x1) %>% 
    filter(x14 == "562") %>% 
    rename(x = x1,
           x1 = x1_2) %>% 
    pivot_longer(-c(source, x), values_to = "absorbance_562") %>% 
    mutate(name = str_remove(name, "x"),
           well_position = paste0(x, name),
           date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           date = ymd(date),
           tray = str_extract(source, "plate[1-9][a-z]?"),
           tray = parse_number(tray, "plate"),
           absorbance_562 = as.numeric(absorbance_562)) %>% 
    dplyr::select(date, tray, well_position, absorbance_562) %>% 
    right_join(map_processed, by = c("date", "tray", "well_position")) %>% 
    filter(!grepl("skip", sample_label))
  
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
    reduction_efficiency = 
      standards %>% filter(standard_mM == 2) %>% 
      group_by(date, standard_type) %>% 
      dplyr::summarize(abs = mean(absorbance_562)) %>% 
      pivot_wider(names_from = "standard_type", values_from = "abs") %>% 
      mutate(red_eff = 100 * FeCl3/FAS) %>% 
      dplyr::select(-FAS, -FeCl3)
    # 99.6 % efficiency

    gg_calibration = 
      standards %>% 
      ggplot(aes(x = standard_ppm, y = absorbance_562, color = as.character(tray)))+
      geom_point()+
      geom_smooth(method = "lm", se = F)+
      facet_wrap(~date + tray)
    
    calibration_coef = 
      standards %>% 
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
    group_by(sample_label) %>% 
    pivot_wider(names_from = "analysis", values_from = "ppm_calculated") %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    left_join(reduction) %>% 
    mutate(Fe_total = Fe_total * 100/red_eff,  # correct for 99.6% reduction efficiency of ascorbic acid
           Fe3 = Fe_total - Fe2) %>% 
    rename(Fe2_ppm = Fe2,
           Fe3_ppm = Fe3,
           FeTotal_ppm = Fe_total) %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    ungroup() %>% 
    dplyr::select(-date, -red_eff)

}
