setwd("C:/Users/olou646/")
file_path<- "C:/Users/olou646"
library(tidyverse)
library(pacman)
install.packages("openxlsx")
library(openxlsx)

getwd()
setwd("C:/Users/olou646/tempest-exp-feom-ConnorSULI")


#Read in Data for chemical data
Raw_Chemical<-read_csv("1-data/TMP_Cond_Files/Raw_TMP_FeOM_Chemical.csv")


#Functions to adjust for dilutions and unit conversions
Chemical_dilution_action <- Raw_Chemical %>% 
  filter(grepl("W", ID)) %>% # filter to samples only
  filter(grepl("Dilution correction", Action)) %>%
  bind_rows() 

Cond_conversion_action <- Raw_Chemical %>% 
  filter(grepl("W", ID)) %>% # filter to samples only
  filter(grepl("Conversion", Action_2)) %>%
  bind_rows() 



#Calculating dilutions and  unit conversions.
#conversion must happen after dilution
 
Cond_dilutions = 
  Chemical_dilution_action %>% 
  mutate(Dilution =  volume/sample) %>% 
  dplyr::select(rundate, ID, Action, Dilution) %>% 
  force()


Cond_conversion = 
  Cond_conversion_action %>%
  mutate(Conversion = Cond *1000) %>%
  dplyr::select(rundate, ID, Action_2, Conversion)


Chemical_flagged <- Cond_dilutions %>% 
  filter(grepl("W", ID)) %>% # filter to samples only
  inner_join(Cond_dilutions, by = "rundate") %>% 
  inner_join(curvepts, by= "rundate") %>%

samples_to_dilution_corrected = 

  left_join(dilutions, by = c("ID", "rundate")) %>% 
  filter(grepl("Dilution correction", Action)) %>%
  filter(!Action %in% "Omit") %>% 
  mutate(doc_mg_l= npoc_raw * Dilution, # True concentration = diluted concentration * total vol / sample vol
         doc_mg_l = as.numeric(doc_mg_l), doc_mg_l = round(doc_mg_l, 2),
  ) %>%
  mutate(doc_mg_l = case_when(Dilution > 30 & npoc_flag == "blank is ≥ 15% of sample value" ~ NA,
                              TRUE ~ doc_mg_l), # removing values if high blanks and high dilution ratios, potentially large source of error. 
         npoc_flag = case_when(is.na(doc_mg_l) ~ "omitted for high dilution and blank values",
                               TRUE ~ npoc_flag),
  ) # removing values if high blanks and high dilution ratios, potentially large source of error.









 

write.xlsx(f_Dilution_estimates,file_path,
           sheetName="Conductivity_Estimates_TMP",
           rownames=FALSE)                     
