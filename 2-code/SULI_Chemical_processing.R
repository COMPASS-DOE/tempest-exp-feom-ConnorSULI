# Chemical Processing and summary data
#Connor O'Loughlin
#Advisor: Allison Myers-Pigg
#Created: 3/20/24
#Updated: 3/29/24
#TEMPEST: Fe-OM mechanism 




library(tidyverse)
library(pacman)
library(openxlsx)

getwd()
setwd("C:/Users/olou646/tempest-exp-feom-ConnorSULI")


#Read in Data for chemical data
Raw_Chemical<-read_csv("1-data/TMP_Cond_Files/Raw_TMP_FeOM_Chemical.csv")


#Function to adjust for dilutions and calculating dilution
Chemical_dilution_action <- Raw_Chemical %>% 
  filter(grepl("W", ID)) %>% # filter to samples only
  filter(grepl("Dilution correction", Action)) %>%
  bind_rows() 

 
Cond_dilutions = 
  Chemical_dilution_action %>% 
  mutate(Dilution =  volume/sample*Cond) %>% 
  dplyr::select(rundate, ID, Action, Dilution) %>% 
  force()



Chemical_dilutions<-merge(Cond_dilutions, Raw_Chemical, by = "ID", all = TRUE) %>%
  subset(select = -c(rundate.x, Action.x, Action.y))%>%
  mutate(rundate = rundate.y) %>%
  subset(select = -rundate.y)%>%
  mutate( Cond = ifelse(is.na(Dilution),Cond, Dilution))%>%
  subset(select = -c(sample,
                     volume,
                     Dilution))




#Function for unit conversion and calculating conversion
#conversion must happen after dilution
 

Cond_conversion_action <-Chemical_dilutions %>% 
  filter(grepl("W", ID)) %>% # filter to samples only
  filter(grepl("Conversion", Action_2)) %>%
  bind_rows()

Cond_conversion = 
  Cond_conversion_action %>%
  mutate(Conversion = Cond /1000) %>%
  dplyr::select(rundate, ID, Action_2, Conversion)

  Cond_estimates<-Cond_conversion%>%
    mutate(units = "mS")%>%
    subset(select = -Action_2)

  
#Merging back with O2 data

Chemical_estimates <-full_join(Cond_estimates, Raw_Chemical, by = "ID")%>%
  mutate(Cond = ifelse(is.na(Conversion),Cond, Conversion))%>%
  mutate(rundate = rundate.y)%>%
  subset(select = -c(Conversion,
                     units.x,
                     units.y,
                     rundate.x,
                     rundate.y,
                     Action,
                     Action_2,
                     sample,
                     volume))%>%
  mutate( units = "mS")

#TDS
Chemical_all<- Chemical_estimates%>%
  mutate("Salinity (g/L)"  = 0.4665*(Cond^1.0878))%>%
  mutate(Treatment = stringr::str_extract(ID, "[a-zA-Z]+(?=\\d)"),
         Wash = stringr::str_extract(ID, "\\d+(?=\\.)"),
         Fraction = stringr::str_extract(ID, "(?<=\\.)\\d+(?=\\.)")
         #stringr::str_extract(sample_name, "(?<=\\.[a-zA-Z].)\\d+")
  ) %>%
  mutate(Treatment = case_when(is.na(Treatment) ~ "AW",
                               TRUE ~ Treatment),
         Wash = case_when(is.na(Wash) ~ "1",
                          TRUE ~ Wash),
         Fraction = case_when(Fraction == "01" ~ "0.1",
                              Fraction == "45" ~ "0.45",
                              is.na(Fraction) ~ "Blank",
                              TRUE ~ Fraction),
         Group = paste(Treatment, Fraction, sep= " ") )

#Exporting data
write_csv(Chemical_all, "../tempest-exp-feom-ConnorSULI/1-data/Summary/Salinity_summary.csv")










 

          
