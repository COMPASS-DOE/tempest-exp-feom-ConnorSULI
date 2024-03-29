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
  subset(select = -rundate.y)


#Function for unit conversion and calculating conversion
#conversion must happen after dilution
 

Cond_conversion_action <- Chemical_dilutions %>% 
  filter(grepl("W", ID)) %>% # filter to samples only
  filter(grepl("Conversion", Action_2)) %>%
  bind_rows()


Cond_conversion = 
  Cond_conversion_action %>%
  mutate(Conversion = Dilution *1000) %>%
  dplyr::select(rundate, ID, Action_2, Conversion)

Cond_conversion2 = 
  Cond_conversion_action %>%
  mutate(Conversion = Cond *1000) %>%
  dplyr::select(rundate, ID, Action_2, Conversion)

Cond_conversion <- merge(Cond_conversion,Cond_conversion2, by = "ID", all = TRUE)

#df$Age <- ifelse(is.na(df$NewAge), df$Age, df$NewAge)

#subset(select = -c(rundate.x, Action.x, Action.y, Cond))%>%
 # mutate(rundate = rundate.y) %>%
  #subset(select = -rundate.y)


Chemical_all <-merge(Cond_conversion, Chemical_dilutions, by = "ID", all = TRUE)











 

          
