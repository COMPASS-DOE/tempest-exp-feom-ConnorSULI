#Connor's SULI project. Spring 2024
#Adivosr: Allison Myers-Pigg
#





#Setting wd and getting packages
setwd("C:/Users/olou646/")
file_path<- "C:/Users/olou646"
library(tidyverse)
library(pacman)
install.packages("openxlsx")
library(openxlsx)
library(readr)
library(devtools)
install.packages("devtools")
install_github("katiewampler/fewsdom")
library(Rtools)
install.packages("rtools")

#Reading data for soil
df<-read_csv("Practice_data.csv")

#Finding standard deviations
sd_df<-read_csv("Practice_data.csv")%>%
  group_by(water_type, depth_cm)%>%
  summarize(sd_Fe_ppm = sd(Fe_ppm))

#df<-sd_Fe_ppm[(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113),]

#five_df<-df[c(2,10,18,26,34,42,50,58,66,74,82,90,98,106,114),]

#ten_df<-df[c(3,11,19,27,35,43,51,59,67,75,83,91,99,107,115),]

#fifteen_df<-df[c(4,12,20,28,36,44,52,60,68,76,84,92,100,108,116),]

#twenty_df<-df[c(5,13,21,29,37,45,53,61,69,77,85,93,101,109,117),]

#twen_fiv_df<-df[c(6,14,22,30,38,46,54,62,70,78,86,94,102,110,118),]

#thirt_df<-df[c(7,15,23,31,39,47,55,63,71,79,87,95,103,111,119),]

#fort_df<-df[c(8,16,24,32,40,48,56,64,72,80,88,96,104,112,120),]

#finding means
avg_df<-aggregate(df$Fe_ppm, by = list(df$water_type), FUN = mean)
result <- df %>%
  group_by(depth_cm, water_type)%>%
  summarize(avg_Fe_ppm = mean(Fe_ppm), avg_Ca_ppm = mean(Ca_ppm))
  

#Creating a data table based on water type and soil depth
result<- merge(result, sd_df, by = c("water_type", "depth_cm"))

ggplot(data =result, mapping = aes(x = depth_cm,y = avg_Fe_ppm, color = water_type)) + 
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = avg_Fe_ppm-sd_Fe_ppm,ymax = avg_Fe_ppm+sd_Fe_ppm), width = 0.2)+
  labs(title = "Iron concentraition versus soils depth", xlab = "Depth (cm)", ylab = "[Fe] (ppm)")+
  theme_minimal()


#I cannot remember what this code pertains to
SPE<-read.csv("SPE_TMP.csv")
DOC<-read.csv("DOC_TMP_2.CSV")
ID_<-read.csv("Sample_ID.CSV")
result<- merge(SPE, DOC, by = c("Short_ID"))
result<- merge(result,ID_, by = c("Short_ID"))

write.xlsx(result,file_path,sheetName="DOC_SPE",rownames=FALSE)



#Code related to Conducitity measurements for TEMPEST 
Conductivity_TMP<-read.csv("Conductivity_data_TMP.csv")

Dilution_estimates<-Conductivity_TMP %>%
 mutate(Dilution_Estimate = ifelse(Diluted == "no",
                                     Conductivity,
                                     Conductivity * 4))
f_Dilution_estimates<-Dilution_estimates %>%
 mutate(units_uS = ifelse(units == "uS",
                    Dilution_Estimate,
                    Dilution_Estimate * 1000))
 

write.xlsx(f_Dilution_estimates,file_path,
           sheetName="Conductivity_Estimates_TMP",
           rownames=FALSE)      


#Prepping TOC Metadata
TMP_1 <- "/RAW_TMP_FeOM_TOC/20240307_Data_Summary_TMP_FEOM_CO_1"
TMP_1 <- read_lines(TMP_1)
