# Stats and final figures
#Connor O'Loughlin
#Advisor: Allison Myers-Pigg
#Created: 3/29/24
#TEMPEST: Fe-OM mechanism 

#Loading Packages and setting wd
library(tidyverse)
library(pacman)
library(openxlsx)
library(magrittr)
library(vegan)
library(permute)
library(lattice)
library(dplyr)
library(readxl)
library(ggplot2)



getwd()
setwd("C:/Users/olou646/tempest-exp-feom-ConnorSULI")


#Load in data
TOC_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/TMP_FeOM-TOC_All_data.csv")
Fe_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Ferrozine_summary_Normalized.csv")
Chemical_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Soil_chemical_data.csv")
Salinity_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Salinity_summary.csv")%>%
  mutate(sample_name = ID)%>%
  subset(select = -ID)
                                                    
  
#Finalizing data normalization
correlation_matrix1 <- merge(Fe_data, TOC_data, by = "sample_name")%>%
  mutate(Treatment = Treatment.x,
         Wash = Wash.x,
         Fraction = Fraction.x,
         Group = Group.x)%>%
  subset(select = -c(Wash.y,
                     Treatment.y,
                     Group.y,
                     Fraction.y,
                     Treatment.x,
                     Wash.x,
                     Group.x,
                     Fraction.x,
                     ferrozine_id,
                     X,
                     X.1,
                     X.2,
                     X.3,
                     X.4,
                     X.5,
                     X.6,
                     X.7,
                     X.8))

correlation_matrix2 <- left_join(Salinity_data, correlation_matrix1, by = "sample_name")%>%
  mutate(Treatment = Treatment.x,
         Wash = Wash.x,
         Fraction = Fraction.x,
         Group = Group.x)%>%
  subset(select = -c(Wash.y,
                     Treatment.y,
                     Group.y,
                     Fraction.y,
                     Treatment.x,
                     Wash.x,
                     Group.x,
                     Fraction.x))


Cor_matrix <- correlation_matrix2 %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
mutate(mean_doc_mg_l = mean(doc_mg_l),
       sd_doc_mg_l = sd(doc_mg_l),
       mean_Fe2_ppm = mean(Fe2_ppm,),
       sd_Fe2_ppm = sd(Fe2_ppm),
       mean_Fe3_ppm = mean(Fe3_ppm),
       sd_Fe3_ppm = sd(Fe3_ppm),
       mean_FeTotal_ppm = mean(FeTotal_ppm),
       sd_FeTotal_ppm = sd(FeTotal_ppm),
       `mean_Fe2_mg/g` = mean(Fe2_mg.g),
       `sd_Fe2_mg/g` = sd(Fe2_mg.g),
       `mean_Fe3_mg/g` = mean(Fe3_mg.g),
       `sd_Fe3_mg/g` = sd(Fe3_mg.g),
       `mean_FeTotal_mg/g` = mean(FeTotal_mg.g),
       `sd_FeTotal_mg/g` = sd(FeTotal_mg.g),
       mean_Fe2_mg = mean(Fe2_mg),
       sd_Fe2_mg = sd(Fe2_mg),
       mean_FeTotal_mg = mean(FeTotal_mg),
       sd_FeTotal_mg = sd(FeTotal_mg),
       `mean_O2%` = mean(O2_.),
       `sd_O2%` = sd(O2_.),
       mean_Cond_mS = mean(Cond),
       sd_Cond_mS = sd(Cond),
       `mean_salinity_g/L` = mean(Salinity..g.L.),
       `sd_salinity_g/L` = sd(Salinity..g.L.))

write_csv(Cor_matrix, "../tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/Fraction subatraction.csv")

       


# Step 2: Calculate the Fractions
unique_rows <- Cor_matrix %>%
  distinct(Group, Fraction, Treatment, Wash, .keep_all = TRUE)%>%
  filter(Fraction != "Blank")

Blanks <- Cor_matrix%>%
  filter(Fraction == "Blank")


#I was getting tired of figuring this out in r so I am doing some things in excel to make this go faster
write_csv(unique_rows, "../tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/Fraction_subtraction.csv")
write_csv(Blanks,"../tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/Blank_subtraction.csv")

#This File is all of the data merged together and the Fe and DOC data have been calculated for each fraction  
#I did this in excel. I used the Fraction_subtraction and Blank_subtraction files and made this new file
#
Adjusted_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/All_adjusted.csv")%>%
  mutate(`[DOC]` = X.DOC.,
         `[Fe2]_correct` = X.Fe2._correct,
         `[Fe3]_correct` = X.Fe3._correct,
         `[Fe]_correct` = X.Fe.,
         `OC:Fe2` = OC.Fe2,
         `OC:Fe3` = OC.Fe3,
         `OC:FeTotal` = OC.FeTotal)%>%
  subset(select = -c( X.DOC.,
                     X.Fe2._correct,
                     X.Fe3._correct,
                     X.Fe.,
                     OC.Fe2,
                     OC.Fe3,
                     OC.FeTotal))


treatment_order <- c('0.1','0.45','1', "Blank")
line_path_order <- c("AW 0.1", "AW 0.45", "AW 1", "OW 0.1", "OW 0.45", "OW 1", "AW Blank", "OW") 

Adjusted_data%>%
ggplot()

Adjusted_data%>%
  group_by(Fraction, Treatment, Wash, Group)%>%
ggplot()+
  geom_pointrange(aes(x=Wash, y=DOC_corrected, ymin = DOC_corrected- sd_doc_mg_l, ymax = DOC_corrected + sd_doc_mg_l, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=DOC_corrected, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "DOC mgC/L", color = "Size Fraction (um)")




  
 
 


