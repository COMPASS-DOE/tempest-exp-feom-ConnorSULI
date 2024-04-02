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



getwd()
setwd("C:/Users/olou646/tempest-exp-feom-ConnorSULI")


#Load in data
TOC_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/TMP_FeOM-TOC_All_data.csv")
Fe_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Ferrozine_summary_Normalized.csv")
  
#Finalizing data normalization
correlation_matrix <- left_join(Fe_data, TOC_data, by = "sample_name")%>%
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

Cor_matrix <- correlation_matrix %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
mutate(mean_doc_mg_l = mean(doc_mg_l),
       sd_doc_mg_l = sd(doc_mg_l))


# Step 2: Calculate the Fractions
unique_rows <- Cor_matrix %>%
  distinct(Group, Fraction, Treatment, Wash, .keep_all = TRUE)
 
 DOC_adjusted <- unique_rows %>%
   group_by(Wash, Treatment, Fraction, Group) %>%
   mutate("0.45-0.1" = sum(ifelse(Fraction == "0.1", mean_doc_mg_l, 0)))%>%
   mutate("1.0-0.45" = sum(ifelse(Fraction == "0.45", mean_doc_mg_l-`0.45-0.1`, 0)))%>%
   mutate( ">1.0" = sum(ifelse(Fraction == "1", mean_doc_mg_l-(`1.0-0.45` + `0.45-0.1`), 0)))

  
 
 
# Step 3: Combine the Results
combined_data <- filtered_data %>%
  distinct(Wash, Treatment) %>%
  left_join(calculated_data, by = c("Wash", "Treatment"))

