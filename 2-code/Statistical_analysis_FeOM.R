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

getwd()
setwd("C:/Users/olou646/tempest-exp-feom-ConnorSULI")


#Load in data
TOC_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/TMP_FeOM-TOC_All_data.csv")
Fe_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Ferrozine_summary.csv")%>%
  mutate(sample_name = id)%>%
subset(select = -id)

#multivariate corrleation
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
                     Fraction.x))



