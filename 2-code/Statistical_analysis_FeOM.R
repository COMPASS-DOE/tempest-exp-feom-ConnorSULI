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
library(ggpubr)
library(corrplot)

getwd()
setwd("C:/Users/olou646/tempest-exp-feom-ConnorSULI")


#Load in data
#TOC_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/TMP_FeOM-TOC_All_data.csv")
#Fe_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Ferrozine_summary_Normalized.csv")
#Chemical_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Soil_chemical_data.csv")
#Salinity_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Salinity_summary.csv")%>%
  mutate(sample_name = ID)%>%
  subset(select = -ID)

#loading files not computer specific
TOC_data <- read.csv("./1-data/Summary/TMP_FeOM-TOC_All_data.csv")
Fe_data <- read.csv("./1-data/Summary/Ferrozine_summary_Normalized.csv") %>%
  select(sample_name:Fe3_mg.g)
Chemical_data <- read.csv("./1-data/Summary/Soil_chemical_data.csv")
Salinity_data <- read.csv("./1-data/Summary/Salinity_summary.csv")%>%
  mutate(sample_name = ID)%>%
  subset(select = -ID) %>%
  rename(exp_date = rundate)
EEMs_Fe_corrected<- read.csv("./1-data/Summary/EEMs_Fe_corrected.csv")
                                                    
#maybe a simpler way to merge and calculate stuff....

fe_MW= 55.845
C_MW = 12.01

Cor_matrix <-  TOC_data %>%
  left_join(Fe_data) %>%
  left_join(Salinity_data) %>%
  group_by(Fraction, Treatment, Wash, Group) %>%
  mutate(Fe_tot_mg.g = Fe2_mg.g + Fe3_mg.g,
         Fe2_mmolL = Fe2_ppm /fe_MW,
         Fe3_mmolL = Fe3_ppm / fe_MW,
         doc_mmolL = doc_mg_l / C_MW,
         Fe_tot_mmolL = Fe2_mmolL + Fe3_mmolL,
         Fe.OC = Fe_tot_mmolL/doc_mmolL) %>%
  select(sample_name, Treatment, Wash, Fraction, Group, doc_mg_l, Fe2_ppm:Cond, `Salinity..g.L.`:Fe.OC)%>%
  mutate_if(is.double, as.numeric) %>%
  summarise_if(is.numeric, list(mean= ~mean(., na.rm=TRUE), sd = ~sd(., na.rm=TRUE)))%>%
  left_join(EEMs_Fe_corrected)%>%
  mutate(SUVA254 = (a254_corrected/doc_mg_l_mean)*100)
  mutate(case_when()) #if there are NAs in the data, it might make s

#Finalizing data normalization
# correlation_matrix1 <- merge(Fe_data, TOC_data, by = "sample_name")%>%
#   mutate(Treatment = Treatment.x,
#          Wash = Wash.x,
#          Fraction = Fraction.x,
#          Group = Group.x)%>%
#   subset(select = -c(Wash.y,
#                      Treatment.y,
#                      Group.y,
#                      Fraction.y,
#                      Treatment.x,
#                      Wash.x,
#                      Group.x,
#                      Fraction.x,
#                      ferrozine_id,
#                      X,
#                      X.1,
#                      X.2,
#                      X.3,
#                      X.4,
#                      X.5,
#                      X.6,
#                      X.7,
#                      X.8))

# correlation_matrix2 <- left_join(Salinity_data, correlation_matrix1, by = "sample_name")%>%
#   mutate(Treatment = Treatment.x,
#          Wash = Wash.x,
#          Fraction = Fraction.x,
#          Group = Group.x)%>%
#   subset(select = -c(Wash.y,
#                      Treatment.y,
#                      Group.y,
#                      Fraction.y,
#                      Treatment.x,
#                      Wash.x,
#                      Group.x,
#                      Fraction.x))


# Cor_matrix <- correlation_matrix2 %>%
#   group_by(Fraction, Treatment, Wash, Group)%>%
# mutate(mean_doc_mg_l = mean(doc_mg_l),
#        sd_doc_mg_l = sd(doc_mg_l),
#        mean_Fe2_ppm = mean(Fe2_ppm,),
#        sd_Fe2_ppm = sd(Fe2_ppm),
#        mean_Fe3_ppm = mean(Fe3_ppm),
#        sd_Fe3_ppm = sd(Fe3_ppm),
#        mean_FeTotal_ppm = mean(FeTotal_ppm),
#        sd_FeTotal_ppm = sd(FeTotal_ppm),
#        `mean_Fe2_mg/g` = mean(Fe2_mg.g),
#        `sd_Fe2_mg/g` = sd(Fe2_mg.g),
#        `mean_Fe3_mg/g` = mean(Fe3_mg.g),
#        `sd_Fe3_mg/g` = sd(Fe3_mg.g),
#        `mean_FeTotal_mg/g` = mean(FeTotal_mg.g),
#        `sd_FeTotal_mg/g` = sd(FeTotal_mg.g),
#        mean_Fe2_mg = mean(Fe2_mg),
#        sd_Fe2_mg = sd(Fe2_mg),
#        mean_FeTotal_mg = mean(FeTotal_mg),
#        sd_FeTotal_mg = sd(FeTotal_mg),
#        `mean_O2%` = mean(O2_.),
#        `sd_O2%` = sd(O2_.),
#        mean_Cond_mS = mean(Cond),
#        sd_Cond_mS = sd(Cond),
#        `mean_salinity_g/L` = mean(Salinity..g.L.),
#        `sd_salinity_g/L` = sd(Salinity..g.L.))

# write_csv(Cor_matrix, "../tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/Fraction subatraction.csv")

       


# Step 2: Calculate the Fractions
# unique_rows <- Cor_matrix %>%
#   distinct(Group, Fraction, Treatment, Wash, .keep_all = TRUE)%>%
#   filter(Fraction != "Blank")
# 
# Blanks <- Cor_matrix%>%
#   filter(Fraction == "Blank")
# 

#from AMP: I have no idea what you did here so it's hard for me to follow this... 
#is this where you did the blank and fraction subtractions and you did this from the group means?? 

#if so, here's an R suggestion:

corrected_all <- Cor_matrix %>%
  filter(!Fraction == "Blank") %>%
  arrange(Treatment, Wash, Fraction) %>% # Ensure data is in correct order
  group_by(Treatment, Wash) %>% # Group by treatment and wash
  mutate_all(~replace(., is.nan(.), NA)) %>%
  mutate_at(vars(doc_mg_l_mean:Fe3_mg.g_mean, Fe_tot_mg.g_mean:Fe.OC_mean, doc_mg_l_sd:Fe3_mg.g_sd,Fe_tot_mg.g_sd:Fe.OC_sd),~ if_else(row_number() == 1, ., . - lag(., default=first(.), order_by = Fraction))) %>% # Subtract fraction from previous row for the variables it makes sense for only
  mutate_all(~pmax(., 0)) #if the fractions are smaller than the error/variability then we get a neg number. turn these into 0s

treatment_order <- c('0.1','0.45','1', "Blank")
line_path_order <- c("AW 0.1", "AW 0.45", "AW 1", "OW 0.1", "OW 0.45", "OW 1", "AW Blank", "OW Blank") 


#DOC plot
corrected_all %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  ggplot()+
  geom_pointrange(aes(x=Wash, y=doc_mg_l_mean, ymin = doc_mg_l_mean- doc_mg_l_sd, ymax = doc_mg_l_mean + doc_mg_l_sd,
                      color= factor(Fraction, levels= treatment_order), shape =Treatment), size = 1.5) +
  geom_path(aes(x=Wash, y=doc_mg_l_mean, color= factor(Fraction, levels= treatment_order),
                group=factor(Group, levels= line_path_order)),lwd=1.5) +
  theme_classic() +
  labs(x = "Wash", y = "DOC mgC/L",title = "DOC release over time", color = "Size Fraction (um)", size = 28)+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))+
  theme(axis.text = element_text(size = 26), # Increase font size of axis text
        axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 26),
        title = element_text(size = 26)) # Increase font size of y-axis label

#DOC plot with just OW treatment
  corrected_all %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  filter(Treatment == "OW")%>%
  ggplot()+
  geom_pointrange(aes(x=Wash, y=doc_mg_l_mean, ymin = doc_mg_l_mean- doc_mg_l_sd, ymax = doc_mg_l_mean + doc_mg_l_sd,
                      color= factor(Fraction, levels= treatment_order), shape =Treatment), size = 1.5) +
  geom_path(aes(x=Wash, y=doc_mg_l_mean, color= factor(Fraction, levels= treatment_order),
                group=factor(Group, levels= line_path_order)),lwd=1.5) +
  theme_classic() +
  labs(x = "Wash", y = "DOC mgC/L",title = "DOC release over time", color = "Size Fraction (um)", size = 28)+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))+
  theme(axis.text = element_text(size = 26), # Increase font size of axis text
        axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 26),
        title = element_text(size = 26)) # Increase font size of y-axis label


corrected_all %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  ggplot()+
  geom_pointrange(aes(x=Wash, y=doc_mmolL_mean, ymin = doc_mmolL_mean- doc_mmolL_sd, ymax = doc_mmolL_mean + doc_mmolL_sd, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=doc_mmolL_mean, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "DOC mM", color = "Size Fraction (um)")+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))

#Fe plot
corrected_all %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  ggplot()+
  geom_pointrange(aes(x=Wash, y=Fe3_mg.g_mean, ymin = Fe3_mg.g_mean- Fe3_mg.g_sd, ymax = Fe3_mg.g_mean + Fe3_mg.g_sd, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=Fe3_mg.g_mean, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "Fe 3+ mg/g", color = "Size Fraction (um)")+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))

corrected_all %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  ggplot()+
  geom_pointrange(aes(x=Wash, y=Fe_tot_mmolL_mean, ymin = Fe_tot_mmolL_mean- Fe_tot_mmolL_sd, ymax = Fe_tot_mmolL_mean + Fe_tot_mmolL_sd, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=Fe_tot_mmolL_mean, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "Total Fe mM", color = "Size Fraction (um)")+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))


#Fe:OC
corrected_all %>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  ggplot()+
  geom_pointrange(aes(x=Wash, y=Fe.OC_mean, ymin = Fe.OC_mean- Fe.OC_sd, ymax = Fe.OC_mean + Fe.OC_sd, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=Fe.OC_mean, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "Total Fe: OC (molar ratio)", color = "Size Fraction (um)")+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))

corrected_all%>%
  mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  group_by(Fraction, Treatment, Wash, Group)%>%
  ggplot()+
  geom_point(aes(x=Wash, y=`Fe2/Fe3`, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=Fe.OC_mean, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "Total Fe: OC (molar ratio)", color = "Size Fraction (um)")+
  scale_shape_manual(values = c("AW" = 1, "OW" = 19))



#Box Plot with Fe:OC broken up by wash and Fraction
corrected_all %>%
  filter(Treatment == "OW")%>%
#  mutate(`Fe.OC_mean` = ifelse(`Fe.OC_mean` < 0, 0, `Fe.OC_mean`))%>%
  ggplot(aes(x = Wash, y= `Fe.OC_mean`)) +
  geom_bar(stat= "identity", color = "Black", fill = "Red", alpha = 0.7, position = "dodge") +
  facet_grid(. ~ Fraction) +
  labs(x = "Wash", y = "Fe:OC", 
       title = "Fe:OC Molar Ratio by Wash and Fraction") +
  theme_classic()+
  theme(axis.text = element_text(size = 26), # Increase font size of axis text
                       axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
                       axis.title.y = element_text(size = 26),
        title = element_text(size = 26)) # Increase font size of y-axis label

#Boxplot of Fe mmol/L 
corrected_all %>%
  mutate(Fe_tot_mmolL_mean = ifelse(Fe_tot_mmolL_mean < 0, 0, Fe_tot_mmolL_mean))%>% # Making all data below 1 beomce 0.%>%
  filter(Treatment == "OW")%>%
  ggplot(aes(x = Wash, y= Fe_tot_mmolL_mean)) +
  geom_bar(stat= "identity", color = "Black", fill = "Skyblue", alpha = 0.7) +
  facet_grid(. ~ Fraction) + # Making 3 separate graphs based upon Fraction size
  labs(x = "Wash", y = "Fe mmol/L", 
       title = "Fe mmol/L by Wash and Fraction")+
  theme_classic()+
  theme(axis.text = element_text(size = 26), # Increase font size of axis text
        axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 26),
        title = element_text(size = 26)) # Increase font size of y-axis label


#Bar plot of Fe2 grouped by fraction and wash
corrected_all %>%
  mutate(Fe2_mmolL_mean = ifelse(Fe2_mmolL_mean < 0, 0, Fe2_mmolL_mean))%>% # Making all data below 1 beomce 0.%>%
  filter(Treatment == "OW")%>%
  ggplot(aes(x = Wash, y= Fe2_mmolL_mean)) +
  geom_bar(stat= "identity", color = "Black", fill = "Skyblue", alpha = 0.7) +
  facet_grid(. ~ Fraction) + # Making 3 separate graphs based upon Fraction size
  labs(x = "Wash", y = "Fe2 mmol/L", 
       title = "Fe2 mmol/L by Wash and Fraction")+
  theme_classic()+
theme(axis.text = element_text(size = 26), # Increase font size of axis text
 axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
axis.title.y = element_text(size = 26)) # Increase font size of y-axis label

#Bar Graph pf Fe3 grouped by Fraction and wash
corrected_all %>%
  mutate(Fe3_mmolL_mean = ifelse(Fe3_mmolL_mean < 0, 0, Fe3_mmolL_mean))%>% # Making all data below 1 beomce 0.%>%
  filter(Treatment == "OW")%>%
  ggplot(aes(x = Wash, y= Fe3_mmolL_mean)) +
  geom_bar(stat= "identity", color = "Black", fill = "Skyblue", alpha = 0.7) +
  facet_grid(. ~ Fraction) + # Making 3 separate graphs based upon Fraction size
  labs(x = "Wash", y = "Fe3 mmol/L", 
       title = "Fe3 mmol/L by Wash and Fraction")+
  theme_classic()+
theme(axis.text = element_text(size = 26), # Increase font size of axis text
 axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
axis.title.y = element_text(size = 26)) # Increase font size of y-axis label


#bar graph of DOC grouped by Fraction and wash
corrected_all %>%
  mutate(doc_mmolL_mean = ifelse(doc_mmolL_mean < 0, 0, doc_mmolL_mean))%>% # Making all data below 1 beomce 0.%>%
  filter(Treatment == "OW")%>%
  ggplot(aes(x = Wash, y= doc_mmolL_mean)) +
  geom_bar(stat= "identity", color = "Black", fill = "Skyblue", alpha = 0.7) +
  facet_grid(. ~ Fraction) + # Making 3 separate graphs based upon Fraction size
  labs(x = "Wash", y = "DOC mmol/L", 
       title = "DOC mmol/L by Wash and Fraction")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), # Increase font size of axis text
        axis.title.x = element_text(size = 20),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 20)) # Increase font size of y-axis label


#bar graph of Fe2/2Fe3 grouped by Fraction and wash
corrected_all %>%
  mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  #mutate(doc_mmolL_mean = ifelse(doc_mmolL_mean < 0, 0, doc_mmolL_mean))%>% # Making all data below 1 beomce 0.%>%
  filter(Treatment == "OW")%>%
  ggplot(aes(x = Wash, y= `Fe2/Fe3`, ymax = 5)) +
  geom_bar(stat= "identity", color = "Black", fill = "Forest Green", alpha = 0.7) +
  facet_grid(. ~ Fraction) + # Making 3 separate graphs based upon Fraction size
  labs(x = "Wash", y = "Fe2:Fe3", 
       title = "Fe2:Fe3 by Wash and Fraction")+
  theme_classic()+
  theme(axis.text = element_text(size = 26), # Increase font size of axis text
        axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 26),
          title = element_text(size = 26))# Increase font size of y-axis label


corrected_all %>%
  #mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  #mutate(doc_mmolL_mean = ifelse(doc_mmolL_mean < 0, 0, doc_mmolL_mean))%>% # Making all data below 1 beomce 0.%>%
  filter(Treatment == "OW")%>%
  ggplot(aes(x = Wash, y= SUVA254)) +
  geom_bar(stat= "identity", color = "Black", fill = "Skyblue", alpha = 0.7) +
  facet_grid(. ~ Fraction) + # Making 3 separate graphs based upon Fraction size
  labs(x = "Wash", y = "SUVA254", 
       title = "SUVA254 by Wash and Fraction")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), # Increase font size of axis text
        axis.title.x = element_text(size = 20),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 20)) # Increase font size of y-axis label


#Linear correlation with Fe corrected a254 and Fe
corrected_all %>%
  ggplot(aes(x = Fe_tot_mmolL_mean, y = a254_corrected)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_regline_equation()+
  stat_cor(label.y = 35)+# Adjust label position as needed
  labs(x = "Fe mmol L-1", y = "a254",
       title = "Linear Regression of Total Fe and a254")+
  theme_classic()+
  theme(axis.text = element_text(size = 16), # Increase font size of axis text
        axis.title.x = element_text(size = 16),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 16))


#Linear Regression with DOC mmol/L and  Total Fe
corrected_all %>%
  ggplot(aes(x = Fe_tot_mmolL_mean, y = doc_mmolL_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_regline_equation()+
  stat_cor(label.y = 35)+# Adjust label position as needed
  labs(x = "Fe mmol L-1", y = "DOC mmol/L",
       title = "Linear Regression of Total Fe and DOC")+
  theme_classic()+
  theme(axis.text = element_text(size = 16), # Increase font size of axis text
        axis.title.x = element_text(size = 16),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 16)) # Increase font size of y-axis label
  


#Linear Regression with SUVA and Total Fe
corrected_all %>%
  ggplot(aes(x = Fe_tot_mmolL_mean, y = SUVA254)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, lwd = 1.5) +
  stat_regline_equation(label.y = 45, size = 12)+
  stat_cor(label.y = 40, size = 12)+# Adjust label position as needed
  labs(x = "Fe mmol L-1", y = "SUVA254",
       title = "Linear Regression of Total Fe and SUVA254", size = 36)+
  theme_classic()+
  theme(axis.text = element_text(size = 26), # Increase font size of axis text
        axis.title.x = element_text(size = 26),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 26),
        title = element_text(size = 26)) # Increase font size of y-axis label
 

#Linear Reg for Fe2/Fe3 and O2%
corrected_all %>%
  mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  ggplot(aes(y =`Fe2/Fe3` , x = O2_._mean)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, lwd = 1.5) +
  stat_regline_equation(label.y = 2, label.x = 50, size = 14)+
  stat_cor(label.y = 1.5,label.x = 50, size = 14)+# Adjust label position as needed
  labs(x = "O2 % ", y = "Fe2:Fe3 %",
       title = "Linear Regression of Fe2:Fe3 and O2")+
  theme(axis.text = element_text(size = 36), # Increase font size of axis text
        axis.title.x = element_text(size = 36),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 36))+ # Increase font size of y-axis label
  theme_classic()

#Linear Reg for Fe2 and Fe3
corrected_all %>%
  ggplot(aes(x = Fe3_mmolL_mean, y = Fe2_mmolL_mean)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, lwd = 1.5) +
  stat_regline_equation(label.y = 2.5, size = 16)+
  stat_cor(label.y = 3, size = 16)+# Adjust label position as needed
  labs(x = "Fe mmol L-1", y = "SUVA254",
       title = "Linear Regression of Total Fe and SUVA254")+
  theme(axis.text = element_text(size = 36), # Increase font size of axis text
        axis.title.x = element_text(size = 36),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 36))+ # Increase font size of y-axis label
  theme_classic()

#Linear Reg for Fe3 and SUVA
corrected_all %>%
  ggplot(aes(x = Fe3_mmolL_mean, y = SUVA254)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, lwd = 1.5) +
  stat_regline_equation(label.y = 45, size = 12)+
  stat_cor(label.y = 40, size = 12)+# Adjust label position as needed
  labs(x = "Fe3 mmol L-1", y = "SUVA254",
       title = "Linear Regression of Fe3 and SUVA254")+
  theme(axis.text = element_text(size = 36), # Increase font size of axis text
        axis.title.x = element_text(size = 36),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 36))+ # Increase font size of y-axis label
  theme_classic()

#Linear Reg for Fe2:Fe3 and Fe:OC
corrected_all %>%
  filter(Treatment == "OW")%>%
  group_by(Wash, Fraction)%>%
  mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  ggplot(aes(x = `Fe2/Fe3`, y = Fe.OC_mean,color = Fraction, shape = Wash)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, lwd = 1.5) +
  stat_regline_equation(label.y = .5,label.x = 1.5, size = 12)+
  stat_cor(label.y = .25,label.x = 1.5, size = 12)+# Adjust label position as needed
  labs(x = "Fe2:Fe3", y = "Fe:OC",
       title = "Linear Regression of Fe2:Fe3 and Fe:OC")+
  theme(axis.text = element_text(size = 36), # Increase font size of axis text
        axis.title.x = element_text(size = 36),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 36))+ # Increase font size of y-axis label
  theme_classic()


#Linear Reg for Fe2:Fe3 and SUVA
corrected_all %>%
  filter(Treatment == "OW")%>%
  mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  ggplot(aes(x = `Fe2/Fe3`, y = SUVA254)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, lwd = 1.5) +
  stat_regline_equation(label.y = 38,label.x = 2, size = 6)+
  stat_cor(label.y = 30, label.x = 2, size = 6)+# Adjust label position as needed
  labs(x = "Fe2:Fe3", y = "SUVA254",
       title = "Linear Regression of Fe2:Fe3 and SUVA254")+
  theme(axis.text = element_text(size = 36), # Increase font size of axis text
        axis.title.x = element_text(size = 36),  # Increase font size of x-axis label
        axis.title.y = element_text(size = 36))+ # Increase font size of y-axis label
  theme_classic()



#Plot of Fe2/Fe3 and wash
corrected_all%>%
  group_by(Fraction, Wash, Treatment, Group)%>%
  mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  ggplot(aes(x= Wash, y= `Fe2/Fe3`))+
  geom_point(aes(x= Wash, y= `Fe2/Fe3`,ymin = 0- doc_mg_l_sd, ymax = 5, color= factor(Fraction, levels= treatment_order), shape =Treatment), size = 1.5) +
  geom_path(aes(x=Wash, y=doc_mg_l_mean, color= factor(Fraction, levels= treatment_order),
                group=factor(Group, levels= line_path_order)),lwd=1.5) +
  theme_classic()

#Plot of SUVA and wash
corrected_all%>%
  group_by(Fraction, Wash, Treatment, Group)%>%
  #mutate(`Fe2/Fe3` = Fe2_mmolL_mean/Fe3_mmolL_mean)%>%
  ggplot(aes(x= Wash, y= SUVA254))+
  geom_point(aes(x= Wash, y= SUVA254,ymin = 0, ymax = 200, color= factor(Fraction, levels= treatment_order), shape =Treatment), size = 1.5) +
  geom_path(aes(x=Wash, y=doc_mg_l_mean, color= factor(Fraction, levels= treatment_order),
                group=factor(Group, levels= line_path_order)),lwd=1.5) +
  theme_classic()


#Correlation matrix
corrected_all %>%
  subset(select = -c(Group, Treatment,Wash,sample_name))%>%
  subset(select = c(Fraction,
                    doc_mg_l_mean,
                    O2_._mean,
                    Cond_mean,
                    Salinity..g.L._mean,
                    Fe2_mmolL_mean,
                    Fe3_mmolL_mean,
                    Fe_tot_mmolL_mean,
                    doc_mmolL_mean,
                    a254_corrected,
                    SUVA254))%>%
  mutate_all(as.numeric)%>%
  cor()%>%
corrplot(method = "circle")

# #I was getting tired of figuring this out in r so I am doing some things in excel to make this go faster
# write_csv(unique_rows, "../tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/Fraction_subtraction.csv")
# write_csv(Blanks,"../tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/Blank_subtraction.csv")
# 
# #This File is all of the data merged together and the Fe and DOC data have been calculated for each fraction  
# #I did this in excel. I used the Fraction_subtraction and Blank_subtraction files and made this new file
# #
# Adjusted_data <- read.csv("C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/Summary/Fractions/All_adjusted.csv")%>%
#   mutate(`[DOC]` = X.DOC.,
#          `[Fe2]_correct` = X.Fe2._correct,
#          `[Fe3]_correct` = X.Fe3._correct,
#          `[Fe]_correct` = X.Fe.,
#          `OC:Fe2` = OC.Fe2,
#          `OC:Fe3` = OC.Fe3,
#          `OC:FeTotal` = OC.FeTotal)%>%
#   subset(select = -c( X.DOC.,
#                      X.Fe2._correct,
#                      X.Fe3._correct,
#                      X.Fe.,
#                      OC.Fe2,
#                      OC.Fe3,
#                      OC.FeTotal))
# 
# 
# treatment_order <- c('0.1','0.45','1', "Blank")
# line_path_order <- c("AW 0.1", "AW 0.45", "AW 1", "OW 0.1", "OW 0.45", "OW 1", "AW Blank", "OW") 
# 
# Adjusted_data%>%
# ggplot()
# 
# Adjusted_data%>%
#   group_by(Fraction, Treatment, Wash, Group)%>%
# ggplot()+
#   geom_pointrange(aes(x=Wash, y=DOC_corrected, ymin = DOC_corrected- sd_doc_mg_l, ymax = DOC_corrected + sd_doc_mg_l, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
#   geom_path(aes(x=Wash, y=DOC_corrected, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
#   theme_classic() +
#   labs(x = "Wash", y = "DOC mgC/L", color = "Size Fraction (um)")




  
 
 


