## This script imports raw data for NPOC and TDN measured using a Shimadzu TOC-L
## at PNNL MCRL and exports clean, Level 1 QC'ed data. 
## Raw Data are read in from GitHub
## 
## Created: 2022-01-15 by Peter Regier for EXCHANGE
## Updated: 2022-06-26 by Allison Myers-Pigg for TEMPEST
## Updated: 2023-12-19 by AMP for ionic strength experiment
## Updated: 2024-03-26 by CO and AMP for SULI experiment
## 
# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive) # drive_ functions

#double check your wd. should be ../tempest_ionic_strength
#if not you need to do new relative file pathing

getwd()

## Set Github filepath for NPOC raw data files:

directory = "C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/TMP_FEOM_TOC/OneDrive_1_3-21-2024"

##FYI this should work if you open your file from the .Rproj in your repo: "tempest-exp-feom-ConnorSULI"

directory = "../tempest-exp-feom-ConnorSULI/1-data/TMP_FEOM_TOC/OneDrive_1_3-21-2024/"


# 2. Functions -----------------------------------------------------------------

## Create a function to read in data
read_data <- function(data){
  # First, scrape date from filename
 rundate <- str_extract(data, "[0-9]{8}")
  # Second, read in data
  read_delim(file = data, skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tdn_raw = `Result(TN)`,
           run_datetime = `Date / Time`) %>% 
    select(sample_name, npoc_raw, tdn_raw,run_datetime) %>% 
    mutate(rundate = rundate)
}

read_mes <- function(readme){
  # First, scrape date from filename
  rundate <- str_extract(readme, "[0-9]{8}")
  # Second, read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_name = `Sample Name`,
           sample_vol = `Sample wt`,
           total_vol = `Total vol`) %>% 
    select(sample_name, Action, sample_vol, total_vol) %>% 
    mutate(rundate = rundate)
}

# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- list.files(path = directory, pattern = "Summary", full.names = TRUE) 
print(files)
ReadMes <- list.files(path = directory, pattern = "Readme", full.names = TRUE) 
print(ReadMes)

npoc_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("W", sample_name)) %>% # filter to samples only
  bind_rows() 

#I'm still getting a weird warning for your data I think it has to do with the different file export type on the 118 TOC than on Oscar.

blanks_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("^Blank", sample_name)) %>% # filter to blanks only
  bind_rows() 

readmes_dilution_action <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("W", sample_name)) %>% # filter to samples only
  filter(grepl("Dilution correction", Action)) %>%
  bind_rows() 

readmes_all <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("W", sample_name)) %>% # filter to samples only
  bind_rows() 

curvepts <-files %>% 
  map_df(read_data) %>% #some curves were omitted according to the read mes, but this doesn't matter rn because all the same range. 
  filter(grepl("^CalCurve_", sample_name)) %>% # filter to curves only
  rename(standard_high_C = npoc_raw) %>%
  select(rundate,standard_high_C) %>%
  #this part of the code would matter if we actually ran different curve ranges between the two runs then applied the other curve to the other dataset (which is what was done). 
  #It doesn't matter now since functionally the same for the same concentration ranges. 
  pivot_longer(cols = c(standard_high_C)) %>%
  na.omit() %>%
  group_by(rundate) %>%
  distinct()%>%
  pivot_wider(names_from= name, values_from = value)%>%
  bind_rows() 


# 4. Calculate blanks and add to data ------------------------------------------

blanks <- blanks_raw %>% 
  filter(!run_datetime %in% NA) %>% 
  mutate(npoc_raw = ifelse(npoc_raw > 0, npoc_raw, NA)) %>%
  group_by(rundate) %>% 
  summarize(npoc_blank= round(mean(npoc_raw[!is.na(npoc_raw)]), 2),
            npoc_blank_SD= round(sd(npoc_raw[!is.na(npoc_raw)]), 2)) %>% #add SD columns 
  select(rundate, npoc_blank, npoc_blank_SD)

View(blanks) # Check out the blank data 

# 5. Flag sketch data -----------------------------------------------------------

npoc_flagged <- npoc_raw %>% 
  filter(grepl("W", sample_name)) %>% # filter to samples only
  inner_join(blanks, by = "rundate") %>% 
  inner_join(curvepts, by= "rundate") %>%
  mutate(npoc_flag = case_when(npoc_raw > standard_high_C ~ "value above cal curve",
                               npoc_blank > 0.15*npoc_raw ~ "blank is ≥ 15% of sample value" # flagging if blank concentration is > 20% of the sample concentration
  )
         #most curves only to 50, those samples were not above it. making 100 for the August and September, which used 0-100
         # npoc_raw = case_when(npoc_flag == "incorrect sample naming, cannot resolve" ~ NA,
         #                      TRUE ~ npoc_raw),
         # tdn_raw = case_when(tdn_flag == "incorrect sample naming, cannot resolve" ~ NA,
         #                     TRUE ~ tdn_raw)
  )

# 6. Dilution Corrections ------------------------------------------------------

dilutions = 
  readmes_dilution_action %>% 
  mutate(Dilution =  total_vol/sample_vol) %>% 
  dplyr::select(rundate, sample_name, Action, Dilution) %>% 
  force()

samples_to_dilution_corrected = 
  npoc_flagged %>%
  left_join(dilutions, by = c("sample_name", "rundate")) %>% 
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

all_samples_dilution_corrected =
  npoc_flagged %>%
  left_join(readmes_all, by = c("sample_name", "rundate")) %>% 
  mutate(doc_mg_l = npoc_raw, tdn_mg_l = tdn_raw) %>%
  filter(!grepl("Dilution correction", Action)) %>% 
  filter(!Action %in% "Omit") %>%
  bind_rows(samples_to_dilution_corrected) %>%
  dplyr::select(sample_name, rundate, doc_mg_l, npoc_flag)%>%
  mutate(doc_mg_l = if_else(doc_mg_l < 0, "NA", as.character(doc_mg_l)),
         doc_mg_l = as.numeric(doc_mg_l), doc_mg_l = round(doc_mg_l, 2))

#Identify if any duplicates were run, this should return an empty data frame if not:#

duplicates <- all_samples_dilution_corrected %>% subset(duplicated(sample_name))

View(duplicates)

## no duplicates so removed that part of the code. 


## 7. Clean data ----------------------------------------------------------------


## Flagging data
npoc_flags <- all_samples_dilution_corrected%>% 
  ## add flags 
  # Below blank 
  mutate(npoc_flag = case_when(
                               doc_mg_l == 'NaN' ~ "value below blank",
                               grepl("value above cal curve",npoc_flag) ~ "value above cal curve",
                               TRUE ~ npoc_flag), 
         #tdn_flag = case_when(
              #                tdn_mg_l == 'NaN' ~ "value below blank",
              #                grepl("value above cal curve",tdn_flag) ~ "value above cal curve",
              #                TRUE ~ tdn_flag),
         doc_mg_l = case_when(doc_mg_l == "NaN" ~ NA,
                     TRUE ~ doc_mg_l),
        # tdn_mg_l = case_when(tdn_mg_l == "NaN" ~ NA,
         #            TRUE ~ tdn_mg_l)
         )

npoc_wmeta <- npoc_flags %>%
  mutate(Treatment = stringr::str_extract(sample_name, "[a-zA-Z]+(?=\\d)"),
         Wash = stringr::str_extract(sample_name, "\\d+(?=\\.)"),
         Fraction = stringr::str_extract(sample_name, "(?<=\\.)\\d+(?=\\.)")
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

# 8. Write data ----------------------------------------------------------------
#look at all your data before saving:

View(npoc_wmeta)

#write_csv(npoc_wmeta, "C:/Users/olou646/tempest-exp-feom-ConnorSULI/1-data/TMP_FEOM_TOC/TMP_FeOM-TOC_Summary.csv")

#Again this file pathing should work if you opened your file inside your R project 
write_csv(npoc_wmeta, "../tempest-exp-feom-ConnorSULI/1-data/TMP_FEOM_TOC/TMP_FeOM-TOC_All_data.csv")



## Look at it all together! 

treatment_order <- c('0.1','0.45','1', "Blank")
line_path_order <- c("AW 0.1", "AW 0.45", "AW 1", "OW 0.1", "OW 0.45", "OW 1", "AW Blank", "OW Blank")


npoc_wmeta %>%
  group_by(Fraction, Treatment, Wash, Group) %>%
  dplyr::summarise(mean_doc_mg_l = mean(doc_mg_l, na.rm=TRUE), sd_doc_mg_l = sd(doc_mg_l, na.rm=TRUE)) %>%
ggplot()+
  geom_pointrange(aes(x=Wash, y=mean_doc_mg_l, ymin = mean_doc_mg_l- sd_doc_mg_l, ymax = mean_doc_mg_l + sd_doc_mg_l, color= factor(Fraction, levels= treatment_order), shape =Treatment)) +
  geom_path(aes(x=Wash, y=mean_doc_mg_l, color= factor(Fraction, levels= treatment_order), group=factor(Group, levels= line_path_order) )) +
  theme_classic() +
  labs(x = "Wash", y = "DOC mgC/L", color = "Size Fraction (um)")

