library(tidyverse)

data <- read.csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLYSI_PAR_secchi/YSI_PAR_profiles_2013-2019.csv')
data$DateTime <- as.Date(data$DateTime)
data <- data[data$DateTime>'2019-01-01',]
data <- data[data$Site=='200' | data$Site=='100',]
data <- data[data$Reservoir=='FCR',]
#median DO saturation at F100 and F200

summary <- data %>% 
  select(Site, DateTime, Depth_m, DO_mgL, DOSat)  
summary <-   na.omit(summary)

summary <- summary %>% 
  group_by(Site) %>% 
  mutate(mean_sat = mean(DOSat, na.rm = TRUE)) %>% 
  mutate(range_sat = (max(DOSat, na.rm = TRUE) - min(DOSat, na.rm = TRUE))) %>% 
  mutate(median_sat = median(DOSat, na.rm = TRUE)) %>% 
  mutate(median_mgl = median(DO_mgL, na.rm = TRUE)) %>% 
  mutate(SD_sat = sd(DOSat, na.rm = TRUE)) %>% 
  mutate(SD_mgl = sd(DO_mgL, na.rm = TRUE))
summary <- summary %>% 
  group_by(Site) %>% 
  mutate(SE_sat = ifelse(Site=='100', SD_sat/27, SD_sat/21)) %>% 
  mutate(SE_mgl = ifelse(Site=='100', SD_mgl/27, SD_mgl/21)) %>% 
  select(Site, mean_sat:SE_mgl) %>% 
  distinct(Site, .keep_all = TRUE)

