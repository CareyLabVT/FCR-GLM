
# Let us run up a couple of fun facts about dams in the CONUS
# This script is a guide that can be used for future papers 
# that describe why Falling Creek is important in the context 
# of why research here is representative of other systems

library(tidyverse)
library(reshape2)
library(zoo)

# facts about FCR
fcr_area <- 0.119       #km2
fcr_start <- 1898       # year constructed
fcr_age <- 2021-1898
dams <- read_csv("./FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/NID2013_YearCompleted_SurfaceArea_State.csv")

dam_facts <- dams %>%
  filter(SURFACE_AREA != 0)%>%
  summarize(med_start_year = median(YEAR_COMPLETED, na.rm = T),
            mean_start_year = mean(YEAR_COMPLETED, na.rm = T),
            med_area_km2 = median(SURFACE_AREA/247, na.rm = T),              # dividing by 247 converts from acres to km2
            mean_area_km2 = mean(SURFACE_AREA/247, na.rm = T),
            max_start_year = max(YEAR_COMPLETED, na.rm = T),
            min_start_year = min(YEAR_COMPLETED, na.rm = T),
            max_area_km2 = max(SURFACE_AREA/247, na.rm = T),
            min_area_km2 = min(SURFACE_AREA/247, na.rm = T))


#What is the median age of all dams in the 2013 NID?
median_age <- dams %>%
  mutate(age = 2021-YEAR_COMPLETED)%>%
  summarize(median_age = median(age, na.rm = T))

# how does FCR stack up against all of that
median_age_plot <- dams %>%
  filter(!is.na(YEAR_COMPLETED))%>%
  mutate(age = 2021-YEAR_COMPLETED)%>%
  ggplot(., aes(age))+
  geom_histogram(bins = 300)+
  geom_vline(xintercept = fcr_age, color = "red")

#what percent of dams are over 100 years old?
num_older_100 <- dams %>%
  filter(YEAR_COMPLETED <= 1921)%>%
  summarize(number = count(.,))

num_older_100$number/87359   # 87359 == the number of dams in the 2013 NID database
# about 8.5%

#what percent of dams over 100 years old are less than 0.5 km2?
num_older_100_less_0.5 <- dams %>%
  filter(YEAR_COMPLETED <= 1921)%>%
  filter(SURFACE_AREA/247 <= 0.5)%>%
  summarize(number = count(.,))

num_older_100_less_0.5$number/num_older_100$number
# about 65.3%

#what percent of dams less than 0.5 km2 are over 100 years old?
num_older_100_less_0.5$number/87359
# about 5.5%
