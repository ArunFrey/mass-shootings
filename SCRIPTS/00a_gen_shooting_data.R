# ---
# COMPARE DATASETS
# ---

library(tidyverse)
library(foreign)
library(lubridate)

# Mother Jones
motherjones <- read_csv("DATA/RAW/Mother Jones/MJ_Shootings_1982-2020.csv") %>%
  mutate(date = mdy(date),
         year = year(date)) %>%
  # delete cases with three fatalities for consistency
  filter(!fatalities <= 3) 

# Stanford Libraries
stanford <- read_csv("DATA/RAW/Stanford Libraries/mass_shootings_stanford.csv") %>%
  mutate(date = mdy(Date),
         year = year(date)) %>%
  # delete cases with three fatalities for consistency
  filter(!`Total Number of Fatalities` <= 3) 

# The Violence Project
violenceproj <- read_csv("DATA/RAW/Violence Project/mass_shooter.csv", skip = 1) %>%
  mutate(date = dmy(`Full Date`),
         year = year(date)) %>%
  # delete cases with three fatalities for consistency
  filter(!`Number Killed` <= 3) 

# In the 1998 Thurston High School shooting, the date differs by 1 day
violenceproj <- violenceproj %>%
  ungroup() %>%
  mutate(date = ifelse(`Case #`==69, as.Date("1998-05-21"), as.Date(date)))
  
  
# Select only all shootings which feature in both the Mother Jones and the Violence Project database
shootings <- motherjones %>% 
  filter(date %in% unique(violenceproj$date))

# for shootings prior to 2016 (after which the Stanford database was no longer upkept), include only the shootings listed also on the Stanford database
shootings <- shootings %>% 
  filter(date %in% stanford$date|year>=2016)

shootings <- shootings %>% 
  mutate(date = as.character(format((date), "%m/%d/%Y")))
         
# save all shootings which overlap with the data
write_csv(shootings, file = "DATA/EDIT/shootings.csv")
