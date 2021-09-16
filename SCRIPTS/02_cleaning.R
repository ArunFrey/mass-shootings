# ---
# CLEANING THE DATA
# ---

library(tidyverse)
library(lubridate)
library(kableExtra)

# SPECIFY TREATMENT VARIABLES ------------------------------------------------------------------


# Generate treatment days
dates_1 <- dates_2 <- dates_3 <- dates_4 <- dates_5 <- dates_6 <- dates_7 <- dates_8 <- dates_9 <- 
  dates_10 <- dates_11 <- dates_12 <- dates_13 <- dates_14 <- dates_15 <- dates_16 <- dates_17 <- 
  dates_18 <- dates_19 <- dates_20 <- dates_21 <- dates_22 <- dates_23 <- dates_24 <- dates_25 <-
  dates_26 <- dates_27 <- dates_28 <- dates_14_c <- Date()

# id column to merge with shooting information
shooting_id <- c()

shootings_overlap <- shootings_overlap %>% 
  arrange(date_shooting)

for(i in 1:nrow(shootings_overlap)){
  dates_1 <- c(dates_1, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 1, by = "1 day"))
  
  dates_2 <- c(dates_2, seq(from = shootings_overlap$date_shooting[i] + 1,
                              to = shootings_overlap$date_shooting[i]  + 2, by = "1 day"))
  
  dates_3 <- c(dates_3, seq(from = shootings_overlap$date_shooting[i] + 1,
                              to = shootings_overlap$date_shooting[i]  + 3, by = "1 day"))
  
  dates_4 <- c(dates_4, seq(from = shootings_overlap$date_shooting[i] + 1,
                              to = shootings_overlap$date_shooting[i]  + 4, by = "1 day"))
  
  dates_5 <- c(dates_5, seq(from = shootings_overlap$date_shooting[i] + 1,
                             to = shootings_overlap$date_shooting[i]  + 5, by = "1 day"))
  
  dates_6 <- c(dates_6, seq(from = shootings_overlap$date_shooting[i] + 1,
                             to = shootings_overlap$date_shooting[i]  + 6, by = "1 day"))
  
  dates_7 <- c(dates_7, seq(from = shootings_overlap$date_shooting[i] + 1,
                  to = shootings_overlap$date_shooting[i]  + 7, by = "1 day"))
  
  dates_8 <- c(dates_8, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 8, by = "1 day"))
  
  dates_9 <- c(dates_9, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 9, by = "1 day"))
  
  dates_10 <- c(dates_10, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 10, by = "1 day"))
  
  dates_11 <- c(dates_11, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 11, by = "1 day"))
  
  dates_12 <- c(dates_12, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 12, by = "1 day"))
  
  dates_13 <- c(dates_13, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 13, by = "1 day"))

  dates_14 <- c(dates_14, seq(from = shootings_overlap$date_shooting[i] + 1,
                          to = shootings_overlap$date_shooting[i]  + 14, by = "1 day"))
  
  dates_15  <- c(dates_15, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 15, by = "1 day"))
  
  dates_16 <- c(dates_16, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 16, by = "1 day"))
  
  dates_17 <- c(dates_17, seq(from = shootings_overlap$date_shooting[i] + 1,
                            to = shootings_overlap$date_shooting[i]  + 17, by = "1 day"))
  
  dates_18 <- c(dates_18, seq(from = shootings_overlap$date_shooting[i] + 1,
                             to = shootings_overlap$date_shooting[i]  + 18, by = "1 day"))
  
  dates_19 <- c(dates_19, seq(from = shootings_overlap$date_shooting[i] + 1,
                             to = shootings_overlap$date_shooting[i]  + 19, by = "1 day"))
  
  dates_20 <- c(dates_20, seq(from = shootings_overlap$date_shooting[i] + 1,
                             to = shootings_overlap$date_shooting[i]  + 20, by = "1 day"))
  
  dates_21 <- c(dates_21, seq(from = shootings_overlap$date_shooting[i] + 1,
                  to = shootings_overlap$date_shooting[i]  + 21, by = "1 day"))
  
  dates_22  <- c(dates_22, seq(from = shootings_overlap$date_shooting[i] + 1,
                               to = shootings_overlap$date_shooting[i]  + 22, by = "1 day"))
  
  dates_23 <- c(dates_23, seq(from = shootings_overlap$date_shooting[i] + 1,
                              to = shootings_overlap$date_shooting[i]  + 23, by = "1 day"))
  
  dates_24 <- c(dates_24, seq(from = shootings_overlap$date_shooting[i] + 1,
                              to = shootings_overlap$date_shooting[i]  + 24, by = "1 day"))
  
  dates_25 <- c(dates_25, seq(from = shootings_overlap$date_shooting[i] + 1,
                              to = shootings_overlap$date_shooting[i]  + 25, by = "1 day"))
  
  dates_26 <- c(dates_26, seq(from = shootings_overlap$date_shooting[i] + 1,
                               to = shootings_overlap$date_shooting[i]  + 26, by = "1 day"))
  
  dates_27 <- c(dates_27, seq(from = shootings_overlap$date_shooting[i] + 1,
                             to = shootings_overlap$date_shooting[i]  + 27, by = "1 day"))
  
  dates_28 <- c(dates_28, seq(from = shootings_overlap$date_shooting[i] + 1,
                  to = shootings_overlap$date_shooting[i]  + 28, by = "1 day"))
  
  dates_14_c <- c(dates_14_c, seq(from = shootings_overlap$date_shooting[i] - 14,
                                   to = shootings_overlap$date_shooting[i]  -1, by = "1 day"))
  
  temp <- tibble(shooting_id = shootings_overlap$shooting_id[[i]], 
                 shooting_nr = i, 
                 date = seq(from = shootings_overlap$date_shooting[i],
                            to = shootings_overlap$date_shooting[i]  + 28, by = "1 day"))
  shooting_id <- bind_rows(shooting_id, temp) 
}

# for shootings with overlapping treatment dates, assign unit to most recent shooting only
shooting_id <- left_join(shooting_id, shootings_overlap[, c("shooting_id", "case", "date_shooting", "location_1", "fatalities", 
                                                            "school_shooting", "school_uni_shooting", "race_shooter")])

shooting_id <- shooting_id %>% 
  mutate(days_since_shooting = date - date_shooting) %>% 
  group_by(date) %>% 
  slice(which.min(days_since_shooting))

# select treated and control units
# treated units: units interviewed within X days after shooting (excl. day of shooting)
# control units: units interviewed prior to first shooting

gss_event <- gss %>%
  group_by(year) %>%
  mutate(treat_1 = ifelse(date %in%  dates_1, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_2 = ifelse(date %in%  dates_2, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_3 = ifelse(date %in%  dates_3, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_4 = ifelse(date %in%  dates_4, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_5 = ifelse(date %in%  dates_5, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_6 = ifelse(date %in%  dates_6, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_7 = ifelse(date %in%  dates_7, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_8 = ifelse(date %in%  dates_8, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_9 = ifelse(date %in%  dates_9, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_10 = ifelse(date %in%  dates_10, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_11 = ifelse(date %in%  dates_11, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_12 = ifelse(date %in%  dates_12, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_13 = ifelse(date %in%  dates_13, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_14 = ifelse(date %in%  dates_14, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_15 = ifelse(date %in%  dates_15, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_16 = ifelse(date %in%  dates_16, 1,
                          ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_17 = ifelse(date %in%  dates_17, 1,
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_18 = ifelse(date %in%  dates_18, 1,
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_19 = ifelse(date %in%  dates_19, 1,
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_20 = ifelse(date %in%  dates_20, 1,
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_21 = ifelse(date %in%  dates_21, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)),
         treat_22 = ifelse(date %in%  dates_22, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_23 = ifelse(date %in%  dates_23, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_24 = ifelse(date %in%  dates_24, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_25 = ifelse(date %in%  dates_25, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_26 = ifelse(date %in%  dates_26, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_27 = ifelse(date %in%  dates_27, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         treat_28 = ifelse(date %in%  dates_28, 1, 
                           ifelse(date %in% shootings_overlap$date_shooting, NA, 0)), 
         control_14 = ifelse(date %in%  dates_14_c, 1, 
                             ifelse(date %in% shootings_overlap$date_shooting, NA, 0))) %>%
  group_by(year) %>%
  mutate(treat_min = min(date[treat_7==1], na.rm = T), 
         # all dates prior to (first) shooting are treated as controls
         control = ifelse(date %in% shootings_overlap$date_shooting, NA, 
                          ifelse(date < treat_min, 1, 0))) %>%
  ungroup() %>%
  mutate(treat_control_1 = ifelse(treat_1==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_2 = ifelse(treat_2==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_3 = ifelse(treat_3==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_4 = ifelse(treat_4==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_5 = ifelse(treat_5==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_6 = ifelse(treat_6==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_7 = ifelse(treat_7==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_8 = ifelse(treat_8==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_9 = ifelse(treat_9==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_10 = ifelse(treat_10==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_11 = ifelse(treat_11==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_12 = ifelse(treat_12==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_13 = ifelse(treat_13==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_14 = ifelse(treat_14==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_15 = ifelse(treat_15==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_16 = ifelse(treat_16==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_17 = ifelse(treat_17==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_18 = ifelse(treat_18==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_19 = ifelse(treat_19==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_20 = ifelse(treat_20==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_21 = ifelse(treat_21==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_22 = ifelse(treat_22==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_23 = ifelse(treat_23==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_24 = ifelse(treat_24==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_25 = ifelse(treat_25==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_26 = ifelse(treat_26==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_27 = ifelse(treat_27==1, "Treated", ifelse(control==1, "Control", NA)),
         treat_control_28 = ifelse(treat_28==1, "Treated", ifelse(control==1, "Control", NA)), 
         treat_control_14_c = ifelse(treat_14==1, "Treated", ifelse(control_14==1, "Control", NA))) %>%
  select(-treat_min)

# only select years where a shooting occurred
gss_event <- gss_event %>% 
  filter(year %in% year(dates_28)) %>%
  # remove the first few dates in January 2004, as these were leftover respondents from the previous wave
  filter(!(date %in% as.Date(c("2004-01-01", "2004-01-02", "2004-01-03", "2004-01-04"))))


# Merge with shooting information
gss_event <- left_join(gss_event, shooting_id) %>%
  mutate(school_shooting = ifelse(is.na(school_shooting), 0, school_shooting), 
         school_uni_shooting = ifelse(is.na(school_uni_shooting), 0, school_uni_shooting))

# generate count of prior shootings (3 months)
prior_shootings <- temp <-c()

for(i in 1:nrow(shootings)){
  temp <- tibble(shooting_id = shootings$shooting_id[[i]], 
                 date = seq(from = shootings$date[i],
                            to = shootings$date[i] %m+% months(3), by = "1 day"))
  prior_shootings <- bind_rows(prior_shootings, temp) 
}

prior_shootings <- prior_shootings %>% 
  group_by(date) %>% 
  count() %>% 
  rename(prior_shootings = n)

# merge count of prior shootings with gss data, replace NA with 0
gss_event <- left_join(gss_event, prior_shootings) %>% 
  mutate(prior_shootings = ifelse(is.na(prior_shootings), 0, prior_shootings))

# shooting last month 
shooting_month <- temp <-c()

for(i in 1:nrow(shootings)){
  temp <- tibble(shooting_id = shootings$shooting_id[[i]], 
                 date = seq(from = shootings$date[i],
                            to = shootings$date[i] %m+% months(1), by = "1 day"))
  shooting_month <- bind_rows(shooting_month, temp) 
}

shooting_month <- shooting_month %>% 
  group_by(date) %>% 
  count() %>% 
  rename(shooting_month = n)

# merge with gss data, replace NA with 0
gss_event <- left_join(gss_event, shooting_month) %>% 
  mutate(shooting_month = ifelse(is.na(shooting_month), 0, shooting_month))


# delete all observations in the control group that had a shooting in the previous month
gss_event <- gss_event %>%
  filter(control==1 & shooting_month==0|control==0)

# generate variable identifying years when school shooting took place
year_school <- unique(year(shootings_overlap$date_shooting[shootings_overlap$school_shooting==1]))
year_school_uni <- unique(year(shootings_overlap$date_shooting[shootings_overlap$school_uni_shooting==1]))

gss_event <- gss_event %>%
  mutate(year_school = ifelse(year %in% year_school, 1, 0), 
         year_school_uni = ifelse(year %in% year_school_uni, 1, 0))

# select non-missing observations on date
gss_event <- gss_event %>%
  filter(!is.na(date))


# Select non-missing observations for DV
gss_event <- gss_event %>%
  filter(!is.na(restrict_guns))

# generate variable that is 1 for when all ivs are nonmissing
gss_event <- left_join(gss_event, gss_event %>% 
                         select(id, year, all_of(ivs)) %>% 
                         na.omit() %>%
                         mutate(sample_ivs = 1) %>%
                         select(id, year, sample_ivs)) %>%
  mutate(sample_ivs = ifelse(is.na(sample_ivs), 0, sample_ivs))

# standardize variables
scale <- function(x, na.rm = T){
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
}

gss_event <- bind_cols(gss_event, 
                       gss_event %>% 
                         select(id, all_of(ivs)) %>%
                         mutate_if(is.numeric, list(std = scale)) %>%
                         select(ends_with("std")))
                       

