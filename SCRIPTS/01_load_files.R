# ---
# LOAD FILES
# ---

library(tidyverse)
library(foreign)
library(lubridate)

theme_set(theme_minimal())

# LOAD DATA ----
gss_raw <- readRDS("DATA/RAW/gss/gss.rds")
# uncomment for only shootings from Mother Jones
#shootings <- read_csv("DATA/RAW/Mother Jones/MJ_Shootings_1982-2020.csv")
# uncomment for shootings from Mother Jones, Stanford, and Violence Project
shootings <- read_csv("DATA/EDIT/shootings.csv")

# GSS -----------------------------------------------------------------------------------------

gss <- gss_raw %>%
  mutate(
    day = sprintf("%02d", dateintv %% 100),
    month = ifelse(nchar(dateintv) == 3, substr(dateintv, 1, 1),
                   ifelse(nchar(dateintv) == 4, substr(dateintv, 1, 2), NA)),
    date = as_date(paste0(year, "-", month, "-", day)))


# GENERATE NEW VARIABLES
gss <- gss %>%
  mutate(
    restrict_guns = ifelse(gunlaw == "favor", 1, ifelse(gunlaw == "oppose", 0, NA)),
    party_cat = ifelse(partyid %in% c("strong democrat", "not str democrat"), "Democrat",
                       ifelse(partyid %in% c("ind,near dem", "independent", "ind,near rep","other party"), 
                              "Independent", 
                              ifelse(partyid %in% 
                                  c("not str republican", "strong republican"), "Republican", NA))),
    party_cat = factor(party_cat, levels = c("Democrat", "Independent", "Republican")),
    party_cat2 = ifelse(partyid %in% c("strong democrat", "not str democrat"), "Democrat",
                       ifelse(partyid %in% c("ind,near dem", "independent", "ind,near rep"), 
                              "Independent", 
                              ifelse(partyid %in% 
                                       c("not str republican", "strong republican"), "Republican", 
                                     ifelse(partyid == "other party", "Other", NA)))),
    party_cat2 = factor(party_cat2, levels = c("Democrat", "Independent", "Republican", "Other")),
    party_cat3 = ifelse(partyid %in% c("strong democrat", "not str democrat", "ind,near dem"), "Democrat",
                        ifelse(partyid %in% c("independent", "other party"), "Independent", 
                               ifelse(partyid %in% 
                                        c("ind, near rep", "not str republican", "strong republican"),
                                      "Republican", NA))),
    party_cat3 = factor(party_cat3, levels = c("Democrat", "Independent", "Republican")),
    democrat = ifelse(party_cat == "Democrat", 1, 
                      ifelse(is.na(party_cat), NA, 0)),
    republican = ifelse(party_cat == "Republican", 1, 
                        ifelse(is.na(party_cat), NA, 0)), 
    independent = ifelse(party_cat == "Independent", 1,
                         ifelse(is.na(party_cat), NA, 0)), 
    married = ifelse(marital == "married", 1,
      ifelse(marital %in% c("widowed", "divorced", "separated", "never married"), 0, NA)),
    owngun = ifelse(owngun == "no", 0,
                    ifelse(owngun == "yes", 1, NA)),
    race = ifelse(race=="black", "Black", ifelse(race=="white", "White", ifelse(race=="other", "Other", NA))), 
    race = factor(race, levels = c("Black", "White", "Other")),
    black = ifelse(race == "Black", 1, 
                   ifelse(is.na(race), NA, 0)),
    white = ifelse(race == "White", 1, 
                   ifelse(is.na(race), NA, 0)),
    other = ifelse(race == "Other", 1, 
                   ifelse(is.na(race), NA, 0)),
    female = ifelse(sex=="female", 1, 0), 
    residence = ifelse(srcbelt %in% c("12 lrgst smsa's", "smsa's 13-100", "other urban"), "Urban",
      ifelse(srcbelt %in% c("suburb, 12 lrgst", "suburb, 13-100"), "Suburban",
        ifelse(srcbelt == "other rural", "Rural", NA))),
    residence = factor(residence, levels = c("Urban", "Suburban", "Rural")),
    rural = ifelse(residence == "Rural", 1, 
                   ifelse(is.na(residence), NA, 0)),
    suburb = ifelse(residence == "Suburban", 1, 
                   ifelse(is.na(residence), NA, 0)),
    urban = ifelse(residence == "Urban", 1, 
                   ifelse(is.na(residence), NA, 0)),
    born = ifelse(born=="yes", 1, 0), 
    cooperative = ifelse(coop %in% c("friendly,interested", "cooperative"), 1,
      ifelse(coop %in% c("restless,impatient", "hostile"), 0, NA)), 
    occ_gun = ifelse(occ10 %in% 
                       c(3710, 3800, 3820, 3850, 3860, 3910, 3930, 9800, 9810, 9820, 9830), 1, 
                     ifelse(is.na(occ10), NA, 0)), 
    military = ifelse(isco08 %in% c(
      "commissioned armed forces officers",
      "non-commissioned Armed Forces Officers",
      "armed forces occupations, other ranks"), 1, 0))




# SHOOTINGS -----------------------------------------------------------------------------------

shootings <- shootings %>%
  mutate(
    date = mdy(date),
    shooting_id = paste0("shooting_", rownames(.))
  ) %>%
  # delete cases with three fatalities for consistency
  filter(!fatalities <= 3) %>%
  # change names and omit "shooting", "massacre" and others
  mutate(
    case = str_remove_all(case, c("shooting|spree|massacre|mass|killings|rampage|revenge|murder|Shooting")),
    # create dummy for school shooting
    school_uni_shooting = ifelse(location_1 == "School", 1, 0), 
    school_shooting = ifelse(location_1 == "School" & 
                                str_detect(case, "university|University|college|College"), 0,
                              ifelse(location_1 == "School" & 
                                       str_detect(summary, "university|University|college|College"), 0,
                                     ifelse(str_detect(case, "Virginia Tech"), 0, school_uni_shooting))), 
    race_shooter = ifelse(race %in% c("Black", "Other", "black", "Latino", "Asian"), 1, 0))

# GET EVENTS THAT OVERLAP WITH SURVEY DATES ---
date_range_gss <- gss %>%
  filter(!is.na(date)) %>%
  filter(!is.na(restrict_guns)) %>%
  group_by(year) %>%
  summarise(
    start_date = min(date, na.rm = T),
    end_date = max(date, na.rm = T))

shootings <- shootings %>%
  left_join(date_range_gss) %>%
  group_by(year) %>%
  mutate(
    overlap = ifelse(date > start_date & date < end_date, "yes", "no"),
    overlap = replace_na(overlap, "no")
  )

shootings_overlap <- shootings %>%
  filter(overlap == "yes") %>%
  rename(date_shooting = date)

