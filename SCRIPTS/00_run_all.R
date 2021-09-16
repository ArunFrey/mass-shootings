#----
# RUN ALL
# ---
rm(list = ls())

library(tidyverse)


# Load variable names 
source("SCRIPTS/vars.R")

# Generate shooting data (shootings that appear in all three lists)
source("SCRIPTS/00a_gen_shooting_data.R")

# Load data
source("SCRIPTS/01_load_files.R")

# clean data
source("SCRIPTS/02_cleaning.R")

# generate descriptives
source("SCRIPTS/03_desc.R")

# save and load the datasets, for ease of use
#save(gss_event, file = "DATA/FINAL/gss_event.R")
#load("DATA/FINAL/gss_event.R")

# main analysis
source("SCRIPTS/04_main.R")

# shootings w. 10 or more fatalities
source("SCRIPTS/04_r_10+_fatalities.R")

#alternative party coding: differentiating between independents and others
source("SCRIPTS/04_r_alt_party_coding.R")

# alternative treatment period: 7 and 14 days
source("SCRIPTS/04_r_alt_treat_period.R")

# logistic regression
source("SCRIPTS/04_r_logit.R")

# matching control and treatment units
source("SCRIPTS/04_r_matching.R")

# month FE (seasonality)
source("SCRIPTS/04_r_month.R")

# no control variables
source("SCRIPTS/04_r_no_controls.R")

# controlling for number of prior shootings in last 3 months
source("SCRIPTS/04_r_prior_shootings.R")

# controlling for year and region fixed effects
source("SCRIPTS/04_r_region_FE.R")

# broadening school category to also include universities and colleges
source("SCRIPTS/04_r_school_uni_shooting.R")

# specification curve *takes long, dont run unnecessarily
source("SCRIPTS/04_r_spec_curve.R")

# specification curve- school *takes long, dont run unnecessarily
source("SCRIPTS/04_r_spec_curve_school.R")

# exclude treatment groups with less than 20 obs
source("SCRIPTS/04_r_treat_20+_obs.R")

# vary treatment period from 1 to 28 days 
source("SCRIPTS/04_r_treat_over_time.R")

# plot models
source("SCRIPTS/05_plots.R")
