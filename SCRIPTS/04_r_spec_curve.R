# ---
# SUPPLEMENTARY ANALYSIS: SPECIFICATION CURVE
# ---

library(tidyverse)
library(estimatr)
library(texreg)
library(MuMIn)

# Specify paths 
plots_path <- "OUTPUT/PLOTS/supp/r_spec_curve/"
tables_path <- "OUTPUT/TABLES/supp/r_spec_curve/"
models_path <- "OUTPUT/MODELS/supp/r_spec_curve/"

# Specify model
controls <- paste(ivs, collapse = " + ")

#  SPECS -------------------------------------------------------------------------------------

# restrict to non-missing on IVS
gss_analysis <- gss_event %>% 
  filter(sample_ivs == 1)

# filter out missing values for treatment values (i.e. day of shooting, since function below does not allow missingness)
gss_analysis <- gss_analysis %>%
  filter(!is.na(treat_14)) %>%
  filter(!is.na(treat_7)) %>%
  filter(!is.na(treat_28))

var_labels <- list("treat_14" = "Treatment", 
                   "treat_14:party_catIndependent" = "Treatment x Independent", 
                   "treat_14:party_catRepublican" = "Treatment x Republican",
                   "party_catIndependent" = "Independent",
                   "party_catRepublican" = "Republican", 
                   "age" = "Age", 
                   "female" = "Female", 
                   "born" = "U.S. born", 
                   "raceWhite" = "White", 
                   "raceOther" = "Other", 
                   "residenceSuburban" = "Suburban", 
                   "residenceRural" = "Rural", 
                   "educ" = "Education (yrs.)", 
                   "married" = "Married", 
                   "owngun" = "Owns Gun", 
                   "cooperative" = "Coop. Interview")

# ANALYSIS ------------------------------------------------------------------------------------

# Model 1
full_7 <- lm(formula(paste(paste0(dv, " ~ treat_7"), controls, sep = " + ")), 
                    data = gss_analysis, na.action = "na.fail")

full_14 <- lm(formula(paste(paste0(dv, " ~ treat_14"), controls, sep = " + ")), 
                 data = gss_analysis, na.action = "na.fail")

full_21 <- lm(formula(paste(paste0(dv, " ~ treat_21"), controls, sep = " + ")), 
                    data = gss_analysis, na.action = "na.fail")

# get complete list of models as separate strings
all_7 <- dredge(full_7, fixed = c("treat_7"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, restrict_guns") %>%
  str_remove_all(", gss_analysis, na.fail")

all_14 <- dredge(full_14, fixed = c("treat_14"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, restrict_guns") %>%
  str_remove_all(", gss_analysis, na.fail")

all_21 <- dredge(full_21, fixed = c("treat_21"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, restrict_guns") %>%
  str_remove_all(", gss_analysis, na.fail")


# Model 2: Interaction of treatment and party_cat
full_7_int <- lm(formula(paste(paste0(dv, " ~ party_cat*treat_7"), controls, sep = " + ")), 
                data = gss_analysis, na.action = "na.fail")

full_14_int <- lm(formula(paste(paste0(dv, " ~ treat_14 * party_cat"), controls, sep = " + ")), 
                 data = gss_analysis, na.action = "na.fail")

full_21_int <- lm(formula(paste(paste0(dv, " ~ treat_21 * party_cat"), controls, sep = " + ")), 
                 data = gss_analysis, na.action = "na.fail")

# get complete list of models as separate strings
all_7_int <- dredge(full_7_int, fixed = c("treat_7", "party_cat", "party_cat:treat_7"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, restrict_guns") %>%
  str_remove_all(", gss_analysis, na.fail")

all_14_int <- dredge(full_14_int, fixed = c("treat_14", "party_cat", "party_cat:treat_14"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, restrict_guns") %>%
  str_remove_all(", gss_analysis, na.fail")

all_21_int <- dredge(full_21_int, fixed = c("treat_21", "party_cat", "party_cat:treat_21"), eval = F) %>%
  lapply(toString) %>%
  str_remove_all("lm, restrict_guns") %>%
  str_remove_all(", gss_analysis, na.fail")


# RUN MODELS ----------------------------------------------------------------------------------

temp_yr <- temp_yr_rg <- temp_tidy <- tidy_models <- c()
time <- Sys.time()
print(paste("Starting", Sys.time()))

for(days in c(7, 14, 21)) {
  
  print(paste("Generating all possible models for:", days, "days"))
  
  model_name <- paste0("all_", days)
  treat <- paste0("treat_", days)
  
  # loop over interaction (specify separate models for interaction with party cat, or no interaction)
  for(int in c("", "_int")) {
    model_name <- paste0(model_name, int)
    models <- get(model_name)
    
    for(fixed in c("year", "year + region")) {
      if(fixed=="year"){
        temp_yr <- lapply(models,
                          FUN = function(x) lm_robust(formula(paste(dv, x)), 
                                                      fixed_effects = ~ as.factor(year), 
                                                      se_type = "stata", 
                                                      data = gss_analysis, 
                                                      subset =get(treat)==1|control==1))
        names(temp_yr) <- models
        
        for(model in names(temp_yr)) {
          temp_tidy <- add_complete_treat_ft(temp_yr[[model]], model = model, treat_var = treat) %>% 
            mutate(int = int, 
                   treat_period = days,
                   fixed = fixed)
          
          
          
          tidy_models <- rbind(tidy_models, temp_tidy)
        }
        
      } else if(fixed=="year + region"){
        temp_yr_rg <- lapply(models,
                             FUN = function(x) lm_robust(formula(paste(dv, x)),
                                                         fixed_effects = ~ as.factor(year) + as.factor(region),
                                                         se_type = "stata",
                                                         data = gss_analysis,
                                                         subset = get(treat)==1|control==1))
        names(temp_yr_rg) <- models

        for(model in names(temp_yr)) {
          temp_tidy <- add_complete_treat_ft(temp_yr_rg[[model]], model = model, treat_var = treat) %>% 
            mutate(int = int, 
                   treat_period = days,
                   fixed = fixed)
          
          tidy_models <- rbind(tidy_models, temp_tidy)
        }
      }
    }
  }
}
Sys.time() - time 

tidy_models_complete <- tidy_models

# Only select treatment effect
tidy_models <- tidy_models %>%
  filter(term %in% c("treat_7", "treat_14", "treat_21")|str_detect(term, "full_"))

tidy_models <- tidy_models %>%
  mutate("Party Affil." = ifelse(str_detect(model, "party_cat"), 1, 0),
         "Sex" = ifelse(str_detect(model, "female"), 1, 0),
         "Age" = ifelse(str_detect(model, "age"), 1, 0),
         "U.S. born" = ifelse(str_detect(model, "born"), 1, 0),
         "Race/Ethnicity" = ifelse(str_detect(model, "race"), 1, 0),
         "Residence" = ifelse(str_detect(model, "residence"), 1, 0),
         "Education" = ifelse(str_detect(model, "educ"), 1, 0),
         "Married" = ifelse(str_detect(model, "married"), 1, 0),
         "Owns Gun" = ifelse(str_detect(model, "owngun"), 1, 0),
         "Coop. Interview" = ifelse(str_detect(model, "cooperative"), 1, 0)) %>% 
  mutate(treat_period = paste(treat_period, "days"),
         treat_period = factor(treat_period, levels = c("7 days", "14 days", "21 days")), 
         fixed = ifelse(fixed=="year", "Year", 
                        ifelse(fixed=="year + region", "Year + Region", NA)), 
         fixed = factor(fixed, levels = c("Year", "Year + Region")),
         int = ifelse(int=="", "Treatment",
                      ifelse(int=="_int", "Treatment x Party", NA)), 
         sample = "Full Sample", 
         term = ifelse(term %in% c("treat_7", "treat_14", "treat_21") & int=="Treatment", 
                       "Treatment Effect", 
                       ifelse(term %in% c("treat_7", "treat_14", "treat_21") & int=="Treatment x Party", 
                              "Democrat",
                              ifelse(term %in% c("full_party_catIndependent:treat_7", 
                                                 "full_party_catIndependent:treat_14", 
                                                 "full_party_catIndependent:treat_21"), "Independent", 
                                     ifelse(term %in% c("full_party_catRepublican:treat_7", 
                                                        "full_party_catRepublican:treat_14", 
                                                        "full_party_catRepublican:treat_21"), "Republican", NA)))))
                      


save(tidy_models_complete,
     file = paste0(models_path, "tidy_models_complete.Rda"))

save(tidy_models,
     file = paste0(models_path, "tidy_models.Rda"))
