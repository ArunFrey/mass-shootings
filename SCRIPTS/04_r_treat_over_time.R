# ---
# SUPPLEMENTARY ANALYSIS: TREATMENT EFFECT OVER TIME
# ---

library(tidyverse)
library(estimatr)
library(texreg)


# Specify paths 
plots_path <- "OUTPUT/PLOTS/supp/r_treat_over_time/"
tables_path <- "OUTPUT/TABLES/supp/r_treat_over_time/"
models_path <- "OUTPUT/MODELS/supp/r_treat_over_time/"

# Specify model
controls <- paste(ivs_std, collapse = " + ")

# function
add_complete_treat_ft <- function(data, model, treat_var = "treat", specs = NULL, 
                                  sample = NULL, nclusters = NULL) {
  tidy_model <- tidy(data) %>%
    mutate(model = model, 
           specs = specs,
           sample = sample, 
           nobs = data$nobs, 
           nclusters = nclusters)
  
  var_cov <- as.data.frame(vcov(data))
  
  # add full terms
  for (j in tidy_model$term) {
    tidy_model <- tidy_model %>%
      mutate(std.error = as.numeric(std.error),
             estimate = as.numeric(estimate))
    
    if (grepl(paste0(":", treat_var, "|", treat_var, ":"), j)) {
      coeff_name <- paste0("full_", j)
      estimate <- tidy_model$estimate[tidy_model$term == treat_var] +
        tidy_model$estimate[tidy_model$term == j]
      treat_loc <- which(names(var_cov) == treat_var)
      j_loc <- which(names(var_cov) == j)
      se <- sqrt(sum(var_cov[c(treat_loc, j_loc), c(treat_loc, j_loc)]))
      new_row <- c(coeff_name, estimate, se, NA, NA, NA, NA, NA, 
                   data$outcome, model, specs, sample, data$nobs, nclusters)
      
      tidy_model <- rbind(tidy_model, new_row) %>%
        mutate_at(vars(estimate, std.error, statistic, p.value, conf.low, conf.high, df, nobs, nclusters), 
                  function(x) as.numeric(x))
    }
  }
  return(tidy_model)
}

#  SPECS -------------------------------------------------------------------------------------

# restrict to non-missing on IVS
gss_analysis <- gss_event %>% 
  filter(sample_ivs == 1)


#foreign::write.dta(gss_event, file = "DATA/EDIT/gss_event.dta")

var_labels <- list("treat_14" = "Treatment", 
                   "treat_14:party_catIndependent" = "Treatment x Independent", 
                   "treat_14:party_catRepublican" = "Treatment x Republican",
                   "party_catIndependent" = "Independent",
                   "party_catRepublican" = "Republican", 
                   "age_std" = "Age (std.)", 
                   "female" = "Female", 
                   "born" = "U.S. born", 
                   "raceWhite" = "White", 
                   "raceOther" = "Other", 
                   "residenceSuburban" = "Suburban", 
                   "residenceRural" = "Rural", 
                   "educ_std" = "Education (std.)", 
                   "married" = "Married", 
                   "owngun" = "Owns Gun", 
                   "cooperative" = "Coop. Interview")

# ANALYSIS ------------------------------------------------------------------------------------
full_models <- c()

for(i in 1:28) {
  treat_var <- paste0("treat_", i)
  model <- paste(dv, "~ ", treat_var)
  
  m1 <- lm_robust(formula(paste(model, controls, sep = " + ")), 
                  se_type = "stata", fixed_effects = ~ as.factor(year), 
                  data = gss_analysis, subset = (get(treat_var)==1|control==1))
  
  m2 <- lm_robust(formula(paste(model, paste0(treat_var, " * party_cat + "), controls, sep = " + ")), 
                  se_type = "stata", fixed_effects = ~ as.factor(year), 
                  data = gss_analysis, subset = (get(treat_var)==1|control==1))
  
  # By school shooting
  
  m1_noschool <- lm_robust(formula(paste(model, controls, sep = " + ")), 
                           se_type = "stata", fixed_effects = ~ as.factor(year), 
                           data = gss_analysis, 
                           subset = (get(treat_var)==1 & school_shooting==0|control==1))
  
  
  m2_noschool <- lm_robust(formula(paste(model, paste0(treat_var, " * party_cat + "), controls, sep = " + ")), 
                           se_type = "stata", fixed_effects = ~ as.factor(year), 
                           data = gss_analysis, 
                           subset = (get(treat_var)==1 & school_shooting==0|control==1))
  
  m1_school <- lm_robust(formula(paste(model, controls, sep = " + ")), 
                         se_type = "stata", fixed_effects = ~ as.factor(year), 
                         data = gss_analysis, 
                         subset = (get(treat_var)==1 & school_uni_shooting==1|control==1 & year_school==1))
  
  m2_school <- lm_robust(formula(paste(model, paste0(treat_var, " * party_cat + "), controls, sep = " + ")), 
                         se_type = "stata", fixed_effects = ~ as.factor(year), 
                         data = gss_analysis, 
                         subset = (get(treat_var)==1 & school_uni_shooting==1|control==1 & year_school==1))
  
  # combine models 
  full_models <- rbind(full_models, 
                       add_complete_treat_ft(m1, model = "M1", treat_var = treat_var, 
                                             specs = "Incl. controls", sample = "Full"), 
                       add_complete_treat_ft(m2, model = "M2", treat_var = treat_var, 
                                             specs = "Incl. controls", sample = "Full"), 
                       add_complete_treat_ft(m1_noschool, model = "M1", treat_var = treat_var, 
                                             specs = "Incl. controls", sample = "No school"), 
                       add_complete_treat_ft(m2_noschool, model = "M2", treat_var = treat_var, 
                                             specs = "Incl. controls", sample = "No school"), 
                       add_complete_treat_ft(m1_school, model = "M1", treat_var = treat_var, 
                                             specs = "Incl. controls", sample = "School"), 
                       add_complete_treat_ft(m2_school, model = "M2", treat_var = treat_var, 
                                             specs = "Incl. controls", sample = "School"))
}

# save models
saveRDS(full_models,
        file= paste0(models_path, "models.rds"))

