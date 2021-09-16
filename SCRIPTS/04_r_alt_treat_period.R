# ---
# SUPPLEMENTARY ANALYSIS: ALTERNATIVE TREATMENT PERIOD SPECIFICATIONS (7 & 21 DAYS)
# ---

library(tidyverse)
library(estimatr)
library(texreg)

# Specify paths 

# Specify model
for(i in c(7, 21)) {
  plots_path <- paste0("OUTPUT/PLOTS/supp/r_alt_treat_period/", i, "/")
  tables_path <- paste0("OUTPUT/TABLES/supp/r_alt_treat_period/", i, "/")
  models_path <- paste0("OUTPUT/MODELS/supp/r_alt_treat_period/", i, "/")
  
  treat <- paste0("treat_", i)
  model <- paste(dv, "~", treat)
  controls <- paste(ivs_std, collapse = " + ")
  
  #  SPECS -------------------------------------------------------------------------------------
  
  # restrict to non-missing on IVS, and on correct sample
  gss_analysis <- gss_event %>% 
    filter(sample_ivs == 1) 
  
  
  #foreign::write.dta(gss_event, file = "DATA/EDIT/gss_event.dta")
  
  var_labels <- list("treat_7" = "Treatment", 
                     "treat_7:party_catIndependent" = "Treatment x Independent", 
                     "treat_7:party_catRepublican" = "Treatment x Republican",
                     "treat_14" = "Treatment", 
                     "treat_14:party_catIndependent" = "Treatment x Independent", 
                     "treat_14:party_catRepublican" = "Treatment x Republican",
                     "treat_21" = "Treatment", 
                     "treat_21:party_catIndependent" = "Treatment x Independent", 
                     "treat_21:party_catRepublican" = "Treatment x Republican",
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
  
  m1 <- lm_robust(formula(paste(model, controls, sep = " + ")), 
                    se_type = "stata", fixed_effects = ~ as.factor(year), 
                    data = gss_analysis, subset = (get(treat)==1|control==1))
  
  m2 <- lm_robust(formula(paste(model, paste0(treat, " * party_cat"), controls, sep = " + ")), 
                    se_type = "stata", fixed_effects = ~ as.factor(year), 
                    data = gss_analysis, subset = (get(treat)==1|control==1))
  
  # BY SCHOOL SHOOTING ------------------------
  
  m1_noschool <- lm_robust(formula(paste(model, controls, sep = " + ")), 
                             se_type = "stata", fixed_effects = ~ as.factor(year), 
                             data = gss_analysis, 
                           subset = (get(treat)==1 & school_shooting==0|control==1))
  
  
  m2_noschool <- lm_robust(formula(paste(model, paste0(treat, " * party_cat"), controls, sep = " + ")), 
                             se_type = "stata", fixed_effects = ~ as.factor(year), 
                             data = gss_analysis, 
                           subset = (get(treat)==1 & school_shooting==0|control==1))
  
  m1_school <- lm_robust(formula(paste(model, controls, sep = " + ")), 
                           se_type = "stata", fixed_effects = ~ as.factor(year), 
                           data = gss_analysis, 
                         subset = (get(treat)==1 & school_shooting==1|control==1 & year_school==1))
  
  m2_school <- lm_robust(formula(paste(model, paste0(treat, " * party_cat"), controls, sep = " + ")), 
                           se_type = "stata", fixed_effects = ~ as.factor(year), 
                           data = gss_analysis, 
                         subset = (get(treat)==1 & school_shooting==1|control==1 & year_school==1))
  
  
  # SAVE TABLES ---------------------------------------------------------------------------------
  
  print(screenreg(list(m1, m2),
            include.ci = FALSE, 
            stars = c(0.01, 0.05, 0.1),
            custom.coef.map = var_labels, 
            custom.gof.rows = 
              list("Num. Groups (Year)" = c(length(m1$fixed_effects), length(m2$fixed_effects)))))
  
  texreg(list(m1, m2), 
         digits = 2,
         include.ci = FALSE, 
         stars = c(0.01, 0.05, 0.1),
         booktabs = TRUE,
         custom.coef.map = var_labels, 
         custom.gof.rows = 
           list("Num. Groups (Year)" = c(length(m1$fixed_effects), length(m2$fixed_effects))),
         caption = paste0("Main effects, ", i, " day treatment period"), 
         caption.above = TRUE,
         use.packages = FALSE,
         label = "table_ate_alt_treat", 
         center = TRUE, 
         file = paste0(tables_path, "table_ate.tex"))

  print(screenreg(list(m1_noschool, m2_noschool, m1_school, m2_school),
                  digits = 3,
                  include.ci = FALSE, 
                  stars = c(0.01, 0.05, 0.1),
                  custom.coef.map = var_labels,
                  custom.header = list("No School" = 1:2, "School" = 3:4),
                  custom.gof.rows = 
                    list("Num. Groups (Year)" = c(length(m1_noschool$fixed_effects), length(m2_noschool$fixed_effects),
                                                  length(m1_school$fixed_effects), length(m2_school$fixed_effects)))))
  
  
  texreg(list(m1_noschool, m2_noschool, m1_school, m2_school),    
         digits = 2,
         include.ci = FALSE, 
         stars = c(0.01, 0.05, 0.1),
         booktabs = TRUE,
         custom.coef.map = var_labels, 
         custom.header = list("Non-school" = 1:2, "School" = 3:4),
         custom.model.names = c("Model 1", "Model 2", "Model 1", "Model 2"),
         custom.gof.rows = 
           list("Num. Groups (Year)" = c(length(m1_noschool$fixed_effects), length(m1_noschool$fixed_effects), 
                                         length(m1_school$fixed_effects), length(m1_school$fixed_effects))),
         caption = paste0("Effects by shooting site, ", i, " day treatment period"), 
         caption.above = TRUE,
         use.packages = FALSE,
         label = "table_school_alt_treat", 
         center = TRUE, 
         file = paste0(tables_path, "table_school.tex"))

  
  # Save models ---------------------------------------------------------------------------------
  
  add_complete_treat_ft <- function(data, model, treat_var = treat, specs = NULL, 
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
      
      if (grepl(paste0(treat_var, ":"), j)) {
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
  
  # combine models 
  full_models <- rbind(add_complete_treat_ft(m1, model = "M1", treat_var = treat, 
                                             specs = paste(i, "day treatment"), sample = "Full"), 
                       add_complete_treat_ft(m2, model = "M2", treat_var = treat, 
                                             specs = paste(i, "day treatment"), sample = "Full"), 
                       add_complete_treat_ft(m1_noschool, model = "M1", treat_var = treat, 
                                             specs = paste(i, "day treatment"), sample = "No school"), 
                       add_complete_treat_ft(m2_noschool, model = "M2", treat_var = treat, 
                                             specs = paste(i, "day treatment"), sample = "No school"), 
                       add_complete_treat_ft(m1_school, model = "M1", treat_var = treat, 
                                             specs = paste(i, "day treatment"), sample = "School"), 
                       add_complete_treat_ft(m2_school, model = "M2", treat_var = treat, 
                                             specs = paste(i, "day treatment"), sample = "School"))
  # save models
  saveRDS(full_models, 
          file= paste0(models_path, "models.rds"))
  
  
}
