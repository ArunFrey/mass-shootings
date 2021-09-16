# ---
# Supplementary analysis: logit
# ---

library(tidyverse)
library(estimatr)
library(texreg)


# Specify paths 
plots_path <- "OUTPUT/PLOTS/supp/r_logit/"
tables_path <- "OUTPUT/TABLES/supp/r_logit/"
models_path <- "OUTPUT/MODELS/supp/r_logit/"

# Specify model
model <- paste(dv, "~ treat_14")
controls <- paste(ivs_std, collapse = " + ")

#  SPECS -------------------------------------------------------------------------------------

# restrict to non-missing on IVS
gss_analysis <- gss_event %>% 
  filter(sample_ivs == 1)


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

m1 <- glm(formula(paste(model, controls, "as.factor(year)", sep = " + ")), 
          data = gss_analysis, subset = (treat_14==1|control==1), family = "binomial")

m2 <- glm(formula(paste(model, "treat_14 * party_cat + ", controls, "as.factor(year)", sep = " + ")), 
          data = gss_analysis, subset = (treat_14==1|control==1), family = "binomial")

# BY SCHOOL SHOOTING ------------------------
m1_noschool <- glm(formula(paste(model, controls, "as.factor(year)", sep = " + ")), 
                         data = gss_analysis, family = "binomial",
                         subset = (treat_14==1 & school_shooting==0|control==1))


m2_noschool <- glm(formula(paste(model, "treat_14 * party_cat", controls, "as.factor(year)", sep = " + ")), 
                         data = gss_analysis, family = "binomial",
                         subset = (treat_14==1 & school_shooting==0|control==1))

m1_school <- glm(formula(paste(model, controls, "as.factor(year)", sep = " + ")), 
                 data = gss_analysis, family = "binomial",
                 subset = (treat_14==1 & school_shooting==1|control==1 & year_school==1))

m2_school <- glm(formula(paste(model, "treat_14 * party_cat", controls, "as.factor(year)", sep = " + ")), 
                       data = gss_analysis, family = "binomial",
                       subset = (treat_14==1 & school_shooting==1|control==1 & year_school==1))


# SAVE TABLES ---------------------------------------------------------------------------------

screenreg(list(m1, m2),
          include.ci = FALSE, 
          stars = c(0.01, 0.05, 0.1),
          custom.coef.map = var_labels, 
          custom.gof.rows = 
            list("Num. Groups (Year)" = c(12, 12)))

texreg(list(m1, m2), 
       digits = 3,
       include.ci = FALSE, 
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       custom.coef.map = var_labels, 
       custom.gof.rows = 
         list("Num. Groups (Year)" = c(12, 12)), 
       caption = "Effect of mass shootings on attitudes towards gun permits, logit", 
       caption.above = TRUE,
       use.packages = FALSE,
       label = "table_ate_logit", 
       center = TRUE, 
       file = paste0(tables_path, "table_ate.tex"))

screenreg(list(m1_noschool, m2_noschool, m1_school, m2_school),
          include.ci = FALSE, 
          stars = c(0.01, 0.05, 0.1),
          custom.coef.map = var_labels,
          custom.header = list("Non-school" = 1:2, "School" = 3:4),
          custom.gof.rows = 
            list("Num. Groups (Year)" = c(12, 12, 2, 2)))


texreg(list(m1_noschool, m2_noschool, m1_school, m2_school),  
       digits = 3,
       include.ci = FALSE, 
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       custom.coef.map = var_labels, 
       custom.header = list("Non-school" = 1:2, "School" = 3:4),
       custom.model.names = c("Model 1", "Model 2", "Model 1", "Model 2"),
       custom.gof.rows = 
         list("Num. Groups (Year)" = c(12, 12, 2, 2)), 
       caption = "Effect of mass shootings on attitudes towards gun permits by shooting site, logit", 
       caption.above = TRUE,
       use.packages = FALSE,
       label = "table_school_logit", 
       center = TRUE, 
       file = paste0(tables_path, "table_school.tex"))


# Save models ---------------------------------------------------------------------------------

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
      new_row <- c(coeff_name, estimate, se, NA, NA, 
                   data$outcome, model, specs, sample, data$nobs, nclusters)
      
      tidy_model <- rbind(tidy_model, new_row) %>%
        mutate_at(vars(estimate, std.error, statistic, p.value), 
                  function(x) as.numeric(x))
    }
  }
  return(tidy_model)
}

# combine models 
full_models <- rbind(add_complete_treat_ft(m1, model = "M1", treat_var = "treat_14", 
                                           specs = "Incl. controls", sample = "Full"), 
                     add_complete_treat_ft(m2, model = "M2", treat_var = "treat_14", 
                                           specs = "Incl. controls", sample = "Full"), 
                     add_complete_treat_ft(m1_noschool, model = "M1", treat_var = "treat_14", 
                                           specs = "Incl. controls", sample = "No school"), 
                     add_complete_treat_ft(m2_noschool, model = "M2", treat_var = "treat_14", 
                                           specs = "Incl. controls", sample = "No school"), 
                     add_complete_treat_ft(m1_school, model = "M1", treat_var = "treat_14", 
                                           specs = "Incl. controls", sample = "School"), 
                     add_complete_treat_ft(m2_school, model = "M2", treat_var = "treat_14", 
                                           specs = "Incl. controls", sample = "School"))
# save models
saveRDS(full_models, 
        file= paste0(models_path, "models.rds"))

