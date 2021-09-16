# ---
# SUPPLEMENTARY ANALYSIS: MATCHING
# ---

library(tidyverse)
library(estimatr)
library(texreg)
library(WeightIt)
library(cobalt)

# Specify paths 
plots_path <- "OUTPUT/PLOTS/supp/r_matching/"
tables_path <- "OUTPUT/TABLES/supp/r_matching/"
models_path <- "OUTPUT/MODELS/supp/r_matching/"

# Specify model
model <- paste(dv, "~ treat_14")
controls <- paste(ivs_std, collapse = " + ")
controls <- paste(controls, "+ occ_gun") # also balance on occupations that involve gun ownership

#  SPECS -------------------------------------------------------------------------------------

# restrict to non-missing on IVS
gss_analysis <- gss_event %>% 
  filter(sample_ivs == 1) %>%
  filter(!is.na(occ_gun))

# restrict to non-missing on treats, since its not allowed in weightit
gss_analysis <- gss_analysis %>%
  filter(!is.na(treat_14))
  
var_labels <- list("treat_14" = "Treatment", 
                   "treat_14:party_catIndependent" = "Treat. x Independent",
                   "treat_14:party_catRepublican" = "Treat. x Republican",
                   "party_catIndependent" = "Independent",
                   "party_catRepublican" = "Republican", 
                   "party_cat_Democrat" = "Democrat",
                   "party_cat_Independent" = "Independent",
                   "party_cat_Republican" = "Republican", 
                   "age_std" = "Age (std.)", 
                   "female" = "Female", 
                   "born" = "U.S. born", 
                   "raceWhite" = "White", 
                   "raceOther" = "Other", 
                   "race_Black" = "Black", 
                   "race_White" = "White", 
                   "race_Other" = "Other", 
                   "residenceSuburban" = "Suburban", 
                   "residenceRural" = "Rural", 
                   "residence_Urban" = "Urban", 
                   "residence_Suburban" = "Suburban", 
                   "residence_Rural" = "Rural", 
                   "educ_std" = "Education (std.)", 
                   "married" = "Married", 
                   "owngun" = "Owns Gun", 
                   "occ_gun" = "Military/police/security",
                   "cooperative" = "Coop. Interview")

# ANALYSIS ------------------------------------------------------------------------------------

# generate weights ----
model <- formula(paste0(treat, " ~ ", controls))

# propensity scores
m_ps <- weightit(model, data = gss_analysis, 
                 method = "ps", replace = T, estimand = "ATE")

m_ps_school <- weightit(model, data = gss_analysis[gss_analysis$year_school==1, ], 
                 method = "ps", replace = T, estimand = "ATE")

# entropy balancing (matching on mean, variance, and skew)
m_ebal <- weightit(model, data = gss_analysis, 
                   method = "ebal", moments = 3, replace = T, estimand = "ATE")

m_ebal_school <- weightit(model, data = gss_analysis[gss_analysis$year_school==1, ], 
                   method = "ebal", moments = 3, replace = T, estimand = "ATE")


# check balance
bal.tab(m_ps, un = TRUE, m.threshold = .1)
bal.tab(m_ebal, un = TRUE, m.threshold = .1)


# plot results
set.cobalt.options(binary = "std")

g1 <- love.plot(model,
                stars = "raw",
                data = gss_analysis, 
                position = "top",
                title = "",
                estimand = "ATE",
                weights = list(w1 = get.w(m_ps),
                               w2 = get.w(m_ebal)),
                var.order = "unadjusted",
                abs = TRUE,
                alpha = 1,
                size = 3.5,
                thresholds = c(m = .1),
                var.names = var_labels,
                shapes = c("triangle filled", "circle filled", "square filled"),
                sample.names = c("Unweighted", "Propensity Score", "Entropy Balance"))

g1 + theme_bw(base_size = 15) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("#5b5b5b", "red", "#d8c292"))


ggsave(paste0(plots_path, "bal_plot.pdf"), width = 7.5)

# RUN MATCHED REGRESSIONS -----

# save weights 
gss_analysis <- gss_analysis %>%
  mutate(w_ps = get.w(m_ps),
         w_ebal = get.w(m_ebal), 
         w_ps_school = ifelse(year_school==1, get.w(m_ps_school), NA),
         w_ebal_school = ifelse(year_school==1,get.w(m_ebal_school), NA))


model <- paste(dv, "~ treat_14")

# RUN MODELS

# Prop score
m1_ps <- lm_robust(formula(paste(model, "party_cat", sep = " + ")),  
                        weights = w_ps, se_type = "stata", fixed_effects = ~ as.factor(year), 
                        data = gss_analysis, subset = (treat_14==1|control==1))

m2_ps <- lm_robust(formula(paste(model, " + treat_14 * party_cat")), 
                  weights = w_ps, se_type = "stata", fixed_effects = ~ as.factor(year), 
                  data = gss_analysis, subset = (treat_14==1|control==1))

# E-Balance
m1_ebal <- lm_robust(formula(paste(model, "party_cat", sep = " + ")),  
                   weights = w_ebal, se_type = "stata", fixed_effects = ~ as.factor(year), 
                   data = gss_analysis, subset = (treat_14==1|control==1))

m2_ebal <- lm_robust(formula(paste(model, " + treat_14 * party_cat")), 
                   weights = w_ebal, se_type = "stata", fixed_effects = ~ as.factor(year), 
                   data = gss_analysis, subset = (treat_14==1|control==1))




# BY SCHOOL SHOOTING ------------------------

# Prop Score
m1_noschool_ps <- lm_robust(formula(paste(model, "party_cat", sep = " + ")),  
                         weights = w_ps, se_type = "stata", fixed_effects = ~ as.factor(year), 
                         data = gss_analysis, 
                         subset = (treat_14==1 & school_shooting==0|control==1))

m2_noschool_ps <- lm_robust(formula(paste(model, "treat_14 * party_cat", sep = " + ")), 
                         weights = w_ps, se_type = "stata", fixed_effects = ~ as.factor(year), 
                         data = gss_analysis, 
                         subset = (treat_14==1 & school_shooting==0|control==1))

m1_school_ps <- lm_robust(formula(paste(model, "party_cat", sep = " + ")),  
                          weights = w_ps_school, se_type = "stata", fixed_effects = ~ as.factor(year), 
                          data = gss_analysis,
                          subset = (treat_14==1 & school_shooting==1|control==1 & year_school==1))

m2_school_ps <- lm_robust(formula(paste(model, "treat_14 * party_cat", sep = " + ")), 
                          weights = w_ps_school, se_type = "stata", fixed_effects = ~ as.factor(year), 
                          data = gss_analysis,
                          subset = (treat_14==1 & school_shooting==1|control==1 & year_school==1))


# E-Balance
m1_noschool_ebal <- lm_robust(formula(paste(model, "party_cat", sep = " + ")),  
                            weights = w_ebal, se_type = "stata", fixed_effects = ~ as.factor(year), 
                            data = gss_analysis, 
                            subset = (treat_14==1 & school_shooting==0|control==1))

m2_noschool_ebal <- lm_robust(formula(paste(model, "treat_14 * party_cat", sep = " + ")), 
                            weights = w_ebal, se_type = "stata", fixed_effects = ~ as.factor(year), 
                            data = gss_analysis, 
                            subset = (treat_14==1 & school_shooting==0|control==1))

m1_school_ebal <- lm_robust(formula(paste(model, "party_cat", sep = " + ")),  
                          weights = w_ebal_school, se_type = "stata", fixed_effects = ~ as.factor(year), 
                          data = gss_analysis, 
                          subset = (treat_14==1 & school_shooting==1|control==1 & year_school==1))

m2_school_ebal <- lm_robust(formula(paste(model, "treat_14 * party_cat", sep = " + ")), 
                          weights = w_ebal_school, se_type = "stata", fixed_effects = ~ as.factor(year), 
                          data = gss_analysis, 
                          subset = (treat_14==1 & school_shooting==1|control==1 & year_school==1))

# SAVE TABLES ---------------------------------------------------------------------------------

screenreg(list(m1_ps, m2_ps, m1_ebal, m2_ebal),
          include.ci = FALSE, 
          stars = c(0.01, 0.05, 0.1),
          custom.coef.map = var_labels, 
          custom.gof.rows = 
            list("Num. Groups (Year)" = c(length(m1_ps$fixed_effects), length(m2_ps$fixed_effects),
                                          length(m1_ebal$fixed_effects), length(m2_ebal$fixed_effects))))

texreg(list(m1_ps, m2_ps, m1_ebal, m2_ebal), 
       digits = 2,
       include.ci = FALSE, 
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       custom.coef.map = var_labels, 
       custom.header = list("Prop. Score" = 1:2, "Entropy Bal." = 3:4),
       custom.gof.rows = 
         list("Num. Groups (Year)" = c(length(m1_ps$fixed_effects), length(m2_ps$fixed_effects),
                                       length(m1_ebal$fixed_effects), length(m2_ebal$fixed_effects))),
       caption = "Main effects, covariate balancing", 
       caption.above = TRUE,
       use.packages = FALSE,
       label = "table_ate_balance", 
       center = TRUE, 
       file = paste0(tables_path, "table_ate.tex"))

texreg(list(m1_noschool_ps, m2_noschool_ps, m1_school_ps, m2_school_ps, 
            m1_noschool_ebal, m2_noschool_ebal, m1_school_ebal, m2_school_ebal),
       digits = 2,
       include.ci = FALSE, 
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       custom.coef.map = var_labels, 
       custom.header = list("Non-school" = 1:2, "School" = 3:4, "Non-school" = 5:6, "School" = 7:8),
       custom.model.names = c("Model 1", "Model 2", "Model 1", "Model 2", 
                              "Model 1", "Model 2", "Model 1", "Model 2"),
       custom.gof.rows = 
         list("Balancing" = c("Prop. S.", "Prop. S.", "Prop. S.", "Prop. S.", 
                              "Ent. B.", "Ent. B.", "Ent. B.", "Ent. B."),
              "Year FE" = c(length(m1_noschool_ps$fixed_effects), length(m2_noschool_ps$fixed_effects),
                                       length(m1_school_ps$fixed_effects), length(m2_school_ps$fixed_effects), 
                                       length(m1_noschool_ebal$fixed_effects), length(m2_noschool_ebal$fixed_effects),
                                       length(m1_school_ebal$fixed_effects), length(m2_school_ebal$fixed_effects))),
       caption = "Effects by shooting site, covariate balancing", 
       sideways = TRUE,
       caption.above = TRUE,
       use.packages = FALSE,
       label = "table_school_balance", 
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
full_models <- rbind(add_complete_treat_ft(m1_ps, model = "M1", treat_var = "treat_14", 
                                           specs = "Propensity Score", sample = "Full"), 
                     add_complete_treat_ft(m2_ps, model = "M2", treat_var = "treat_14", 
                                           specs = "Propensity Score", sample = "Full"), 
                     add_complete_treat_ft(m1_ebal, model = "M1", treat_var = "treat_14", 
                                           specs = "Entropy Balance", sample = "Full"), 
                     add_complete_treat_ft(m2_ebal, model = "M2", treat_var = "treat_14", 
                                           specs = "Entropy Balance", sample = "Full"), 
                     add_complete_treat_ft(m1_noschool_ps, model = "M1", treat_var = "treat_14", 
                                           specs = "Propensity Score", sample = "No school"), 
                     add_complete_treat_ft(m2_noschool_ps, model = "M2", treat_var = "treat_14", 
                                           specs = "Propensity Score", sample = "No school"), 
                     add_complete_treat_ft(m1_noschool_ebal, model = "M1", treat_var = "treat_14", 
                                           specs = "Entropy Balance", sample = "No school"), 
                     add_complete_treat_ft(m2_noschool_ebal, model = "M2", treat_var = "treat_14", 
                                           specs = "Entropy Balance", sample = "No school"), 
                     add_complete_treat_ft(m1_school_ps, model = "M1", treat_var = "treat_14", 
                                           specs = "Propensity Score", sample = "School"), 
                     add_complete_treat_ft(m2_school_ps, model = "M2", treat_var = "treat_14", 
                                           specs = "Propensity Score", sample = "School"), 
                     add_complete_treat_ft(m1_school_ebal, model = "M1", treat_var = "treat_14", 
                                           specs = "Entropy Balance", sample = "School"), 
                     add_complete_treat_ft(m2_school_ebal, model = "M2", treat_var = "treat_14", 
                                           specs = "Entropy Balance", sample = "School"))

# save models
saveRDS(full_models, 
        file= paste0(models_path, "models.rds"))

