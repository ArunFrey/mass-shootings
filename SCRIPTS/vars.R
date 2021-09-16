# ---
# VARIABLES USED IN MODELS
# ---


# DEPENDENT VARIABLES -------------------------------------------------------------------------

dv <- "restrict_guns"
treat <- "treat_14"

# For desc stat only
ivs <- c("party_cat", "age", "female", "born", "race", "residence","educ", 
          "married", "owngun", "cooperative")

ivs_std <- c("party_cat", "age_std", "female", "born", "race", "residence","educ_std", 
         "married", "owngun", "cooperative")

ivs_label <- c(
  "Independent", "Republican", 
  "Age", "Female", "US Born", "Black", "Other", "Rural", "Suburban", "Education (yrs.)", 
  "Married", "Gun Ownership", "Coop. Interview")

ivs_label_std <- c(
  "Independent", "Republican", 
  "Age (std.)", "Female", "US Born", "Black", "Other", "Rural", "Suburban", "Education (std.)", 
  "Married", "Gun Ownership", "Coop. Interview")
