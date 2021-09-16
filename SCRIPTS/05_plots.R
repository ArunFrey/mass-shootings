# ---
# PLOTTING TREATMENT EFFECTS
# ---

library(tidyverse)

theme_set(theme_bw(base_size = 16))

# specify plot widths and heights
plot_width <- 5
plot_height <- 6

  
# Plotting functions --------------------------------------------------------------------------

# Load model
load_model <- function(path_to_rds_file) {
  data <- readRDS(path_to_rds_file)
  
  data <- data %>% 
    # only select treatment variables
    filter(str_detect(term, "treat_")) %>% 
    filter(nchar(term) <=8|str_detect(term, "full_")) %>%
    mutate(days = as.numeric(stringr::str_extract(term, "\\d{1,2}")), 
             term = ifelse(str_detect(term, "treat_\\d{1,2}") & model == "M1", "Total", 
                         ifelse(str_detect(term, "treat_\\d{1,2}$") & model == "M2", "Democrat", 
                                ifelse(str_detect(term, "full.+Independent")  & model == "M2", "Independent", 
                                       ifelse(str_detect(term, "full.+Republican")  & model == "M2", "Republican", 
                                              ifelse(str_detect(term, "full.+Other")  & model == "M2", "Other", NA))))),
           term = factor(term, levels = c("Total", "Democrat", "Independent", "Republican", "Other")), 
           model = ifelse(model=="M1", "Model 1", 
                          ifelse(model=="M2", "Model 2", NA)), 
           sample = ifelse(sample=="Full", "Full sample", 
                           ifelse(sample=="School", "At school", 
                                  ifelse(sample=="No school", "Not at school", sample))),
           sample = factor(sample, levels = c("Full sample", "At school", "Not at school", "White", "Non-white")), 
           plot_type = ifelse(sample=="Full sample", "Main effects", 
                              ifelse(sample %in% c("At school", "Not at school"),
                                     "By shooting site", NA)),
           plot_type = factor(plot_type, levels = c("Main effects", "By shooting site")))
  
  return(data)
}

plot_model <- function(data, subset = FALSE, subset_sample = NULL, show_line = F, 
                       matching = FALSE, school_uni_shooting = F, party_cat2 = F) {
  
  if(subset == TRUE) {
    data <- data %>%
      filter(sample %in% subset_sample)
  }
  
  plot <- data %>% 
    ggplot(aes(y = reorder(term, desc(term)), 
               x = estimate, 
               xmin = estimate - 1.96 * std.error, 
               xmax = estimate + 1.96 * std.error,
               color = term, shape = term, group = sample, linetype = sample),
           position = position_dodge(width = -.5)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.7, color = "darkgrey") +
    geom_point(size = 2.5, 
               position = position_dodge(width=-0.5),
               show.legend = FALSE) +
    geom_errorbar(width = 0, size = 0.7,
                  position = position_dodge(width=-0.5)) +
    facet_grid(model~plot_type, scales  = "free_y", space = "free") +
    scale_color_manual(labels = c("Total", "Democrat", "Independet", "Republican"),
                       values = c("black", "#30a2da", "#e5ae38", "#fc4f30")) +
    scale_linetype_discrete(name = "Shooting Site") +
    scale_x_continuous(breaks = seq(-1, 2, 0.05)) +
    xlab("Treatment effect") +
    coord_cartesian(xlim = c(-0.09, 0.18)) + 
    theme(axis.title.y = element_blank(),
          panel.grid = element_line(size = 0.5))
  
  if(show_line==FALSE) {
    plot <- plot + guides(
      color = FALSE,
      linetype = FALSE)
  } else {
    plot <- plot + guides(
      color = FALSE) +
      theme(legend.position = "top")
  }
  
  if(matching==T) {
    plot <- plot + facet_grid(model~specs, scales  = "free_y", space = "free")
  }
  
  if(school_uni_shooting==T) {
    plot <- plot + 
      scale_linetype_discrete(name = "Shooting Site", 
                              labels = c("At school/college", "Not at school/college"))
  }
  
  if(party_cat2==T) {
    plot <- plot +
      scale_color_manual(labels = c("Total", "Democrat", "Independet", "Republican", "Other"),
                       values = c("black", "#30a2da", "#e5ae38", "#fc4f30", "grey50")) + 
      scale_x_continuous(breaks = seq(-0.2, 2, 0.1))
  }

    return(plot)
}

# Main plot: Treatment effect -----------------------------------------------------------------

models_path <- "OUTPUT/MODELS/main/"
plots_path <- "OUTPUT/PLOTS/main/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T, subset_sample = "Full sample")
g2 <- plot_model(models, subset = T, subset_sample = c("Not at school", "At school"), show_line = T)

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width, height = plot_height-1)
ggsave(g2, file = paste0(plots_path, "treat_school.pdf"), width = plot_width, height = plot_height)


# Shootings with 10+ fatalities only ----------------------------------------------------------
models_path <- "OUTPUT/MODELS/supp/r_10+_fatalities/"
plots_path <- "OUTPUT/PLOTS/supp/r_10+_fatalities/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), 
                 show_line = T) +
  coord_cartesian(c(-0.2, 0.4)) +
  scale_x_continuous(breaks = seq(-0.2, 0.4, 0.1))

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height-1)


# Alternative treatment period ----------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_alt_treat_period/"
plots_path <- "OUTPUT/PLOTS/supp/r_alt_treat_period/"

for(i in c(7, 21)) {
  models <- load_model(paste0(models_path, i, "/models.rds"))
  
  g1 <- plot_model(models, subset = T,
                   subset_sample = c("Full sample", "Not at school", "At school"), 
                   show_line = T)
  
  ggsave(g1, file = paste0(plots_path, i, "/treat.pdf"), width = plot_width*2, height = plot_height)
}

# Logit ----------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_logit/"
plots_path <- "OUTPUT/PLOTS/supp/r_logit/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), 
                 show_line = T) +
  coord_cartesian(c(-0.6, 1.6)) +
  scale_x_continuous(breaks = seq(-0.6, 1.6, 0.6 ))

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height-1)



# Alternative treatment: exclude cases with less than 20 obs ----------------------------------

models_path <- "OUTPUT/MODELS/supp/r_treat_20+_obs/"
plots_path <- "OUTPUT/PLOTS/supp/r_treat_20+_obs/"

models <- load_model(paste0(models_path, "models.rds"))

g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), show_line = T)

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)


# No controls ---------------------------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_no_controls/"
plots_path <- "OUTPUT/PLOTS/supp/r_no_controls/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), 
                 show_line = T)

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)

# Alternative party coding --------------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_alt_party_coding/"
plots_path <- "OUTPUT/PLOTS/supp/r_alt_party_coding/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T, party_cat2 = T, show_line = T,
                 subset_sample = c("Full sample", "Not at school", "At school")) + 
  coord_cartesian(xlim = c(-0.25, 0.25))

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)


# Controlling for prior shootings --------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_prior_shootings//"
plots_path <- "OUTPUT/PLOTS/supp/r_prior_shootings/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), 
                 show_line = T)

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)

# Matching ------------------------------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_matching/"
plots_path <- "OUTPUT/PLOTS/supp/r_matching/"

models <- load_model(paste0(models_path, "models.rds"))

g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample"), matching = T)

g2 <- plot_model(models, subset = T,
                 subset_sample = c("Not at school", "At school"), 
                 show_line = T, matching = T)

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)
ggsave(g2, file = paste0(plots_path, "treat_school.pdf"), width = plot_width*2, height = plot_height)

# Seasonality (Month Fixed effects) ----------------------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_month/"
plots_path <- "OUTPUT/PLOTS/supp/r_month/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), 
                 show_line = T) +
  coord_cartesian(xlim = c(-0.1, 0.22))

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)



# Year & Region FE ----------------------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_region_FE/"
plots_path <- "OUTPUT/PLOTS/supp/r_region_FE/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model(models, subset = T,
                 subset_sample = c("Full sample", "Not at school", "At school"), 
                 show_line = T)

ggsave(g1, file = paste0(plots_path, "treat.pdf"), width = plot_width*2, height = plot_height)



# School and Uni Shooting ---------------------------------------------------------------------

models_path <- "OUTPUT/MODELS/supp/r_school_uni_shooting/"
plots_path <- "OUTPUT/PLOTS/supp/r_school_uni_shooting/"

models <- load_model(paste0(models_path, "models.rds"))
g2 <- plot_model(models, subset = T, school_uni_shooting = T,
                 subset_sample = c("Not at school", "At school"), 
                 show_line = T)

ggsave(g2, file = paste0(plots_path, "treat_school.pdf"), width = plot_width+2, height = plot_height)


# Treatment effect over time ------------------------------------------------------------------

# function
plot_model_over_time <- function(data, day_cutoff = 21, ylim_cutoff = c(-0.1, 0.16)) {
  data <- data %>% 
    mutate(sample = ifelse(sample=="At school", "School shooting", 
                           ifelse(sample=="Not at school", "Non-school shooting", "Full sample")),
           sample = factor(sample, levels = c("Full sample", "School shooting", "Non-school shooting"))) 
  
  plot <- data %>% 
    filter(days <= day_cutoff) %>% 
    ggplot(aes(x = days, y = estimate, group = term, color = term, fill = term, 
               ymin = estimate-1.96*std.error, ymax = estimate+1.96*std.error, alpha = 1/2)) +
    geom_point() +
    geom_ribbon() +
    geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
    facet_grid(sample ~ term) +
    scale_color_manual(labels = c("Total", "Democrat", "Independet", "Republican"),
                       values = c("black", "#30a2da", "#e5ae38", "#fc4f30")) +
    scale_fill_manual(labels = c("Total", "Democrat", "Independet", "Republican"),
                      values = c("black", "#30a2da", "#e5ae38", "#fc4f30")) +
    coord_cartesian(xlim = c(0, 21), ylim = ylim_cutoff) + 
    xlab("Treatment period (in days)") +
    ylab("Treatment effect") + 
    theme(legend.position = "none", 
          axis.title.y = element_text(angle = 270))
  
  return(plot)
}


models_path <- "OUTPUT/MODELS/supp/r_treat_over_time/"
plots_path <- "OUTPUT/PLOTS/supp/r_treat_over_time/"

models <- load_model(paste0(models_path, "models.rds"))
g1 <- plot_model_over_time(models)

ggsave(g1, file = paste0(plots_path, "treat_over_time.pdf"), 
       width = plot_width*2, height = plot_height+1)


# Specification Curve -------------------------------------------------------------------------

# FUNCTIONS TO CREATE TOP AND BOTTOM PLOT OF SPECIFICATION CURVE 
make_coef_plot <- function(data, subset = "Treatment Effect", treat_period = c("7 days", "14 days", "21 days")) {
  
  data <- data %>%
    filter(term == subset) %>% 
    filter(treat_period == treat_period)
  
  # plot specification curve
  g1 <- data %>%
    ggplot(aes(x = h_order, y = estimate, fill = term)) +
    geom_ribbon(aes(ymin = estimate - 1.96* std.error,
                    ymax = estimate + 1.96* std.error), 
                size = 0.2, alpha = 0.5) +
    geom_point(aes(color = term), shape = 21, size = 0.2) +
    ylab("Treatment Coefficient") +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = 2) +
    facet_wrap(~term, scales = "free_x") +
    coord_cartesian(ylim = c(-0.07, 0.20)) +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 16),
          axis.title.y = element_text(size = 14))
  
    if(str_detect(subset, "Treatment Effect")){
      g1 <- g1 +
        scale_color_manual(guide = FALSE, values = c("black")) +
        scale_fill_manual(guide = FALSE, values = c("black"))
      } else if(str_detect(subset, "Democrat")){
      g1 <- g1 +
        scale_color_manual(guide = FALSE, values = c("#30a2da")) +
        scale_fill_manual(guide = FALSE, values = c("#30a2da"))
      } else if(str_detect(subset, "Independent")){
      g1 <- g1 +
        scale_color_manual(guide = FALSE, values = c("#e5ae38")) +
        scale_fill_manual(guide = FALSE, values = c("#e5ae38"))
      } else if(str_detect(subset, "Republican")){
      g1 <- g1 +
        scale_color_manual(guide = FALSE, values = c("#fc4f30")) +
        scale_fill_manual(guide = FALSE, values = c("#fc4f30"))
      }
  return(g1)
}

# BOTTOM PLOT
# Function to create a specification plot for a single category.
make_spec_plot <- function(data, category, subset = "Treatment Effect") {
  
  data <- data %>%
    filter(term == subset)
  
  # category = spec_cols[1] # DEBUG
  specs <- data %>%
    dplyr::select(h_order, category) %>%
    pivot_longer(starts_with(category), names_prefix = paste0(category, "_")) %>%
    mutate(name = factor(name, levels = rev(unique(name))))
  
  if(is.numeric(specs$value)){
    specs <- specs %>% mutate(h_order = ifelse(value==0, NA, h_order))
    spec_plot <- ggplot(specs, aes(x = h_order, y = reorder(name, desc(name)))) +
      geom_point(shape = "|", size = 0.5) +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), 
            panel.grid.major = element_line(size = 0.5),
            panel.grid.minor = element_line(size = 0.25),
            axis.text.y = element_text(size = 13),
            strip.text = element_text(size = 16))
    
    
  } else {
    spec_plot <- ggplot(specs, aes(x = h_order, y = value)) +
      geom_point(shape = "|", size = 0.5) +
      scale_alpha_continuous(guide = FALSE, range = c(0, 1)) +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), 
            axis.text.y = element_text(size = 13),
            panel.grid.major = element_line(size = 0.5),
            panel.grid.minor = element_line(size = 0.25),
            strip.text = element_text(size = 16))
  }
}

# Paths
models_path <- "OUTPUT/MODELS/supp/r_spec_curve/"
plots_path <- "OUTPUT/PLOTS/supp/r_spec_curve/"

# Full Model ----------
load(paste0(models_path, "tidy_models.rda"))

tidy_models <- tidy_models %>% 
  mutate(term = paste0(term, ": ", sample)) %>%
  group_by(term) %>%
  arrange(estimate) %>%
  mutate(h_order = 1:n()) %>% 
  ungroup()

# Generate and save models
for(i in c("Treatment Effect", "Democrat", "Independent", "Republican")) {
  subset_var <- paste0(i, ": Full Sample")
  
  coef_plot <- make_coef_plot(tidy_models, subset = subset_var)
  
  spec_plots <- lapply(list(
    c("Age", "Sex", "U.S. born",
      "Race/Ethnicity", "Residence", "Education", "Married", "Owns Gun", "Coop. Interview"),
    c("fixed"), c("treat_period")), make_spec_plot, data = tidy_models, subset = subset_var)
  
  combined_plot <- cowplot::plot_grid(plotlist = c(list(coef_plot), spec_plots),
                                      labels = c("", "Controls", "Fixed-Effects", "Treatment Period"),
                                      label_size = 14, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                      rel_heights = c(10, 8, 2, 2.5), align = "v", ncol = 1)
  if(i == "Treatment Effect") {
    ggsave(combined_plot, file = paste0(plots_path, "spec_treat.pdf"), 
           width = 8, height = 10)
  } else {
    ggsave(combined_plot, file = paste0(plots_path, "spec_", tolower(i), ".pdf"), 
           width = 8, height = 10)
  }
}


# Treatment effect by location (School shooting) ----------

load(paste0(models_path, "tidy_models_school.rda"))

tidy_models <- tidy_models %>% 
  mutate(term = paste0(term, ": ", sample)) %>%
  filter(school_type=="Schools") %>%
  group_by(term) %>%
  arrange(estimate) %>%
  mutate(h_order = 1:n()) %>% 
  ungroup()

# Treatment Effect
for(i in c("Treatment Effect", "Democrat", "Independent", "Republican")) {
  for(j in c("School shootings", "Non-school shootings")) {
    subset_var <- paste0(i, ": ", j)
    coef_plot <- make_coef_plot(tidy_models, subset = subset_var)
    
    spec_plots <- lapply(list(
      c("Age", "Sex", "U.S. born",
        "Race/Ethnicity", "Residence", "Education", "Married", "Owns Gun", "Coop. Interview"),
      c("fixed"), c("treat_period")), make_spec_plot, data = tidy_models, subset = subset_var)
    
    combined_plot <- cowplot::plot_grid(plotlist = c(list(coef_plot), spec_plots),
                                        labels = c("", "Controls", "Fixed-Effects", "Treatment Period"),
                                        label_size = 14, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                        rel_heights = c(10, 8, 2, 2.5), align = "v", ncol = 1)
    if(i == "Treatment Effect") {
      ggsave(combined_plot, file = paste0(plots_path, "spec_treat_", tolower(j), ".pdf"), 
             width = 8, height = 10)
    } else {
      ggsave(combined_plot, file = paste0(plots_path, "spec_", tolower(i), 
                                          "_", tolower(j), ".pdf"),
             width = 8, height = 10)
    }
  }
}






