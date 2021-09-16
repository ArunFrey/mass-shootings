# ---
# DESCRIPTIVES
# ---

library(knitr)
library(tidyverse)
library(kableExtra)
library(vtable)

theme_set(theme_minimal(base_size = 19))

plots_path <- "OUTPUT/PLOTS/desc/"
tables_path <- "OUTPUT/TABLES/desc/"
models_path <- "OUTPUT/MODELS/desc/"


labs <- data.frame(restrict_guns = 'Supports gun permits',
                   party_cat = 'Party affiliation',
                   age = 'Age', 
                   female = 'Female', 
                   born = "U.S. born", 
                   race = "Race/Ethnicity", 
                   residence = "Residence", 
                   married = "Married", 
                   owngun = "Gun ownership", 
                   educ = "Education (years)",
                   cooperative = "Coop. interview")

# TABLES --------------------------------------------------------------------------------------

# Missingness Table ------
gss_event %>% 
  select(all_of(dv), all_of(ivs), treat_control_14) %>% 
  filter(!(is.na(treat_control_14))) %>%
  mutate_if(is.factor, as.numeric) %>%
  vtable::st(group = "treat_control_14", summ = c('countNA(x)', 'propNA(x)'), 
             summ.names = c("Freq.", "Prop."),
             digits = 3, factor.numeric = T,
             labels = labs, 
             fit.page = NA, 
             title = "Missing values",
             anchor = "missing_tab", out = "latex", file = paste0(tables_path, "missing_tab.tex"))
  
# Summary Statistics -----
gss_event %>% 
  select(all_of(dv), all_of(ivs), treat_control_14) %>% 
  na.omit() %>%
  vtable::st(group = "treat_control_14",                    
             group.test = list(star.cutoffs = c(0.001, 0.01, 0.05), 
                               format = '${pval}$ ({name})'), 
             digits = 2, fixed.digits = TRUE, 
             factor.percent = FALSE, factor.counts = FALSE, 
             fit.page = NA, 
             title = "Summary statistics",
             labels = labs, 
             anchor = "summary_tab", out = "latex", file = paste0(tables_path, "summary_tab.tex"))


# Interview dates ----
table_gss <- gss %>%
  filter(year %in% gss_event$year) %>%
  group_by(year) %>%
  summarise('Field start date' = min(date, na.rm = T), 
            'Field end date' = max(date, na.rm = T),
            'N (interviews)' = n())

table_gss$`Field start date` <- format(table_gss$`Field start date`, "%b %d, %Y")
table_gss$`Field end date` <- format(table_gss$`Field end date`, "%b %d, %Y")

# Latex Table
table_gss %>%
  rename(Year = year) %>%
  kable("latex", booktabs = T, 
        caption = paste0("General Social Survey, interview dates"), 
        label = "gss_date") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(file = paste0(tables_path, "gss_date.tex"))


# Shooting Summary Stats -----

selected_shootings <- gss_event %>%
  select(all_of(dv), all_of(ivs), treat_14, case) %>% 
  na.omit() %>%
  group_by(case, treat_14) %>%
  count() %>%
  spread(treat_14, n) %>% 
  select(case, `1`) %>%
  ungroup() %>%
  rename("Treated obs." = `1`)

selected_shootings %>% 
  left_join(shootings_overlap) %>%
  select(case, location, date_shooting, school_shooting, fatalities, `Treated obs.`) %>%
  mutate(school_shooting = ifelse(school_shooting==1, "Yes", "No")) %>%
  rename(date = date_shooting, 
         "School" = school_shooting) %>%  
  arrange(desc(date)) %>%
  mutate(date = format(date, format = "%b %d, %Y")) %>%
  rename_all(Hmisc::capitalize) %>%
  kable("latex", 
        caption = "Details on each mass shooting", 
        label = "shootings_summary",
        booktabs = T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  cat(file = paste0(tables_path, "shootings_summary.tex"))


# PLOTS ---------------------------------------------------------------------------------------

# Attitudes towards gun permits over time (by pol. affiliation) -----
g1 <- gss %>% 
  group_by(year, party_cat) %>%
  filter(!is.na(party_cat)) %>%
  summarise(restrict_guns=mean(restrict_guns, na.rm = T)) %>%
  filter(!is.na(restrict_guns)) %>%
  ggplot(aes(x = year, y = restrict_guns, color = party_cat, shape = party_cat, linetype = party_cat)) +
  geom_point(size = 2) +
  geom_line(na.rm = F) +  
  xlim(1972, 2019) +
  xlab("") +
  scale_color_manual(name = "Political Affiliation", 
                     labels = c("Democrat", "Independent", "Republican"),
                     values = c("#30a2da", "#e5ae38", "#fc4f30")) +
  scale_shape_manual(name = "Political Affiliation", 
                     labels = c("Democrat", "Independent", "Republican"),
                     values = c("circle", "square", "triangle")) +
  scale_linetype_manual(name = "Political Affiliation", 
                     labels = c("Democrat", "Independent", "Republican"),
                     values = c("solid", "dotted", "longdash")) +
  theme(legend.position = "top") + 
  scale_y_continuous(name = "% Favoring Gun Permits",
                     limits = c(0, 1),
                     breaks=seq(0, 1,0.2), 
                     labels = scales::percent, 
                     guide = guide_axis(check.overlap = TRUE)) 
  

ggsave(g1, file = paste0(plots_path, "guncontrol_by_party.pdf"), width = 9)
print(g1)


# Treatment and control periods -----
g1 <- gss_event %>%
  ggplot(aes(x = date, color = treat_control_14, fill = treat_control_14)) +
  geom_bar(width = 1) +
  facet_wrap(~year, scales = "free", nrow = 4) +
  geom_vline(xintercept = shootings_overlap$date_shooting, 
             color = "black", linetype = 2) +
  scale_color_manual(name = "Sample Groups", values = c("#314e52", "#f2a154", "#e7e6e1"), 
                    labels = c("Control", "Treated", "Excluded from sample"), na.value = "#e7e6e1") +
  scale_fill_manual(name = "Sample Groups", values = c("#314e52", "#f2a154", "#e7e6e1"), 
                    labels = c("Control", "Treated", "Excluded from sample"), na.value = "#e7e6e1") +
  labs(fill = "Sample Groups") +
  ylab("Number of GSS respondents per day") + 
  scale_x_date(labels = scales::date_format("%b"), breaks = scales::date_breaks("1 month")) +
  scale_y_continuous(guide = guide_axis(check.overlap = TRUE), 
                     breaks = seq(0, 60, 10)) +
  theme(axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 11), 
        panel.grid =  element_line(size = 0.1, colour = "grey90"),
        legend.position = "top")

ggsave(g1, file = paste0(plots_path, "sample_plot.pdf"), width = 12, height = 7.5)
print(g1)
