# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(tidyverse)

# read in cleaned data
school_data = read_tsv("data/clean/school_data.tsv") %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                factor))
school_train = read_tsv("data/clean/school_train.tsv") %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                factor))

############################ Response Distribution ############################# 

# calculate mean # of crimes per 1,000 students enrolled
mean_crimes_per1000 = school_train %>%
  summarise(mean(crimes_per1000)) %>%
  pull()
mean_crimes_per1000

# create histogram of log of crimes
p = school_train %>%
  ggplot(aes(x = crimes_per1000)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = mean_crimes_per1000,
             linetype = "dashed") +
  labs(x = "Crimes Per 1000 Students", 
       y = "Number of Schools") +
  theme(text = element_text(size = 12)) +
  theme_bw()
p

# save the histogram
ggsave(filename = "results/response-hist-whole.png", 
       plot = p, 
       device = "png", 
       width = 3, 
       height = 3)

p_sqrt = school_train %>%
  ggplot(aes(x = sqrt(crimes_per1000))) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = sqrt(mean_crimes_per1000),
             linetype = "dashed") +
  labs(x = "(Sqrt) Crimes Per 1000 Students", 
       y = "Number of Schools") +
  theme(text = element_text(size = 9)) +
  theme_bw()
p_sqrt

p_tgt = plot_grid(p, p_sqrt)
ggsave(filename = "results/response-hist-both.png", 
       plot = p_tgt, 
       device = "png", 
       width = 7, 
       height = 4)


# top 10 schools by number of crimes
school_train %>%
  arrange(desc(crimes_per1000)) %>%
  head(10) %>%
  inner_join(directory_data, by = "ncessch") %>%
  select(school_name, crimes_per1000) %>%
  write_tsv("results/top-10-schools-data.tsv")

# drop maple lane school (juvenile detention facility) 
school_train <- school_train %>%
  filter(crimes_per1000 < max(crimes_per1000))

######################## Feature-Feature Relationships ########################
# install.packages("corrplot")
library(corrplot)

school_train_numvars_s = school_train %>%
  select(c("teachers_fte_crdc", "security_guard_fte", "salaries_teachers", 
           "expenditures_nonpersonnel", "avg_suspensions",
            "threats_w_weapon_incidents", "threats_w_firearm_incidents",
           "free_reduced_lunch_per1000"))


corrplot(cor(school_train_numvars_s),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "upper",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         col = NULL)       # Color palette

school_train_numvars_c = school_train %>%
  select(c("percent_no_highschool", "percent_only_highschool", 
           "percent_all_college", 
           "Unemployment_rate_2015", "white_pop",
           "black_pop", "Poverty_all_population",
           "minor_poverty", "Household_income"))


corrplot(cor(school_train_numvars_c),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "upper",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         col = NULL)       # Color palette

school_train_agg = school_train %>%
  select(c("percent_no_highschool", "percent_only_highschool", 
           "percent_all_college", 
           "Unemployment_rate_2015", "white_pop",
           "black_pop", "Poverty_all_population",
           "minor_poverty", "Household_income", 
           "teachers_fte_crdc", "security_guard_fte", "salaries_teachers", 
           "expenditures_nonpersonnel", "avg_suspensions",
           "threats_w_weapon_incidents", "threats_w_firearm_incidents",
           "free_reduced_lunch_per1000"))


corrplot(cor(school_train_agg),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "upper",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         col = NULL)       # Color palette

######################## Response-Feature Relationships ########################

## to prevent selection bias, use training set to explore response-feature

# Crimes vs. Firearm / Explosives Threats
p_firearm = school_train %>%
  select(crimes_per1000, threats_w_firearm_incidents) %>%
  ggplot(aes(x = threats_w_firearm_incidents, y = sqrt(crimes_per1000))) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Threats w Firearms/Explosives",
       y = "(Sqrt) Crimes Per 1000 Students") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# Crimes vs. Poverty Rate
p_poverty = school_train %>%
  select(crimes_per1000, Poverty_all_population) %>%
  ggplot(aes(x = Poverty_all_population, y = sqrt(crimes_per1000))) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Poverty Rate",
       y = "(Sqrt) Crimes Per 1000 Students") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# Crimes vs. Unemployment Rate
p_unemploy = school_train %>%
  select(crimes_per1000, Unemployment_rate_2015) %>%
  ggplot(aes(x = Unemployment_rate_2015, y = sqrt(crimes_per1000))) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Unemployment Rate",
       y = "(Sqrt) Crimes Per 1000 Students") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

p_pov_firearm = plot_grid(p_firearm, p_poverty, p_unemploy, nrow = 1)
ggsave(filename = "results/scatter-both.png", 
       plot = p_pov_firearm, 
       device = "png", 
       width = 8, 
       height = 3)

# Crimes vs. School Type
p_type = school_train %>%
  select(crimes_per1000, school_type) %>%
  ggplot(aes(x = school_type, y = sqrt(crimes_per1000))) +
  geom_boxplot(aes(fill = factor(school_type))) +
  labs(x = "School Type", y = "(Sqrt) Crimes Per 1000 Students") +
  scale_x_discrete(labels = c('Regular School',
                              'Special Education School',
                              'Vocational School')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(filename = "results/school-type-box.png", 
       plot = p_type, 
       device = "png", 
       width = 8, 
       height = 5)




